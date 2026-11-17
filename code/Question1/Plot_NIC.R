# Plots news impact curves with inline labels, a shaded bad news half and the response to a chosen shock size marked on each curve.
Plot_NIC <- function(df, Mark_Shock = 0.04, Title = "News Impact Curves", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("Shock", "Vol") %in% names(df)) ) stop("... for Plot_NIC to work, first run Garch_NIC_Calc to create the 'Shock' and 'Vol' columns") # stop early if the curves are missing

    Marks <-
        df %>%
        group_by(Name) %>% # one pair of points per curve
        summarise(Down = Vol[which.min(abs(Shock + Mark_Shock))], Up = Vol[which.min(abs(Shock - Mark_Shock))], .groups = "drop") %>% # the volatility after a fall and after a rise of the chosen size
        gather(Side, Vol, -Name) %>% # long format, one row per point
        mutate(Shock = ifelse(Side == "Down", -Mark_Shock, Mark_Shock)) # the shock at which each point sits

    Ends <- df %>% group_by(Name) %>% filter(Shock == max(Shock)) %>% ungroup() # the right hand end of each curve, where its label goes

    g <-
        df %>%
        ggplot() + # start the plot
        annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "firebrick", alpha = 0.06) + # shade the bad news half
        annotate("text", x = min(df$Shock), y = min(df$Vol), label = "Bad news", hjust = 0, vjust = 0, fontface = "bold", color = "firebrick", size = fmxdat::ggpts(10)) + # name the bad news half, in the empty bottom left corner
        annotate("text", x = max(df$Shock), y = max(df$Vol), label = "Good news", hjust = 1, vjust = 1, fontface = "bold", color = "grey40", size = fmxdat::ggpts(10)) + # name the good news half
        geom_vline(xintercept = c(-Mark_Shock, Mark_Shock), linetype = "dotted", color = "grey50") + # dotted guides at the marked shock sizes
        geom_line(aes(Shock, Vol, color = Name), linewidth = 1.2) + # the volatility response of each series
        geom_point(data = Marks, aes(Shock, Vol, color = Name), size = 2.5) + # mark the response to the chosen shock size
        ggrepel::geom_label_repel(data = Marks, aes(Shock, Vol, label = scales::percent(Vol, accuracy = 0.1)), color = "grey15", fill = "white", label.size = 0, label.padding = 0.12, size = fmxdat::ggpts(8), fontface = "bold", direction = "y", nudge_x = 0.006, min.segment.length = 0, seed = 1) + # print the response next to each point, without overlaps
        geom_text(data = Ends, aes(Shock, Vol, label = Name, color = Name), hjust = 0, nudge_x = 0.002, size = fmxdat::ggpts(9), fontface = "bold") + # label each curve at its end instead of using a legend
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(-0.06, 0.06, 0.02), expand = expansion(mult = c(0.02, 0.45))) + # percentage axis with ticks only where there is data and room on the right for the labels
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # percentage axis
        guides(color = "none") + # the curves are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        scale_color_manual(values = c("#1F3A6B", "#B5451B", "#1E7B3A")) + # dark, distinct colours that stay legible as text on white
        labs(x = "Size of yesterday's unexpected daily return", y = "Implied annualised volatility today", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g) # finishing touches

}
