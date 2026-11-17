# Plots the average pairwise and cap-weighted implied correlation of the largest constituents, with the gap between them shaded and the peak labelled.
Plot_Avg_Corr <- function(df, Highlight_From = lubridate::ymd(20260101), Title = "Inter-Constituent Correlation", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("date", "Avg_Corr", "Implied_Corr") %in% names(df)) ) stop("... for Plot_Avg_Corr to work, first run Avg_Corr_Calc") # stop early if the correlation series is missing

    Peak <- df %>% slice_max(Implied_Corr, n = 1, with_ties = FALSE) # the month with the highest cap-weighted correlation
    Ends <- df %>% filter(date == max(date)) # the last month, where the line labels go

    g <-
        df %>%
        ggplot() + # start the plot
        annotate("rect", xmin = Highlight_From, xmax = max(df$date), ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.12) + # shade the recent period that the question is about
        geom_ribbon(aes(date, ymin = pmin(Avg_Corr, Implied_Corr), ymax = Implied_Corr), fill = "firebrick", alpha = 0.25) + # red where the large stocks move together more than the average pair
        geom_ribbon(aes(date, ymin = Implied_Corr, ymax = pmax(Avg_Corr, Implied_Corr)), fill = "grey60", alpha = 0.25) + # grey where the large stocks move together less than the average pair
        geom_line(aes(date, Avg_Corr), color = "grey35", linewidth = 0.9) + # the simple average of pairwise correlations
        geom_line(aes(date, Implied_Corr), color = "firebrick", linewidth = 1.2) + # the cap-weighted implied correlation
        geom_point(data = Peak, aes(date, Implied_Corr), color = "firebrick", size = 3) + # mark the peak
        geom_label(data = Peak, aes(date, Implied_Corr, label = glue::glue("{format(date, '%b %Y')}: {sprintf('%.2f', Implied_Corr)}")), vjust = -0.5, size = fmxdat::ggpts(8), label.size = 0, fill = "white") + # label the peak
        annotate("text", x = Ends$date, y = Ends$Implied_Corr, label = "Cap-weighted\nimplied", color = "firebrick", hjust = -0.1, fontface = "bold", size = fmxdat::ggpts(8.5), lineheight = 0.85) + # label the implied correlation at its end
        annotate("text", x = Ends$date, y = Ends$Avg_Corr, label = "Average\npairwise", color = "grey35", hjust = -0.1, fontface = "bold", size = fmxdat::ggpts(8.5), lineheight = 0.85) + # label the pairwise average at its end
        scale_x_date(expand = expansion(mult = c(0.01, 0.14)), date_breaks = "1 year", date_labels = "%Y") + # yearly labels and room on the right for the line labels
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + # room above the peak label
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Correlation of daily returns", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g

}
