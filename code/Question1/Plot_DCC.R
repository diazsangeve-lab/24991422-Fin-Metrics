# Plots monthly DCC correlations as lollipops measured from each pair's long run average, so that periods of tighter and looser co-movement stand out.
Plot_DCC <- function(df, Highlight_From = lubridate::ymd(20260101), Title = "Dynamic Conditional Correlations", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !"Rho" %in% names(df)) stop("... for Plot_DCC to work, first run DCC_Cor_Calc to create the 'Rho' column") # stop early if the correlations are missing

    Monthly <-
        df %>%
        mutate(Month = lubridate::floor_date(date, unit = "month")) %>% # label every week with its month
        group_by(Pair, Month) %>% # one number per pair per month
        summarise(Rho = mean(Rho), .groups = "drop") %>% # the average weekly correlation in the month
        group_by(Pair) %>% # work out each pair's own average
        mutate(Avg = mean(Rho), Dev = Rho - Avg) %>% # the long run average and each month's deviation from it
        ungroup() %>% # remove the grouping again
        mutate(Strip = glue::glue("{Pair} (average {sprintf('%.2f', Avg)})")) # panel titles that carry the average level

    Notes <-
        bind_rows(Monthly %>% group_by(Strip) %>% slice_max(Dev, n = 1, with_ties = FALSE), # the month with the tightest link in each panel
                  Monthly %>% filter(Month >= Highlight_From) %>% group_by(Strip) %>% slice_min(Dev, n = 1, with_ties = FALSE)) %>% # the loosest month in the highlighted period
        ungroup() %>% # remove the grouping again
        mutate(Label = glue::glue("{format(Month, '%b %Y')}: {sprintf('%.2f', Rho)}")) # the month and the correlation level

    g <-
        Monthly %>%
        ggplot() + # start the plot
        annotate("rect", xmin = Highlight_From, xmax = max(Monthly$Month) + 31, ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.12) + # shade the recent period that the question is about
        geom_hline(yintercept = 0, color = "grey40") + # zero marks the pair's long run average
        geom_segment(aes(x = Month, xend = Month, y = 0, yend = Dev, color = Dev > 0), linewidth = 0.6) + # the stick of each lollipop, from the average to the month's value
        geom_point(aes(Month, Dev, color = Dev > 0), size = 1.3) + # the head of each lollipop
        geom_label(data = Notes, aes(Month, Dev, label = Label), size = fmxdat::ggpts(8), label.size = 0, fill = "white", alpha = 0.85, vjust = ifelse(Notes$Dev > 0, -0.3, 1.3)) + # label the extreme months
        facet_wrap(~Strip, ncol = 1) + # one panel per pair
        scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "steelblue")) + # red when the link is tighter than usual and blue when it is looser
        scale_y_continuous(expand = expansion(mult = 0.25)) + # leave room for the labels
        guides(color = "none") + # the colours are explained in the subtitle
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Correlation relative to its average", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T) # finishing touches, with yearly date labels

}
