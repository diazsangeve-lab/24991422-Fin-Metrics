# Plots trailing market dispersion against the spread of active manager results as a connected scatter through time, coloured by the share of managers beating the benchmark.
Plot_Alpha_Opportunity <- function(df, Title = "Alpha Opportunity and Active Manager Outcomes", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Alpha_Opportunity, with input df, plus the title, subtitle and caption text

    if( !all(c("date", "Disp12", "Mgr_Spread", "Share_Outperform") %in% names(df)) ) stop("... for Plot_Alpha_Opportunity to work, provide 'date', 'Disp12', 'Mgr_Spread' and 'Share_Outperform'") # stop early if a column is missing

    D <- df %>% filter(!is.na(Disp12), !is.na(Mgr_Spread)) %>% arrange(date) # complete months in time order, so that the path is drawn correctly
    Ends <- bind_rows(D %>% slice_head(n = 1), D %>% slice_tail(n = 1)) # the first and last months, which get labels

    g <- # build the plot in layers and store it as g
        D %>% # start from D, built above
        ggplot(aes(Disp12, Mgr_Spread)) + # market dispersion along the bottom and manager spread up the side
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "grey40", fill = "grey85", linewidth = 0.6, linetype = "dashed") + # the fitted relationship with its confidence band
        geom_path(color = "grey70", linewidth = 0.4) + # connect the months in time order
        geom_point(aes(fill = Share_Outperform), shape = 21, size = 3.2, color = "white", stroke = 0.4) + # one point per month, coloured by how many managers were ahead of the benchmark
        geom_label(data = Ends, aes(label = format(date, "%b %Y")), size = fmxdat::ggpts(8), label.size = 0, fill = "white", alpha = 0.85, vjust = -0.6) + # label where the path starts and ends
        scale_fill_gradient2(low = "firebrick", mid = "grey92", high = "steelblue", midpoint = 0.5, labels = scales::percent_format(accuracy = 1)) + # red when most managers lag the benchmark and blue when most lead it
        scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) + # dispersion as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # manager spread as a percentage
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.key.width = unit(1.5, "cm")) + # a wider legend
        labs(x = "Trailing 12 month average weighted dispersion", y = "Spread of 12 month manager excess returns", title = Title, subtitle = Subtitle, caption = Caption, fill = "Share of managers\nbeating the benchmark") # axis names, title and note

    g # return the finished plot

}
