# Plots weighted and unweighted cross-sectional dispersion through time, with daily values faded behind their smoothed paths and key events labelled.
Plot_Dispersion <- function(df, Events = NULL, Smooth_N = 63, Title = "Cross-Sectional Return Dispersion", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Dispersion, with inputs df, Events and Smooth_N, plus the title, subtitle and caption text

    if( !all(c("date", "Dispersion_W", "Dispersion_U") %in% names(df)) ) stop("... for Plot_Dispersion to work, first run Dispersion_Calc") # stop early if the dispersion series is missing

    Long <- # build Long through the steps below
        df %>% # start from the input data
        select(date, Weighted = Dispersion_W, Unweighted = Dispersion_U) %>% # the two measures
        gather(Measure, Value, -date) %>% # long format, one row per day and measure
        arrange(date) %>% # order by date before smoothing
        group_by(Measure) %>% # smooth each measure on its own
        mutate(Smooth = RcppRoll::roll_mean(Value, n = Smooth_N, fill = NA, align = "right")) %>% # a rolling average that shows the underlying level
        ungroup() # remove the grouping again

    Ends <- Long %>% filter(!is.na(Smooth)) %>% group_by(Measure) %>% filter(date == max(date)) %>% ungroup() # the last point of each smoothed line, where its label goes

    g <- # build the plot in layers and store it as g
        Long %>% # start from Long, built above
        ggplot() + # start the plot
        geom_line(aes(date, Value, color = Measure), alpha = 0.25, linewidth = 0.3) + # the noisy daily values, faded into the background
        geom_line(aes(date, Smooth, color = Measure), linewidth = 1.3) + # the smoothed level of each measure
        ggrepel::geom_text_repel(data = Ends, aes(date, Smooth, label = Measure, color = Measure), hjust = 0, nudge_x = 25, direction = "y", fontface = "bold", size = fmxdat::ggpts(9), segment.color = NA) + # label each smoothed line at its end, pushed apart vertically so the labels never overlap
        scale_color_manual(values = c(Weighted = "darkorange", Unweighted = "grey35")) + # orange for the headline weighted measure and grey for the comparison
        scale_x_date(expand = expansion(mult = c(0.01, 0.13)), date_breaks = "1 year", date_labels = "%Y") + # yearly labels and room on the right for the line labels
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # dispersion as a percentage
        guides(color = "none") + # the lines are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Dispersion of one month stock returns", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    if( !is.null(Events) ){ # only mark events when a table of events is supplied
        Peaks <- Events %>% rowwise() %>% mutate(y = max(df$Dispersion_W[abs(as.numeric(df$date - date)) <= 10])) %>% ungroup() # the height of the weighted series around each event, so that the label sits on its spike
        g <- g + ggrepel::geom_label_repel(data = Peaks, aes(date, y, label = Label), size = fmxdat::ggpts(8), nudge_y = 0.02, label.size = 0, fill = "white", alpha = 0.9, segment.color = "grey50") # label each event next to its spike
    }

    g # return the finished plot

}
