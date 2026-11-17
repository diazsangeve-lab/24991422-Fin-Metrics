# Plots monthly realised volatility as a long time series heatmap, one row per series, with key events marked above the tiles.
Plot_Vol_Heatmap <- function(df, Events = NULL, Cap = 0.4, Title = "Monthly Volatility Heatmap", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("Month_Date", "Vol") %in% names(df)) ) stop("... for Plot_Vol_Heatmap to work, first run Monthly_Vol_Calc") # stop early if the monthly numbers are missing

    N_Rows <- length(unique(df$Name)) # the number of series, used to place the event labels above the top row

    g <-
        df %>%
        mutate(Vol_Capped = pmin(Vol, Cap)) %>% # cap the colour scale so that the Covid months do not wash out every other month
        ggplot() + # start the plot
        geom_tile(aes(Month_Date, Name, fill = Vol_Capped), width = 31, height = 0.92) + # one tile per series per month
        scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = scales::percent_format(accuracy = 1), limits = c(0, Cap)) + # pale for calm months and dark red for volatile ones
        scale_y_discrete(limits = rev) + # the first series at the top
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(panel.grid = element_blank(), legend.key.width = unit(1.5, "cm")) + # no grid lines behind the tiles and a wider legend
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption, fill = "Annualised\nvolatility") # axis names, title and note

    if( !is.null(Events) ){
        g <- g +
            geom_vline(data = Events, aes(xintercept = date), linetype = "dashed", linewidth = 0.4, color = "grey20") + # a dashed line through the tiles at each event
            geom_label(data = Events, aes(date, N_Rows + 0.9, label = Label), size = fmxdat::ggpts(8), label.size = 0, fill = "white", vjust = 0) + # the event name above the top row
            coord_cartesian(clip = "off") # allow the labels to sit above the panel
    }

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T) # finishing touches, with yearly date labels

}
