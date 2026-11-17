# Plots the client fund's rolling returns inside the peer group's percentile bands, with a strip below showing the quartile it finished in each calendar year.
Plot_Peer_Fan <- function(Pct, Cal, Fund_Label = "ABC Fund", Title = "ABC Fund Against the Peer Group Over Time", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <- Pct %>% filter(!is.na(Fund_Ret)) # the months in which the fund has a full window
    Lims <- c(lubridate::floor_date(min(D$date), "year"), max(D$date) + 420) # the date range shared by both panels, with room on the right for labels
    Ends <- D %>% slice_tail(n = 1) # the last month, where the labels go
    Breaks <- seq(Lims[1], max(D$date), by = "1 year") # yearly labels that stop at the last month
    Quart_Cols <- c("Top quartile" = "darkorange", "Second quartile" = "#F6C38B", "Third quartile" = "grey80", "Bottom quartile" = "grey60") # one colour per quartile

    g1 <-
        ggplot(D) + # start the top panel
        geom_ribbon(aes(date, ymin = P05, ymax = P95), fill = "steelblue", alpha = 0.15) + # the middle 90 percent of peers
        geom_ribbon(aes(date, ymin = P25, ymax = P75), fill = "steelblue", alpha = 0.3) + # the middle half of peers
        geom_line(aes(date, P50), color = "steelblue4", linewidth = 0.8) + # the median peer
        geom_line(data = D %>% filter(!is.na(BM_Ret)), aes(date, BM_Ret), color = "black", linetype = "dashed", linewidth = 0.8) + # the benchmark
        geom_line(aes(date, Fund_Ret), color = "darkorange", linewidth = 1.6) + # the client fund
        ggrepel::geom_text_repel(data = Ends %>% select(date, `ABC Fund` = Fund_Ret, `Median peer` = P50, `Capped SWIX` = BM_Ret) %>% gather(Who, y, -date), aes(date, y, label = Who, color = Who), hjust = 0, nudge_x = 40, direction = "y", fontface = "bold", size = fmxdat::ggpts(9.5), segment.color = NA, min.segment.length = Inf) + # label the three lines at the right, pushed apart so they never overlap
        scale_color_manual(values = c(`ABC Fund` = "darkorange3", `Median peer` = "steelblue4", `Capped SWIX` = "black"), guide = "none") + # label colours matching the lines
        scale_x_date(limits = Lims, breaks = Breaks, date_labels = "%Y") + # yearly labels
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # returns as percentages
        Slide_Theme() + # the slide theme
        theme(axis.text.x = element_blank()) + # the strip below carries the years
        labs(x = "", y = "Rolling 3 year return (ann.)", title = Title, subtitle = Subtitle) # axis names and title

    g2 <-
        Cal %>%
        mutate(Mid = as.Date(paste0(Year, "-07-01")), # the middle of each year, to line up with the top panel
               Lab = paste0(round(Pct * 100), ifelse(YTD, "*", ""))) %>% # the share of peers beaten, with a star for the incomplete year
        filter(Mid >= Lims[1]) %>% # only years inside the top panel's range
        ggplot() + # start the strip
        geom_tile(aes(Mid, 1, fill = Quartile), width = 330, height = 0.9, color = "white") + # one tile per calendar year
        geom_text(aes(Mid, 1, label = Lab), fontface = "bold", size = fmxdat::ggpts(10)) + # the share of peers beaten
        scale_fill_manual(values = Quart_Cols, breaks = names(Quart_Cols)) + # the quartile colours
        scale_x_date(limits = Lims, breaks = Breaks, date_labels = "%Y") + # the same dates as the top panel
        Slide_Theme() + # the slide theme
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank(), legend.position = "bottom") + # a clean strip
        labs(x = "", y = "", fill = "", caption = Caption) # the legend and note

    patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(3.2, 1)) # the fan above the calendar strip

}
