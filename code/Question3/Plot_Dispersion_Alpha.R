# Plots trailing market dispersion against the client fund's and the typical peer's trailing alpha, alongside the average alpha in low, middle and high dispersion windows.
Plot_Dispersion_Alpha <- function(DA, Fund_Label = "ABC Fund", Title = "Market Dispersion and Risk-Adjusted Outperformance", Subtitle = "", Caption = "Note:\nCalculation own"){

    Long <- DA %>% select(date, Disp_Roll, Tercile, `ABC Fund` = Fund_Alpha, `Median peer` = Peer_Alpha) %>% gather(Who, Alpha, `ABC Fund`, `Median peer`) # long format, one row per window and fund
    Who_Cols <- c(`ABC Fund` = "darkorange", `Median peer` = "steelblue") # one colour each

    g1 <-
        Long %>%
        ggplot(aes(Disp_Roll, Alpha, color = Who)) + # dispersion along the bottom and alpha up the side
        geom_hline(yintercept = 0, color = "grey40") + # zero alpha
        geom_point(alpha = 0.6, size = 2.6) + # one point per 12 month window
        geom_smooth(method = "lm", se = FALSE, linewidth = 1.4, formula = y ~ x) + # the straight line fit for each
        scale_color_manual(values = Who_Cols) + # the fund colours
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + # dispersion as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # alpha as a percentage
        Slide_Theme() + # the slide theme
        theme(legend.position = "top") + # the legend above the points
        labs(x = "Average monthly dispersion over the past 12 months", y = "Alpha over the past 12 months", color = "", title = "Each point is a 12 month window") # axis names and panel title

    Summ <- Long %>% group_by(Tercile, Who) %>% summarise(Alpha = mean(Alpha), .groups = "drop") # the average alpha in each third of windows
    Hit <- DA %>% group_by(Tercile) %>% summarise(Hit = mean(Fund_Alpha > Peer_Alpha), .groups = "drop") # how often the fund beat the typical peer in each third

    g2 <-
        ggplot() + # start the bar panel
        geom_col(data = Summ, aes(Tercile, Alpha, fill = Who), position = position_dodge(width = 0.75), width = 0.7) + # the fund and the typical peer side by side
        geom_text(data = Summ, aes(Tercile, Alpha, label = scales::percent(Alpha, accuracy = 0.1), group = Who), position = position_dodge(width = 0.75), vjust = -0.4, size = fmxdat::ggpts(9), fontface = "bold") + # print each average
        geom_text(data = Hit, aes(Tercile, max(Summ$Alpha) * 1.28, label = glue::glue("ABC ahead in\n{scales::percent(Hit, accuracy = 1)} of windows")), size = fmxdat::ggpts(8.5), color = "grey25", lineheight = 0.9) + # how often the fund beat the typical peer
        scale_fill_manual(values = Who_Cols) + # the fund colours
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) + # alpha as a percentage
        Slide_Theme() + # the slide theme
        theme(legend.position = "none") + # the left panel carries the legend
        labs(x = "", y = "Average 12 month alpha", title = "Average by dispersion regime") # axis names and panel title

    patchwork::wrap_plots(g1, g2, nrow = 1, widths = c(1.25, 1)) + # the two panels side by side
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(34), subtitle.size = fmxdat::ggpts(28), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # one title for the figure

}
