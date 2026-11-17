# Plots the autocorrelation of returns next to that of squared returns for each series, with the 95 percent band, so that volatility clustering shows up as bars that stay outside the band.
Plot_Acf_Clustering <- function(Acf_df, Title = "Volatility Clustering", Subtitle = "", Caption = "Note:\nCalculation own"){

    Lab <- Acf_df %>% distinct(Name, Type, LB_Stat, LB_p) %>% mutate(Text = glue::glue("Ljung-Box Q(10) = {round(LB_Stat)}{ifelse(LB_p < 0.001, ', p < 0.001', paste0(', p = ', sprintf('%.3f', LB_p)))}")) # the test result for each panel

    ggplot(Acf_df) + # start the plot
        geom_hline(yintercept = 0, color = "grey40") + # zero autocorrelation
        geom_ribbon(aes(Lag, ymin = -Bound, ymax = Bound), fill = "steelblue", alpha = 0.15) + # the band within which autocorrelation is not significant
        geom_segment(aes(Lag, 0, xend = Lag, yend = ACF, color = abs(ACF) > Bound), linewidth = 1.1) + # a bar for each lag
        geom_point(aes(Lag, ACF, color = abs(ACF) > Bound), size = 1.8) + # the tip of each bar
        geom_label(data = Lab, aes(Inf, Inf, label = Text), hjust = 1.02, vjust = 1.2, size = fmxdat::ggpts(7.5), label.size = 0, fill = "white", color = "grey15") + # the Ljung-Box test in the corner of each panel
        facet_grid(Name ~ Type) + # rows for the series and columns for returns and squared returns
        scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey55"), labels = c(`TRUE` = "Outside the 95% band", `FALSE` = "Inside the band")) + # red for significant lags
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "bottom") + # the legend below the panels
        labs(x = "Lag (trading days)", y = "Autocorrelation", color = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
