# Plots rolling annualised volatility per series, with each series' own long run average as a dashed reference line.
Plot_Roll_Vol <- function(df, Title = "Rolling Realised Volatility", Subtitle = "", Caption = "Note:\nCalculation own", Facet = TRUE){

    if( !"RollSD" %in% names(df)) stop("... for Plot_Roll_Vol to work, first run Roll_SD_Calc to create the 'RollSD' column") # stop early if the rolling numbers are missing

    g <-
        df %>%
        ggplot() + # start the plot
        geom_line(aes(date, RollSD, color = Name), alpha = 0.8, linewidth = 0.9) + # the rolling volatility line of each series
        geom_line(aes(date, Avg_SD, color = Name), linetype = "dashed", alpha = 0.7) + # each series' own long run average, to judge whether today is high or low
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_cols() + # the crisp course colours
        labs(x = "", y = "Annualised Volatility", title = Title, subtitle = Subtitle, caption = Caption) + # axis names, title and note
        guides(color = "none") # the facet strips already name each series, so the legend is dropped

    if(Facet) g <- g + facet_wrap(~Name, ncol = 1) # give each series its own panel so that the lines do not hide each other

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1) # finishing touches, with yearly date labels and a percentage axis

}
