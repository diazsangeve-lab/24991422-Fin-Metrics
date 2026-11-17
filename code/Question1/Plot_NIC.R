# Plots news impact curves for several series on one set of axes, so that their asymmetry can be compared.
Plot_NIC <- function(df, Title = "News Impact Curves", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("Shock", "Vol") %in% names(df)) ) stop("... for Plot_NIC to work, first run Garch_NIC_Calc to create the 'Shock' and 'Vol' columns") # stop early if the curves are missing

    g <-
        df %>%
        ggplot() + # start the plot
        geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) + # a line at zero to separate bad news on the left from good news on the right
        geom_line(aes(Shock, Vol, color = Name), linewidth = 1.2, alpha = 0.85) + # the volatility response of each series
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_cols() + # the crisp course colours
        labs(x = "Size of yesterday's unexpected daily return", y = "Implied annualised volatility today", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    fmxdat::finplot(g, x.pct = T, x.pct_acc = 1, y.pct = T, y.pct_acc = 1) # finishing touches, with percentage axes

}
