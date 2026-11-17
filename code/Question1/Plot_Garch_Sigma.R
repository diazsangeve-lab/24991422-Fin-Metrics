# Plots the GARCH conditional volatility of several series on one set of axes, so that their levels can be compared directly.
Plot_Garch_Sigma <- function(df, Highlight_From = lubridate::ymd(20260101), Smooth_N = 21, Title = "Conditional Volatility from GARCH", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !"Sigma" %in% names(df)) stop("... for Plot_Garch_Sigma to work, first run Best_Garch_Sigma to create the 'Sigma' column") # stop early if the model output is missing

    g <-
        df %>%
        mutate(Label = glue::glue("{Name} ({Model})")) %>% # show which model was chosen for each series in the legend
        arrange(date) %>% # order by date before smoothing
        group_by(Label) %>% # smooth each series on its own
        mutate(Sigma = RcppRoll::roll_mean(Sigma, n = Smooth_N, fill = NA, align = "right")) %>% # a one month moving average, purely so that the three lines are readable on one plot
        ungroup() %>% # remove the grouping again
        filter(!is.na(Sigma)) %>% # drop the first few days, where the average is not yet full
        ggplot() + # start the plot
        annotate("rect", xmin = Highlight_From, xmax = max(df$date), ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "steelblue") + # shade the most recent period that the question is about
        geom_line(aes(date, Sigma, color = Label), alpha = 0.8, linewidth = 0.8) + # the noise reduced volatility line of each series
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_cols() + # the crisp course colours
        labs(x = "", y = "Annualised Conditional Volatility", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1) # finishing touches, with yearly date labels and a percentage axis

}
