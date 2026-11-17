# Plots time-varying DCC correlations, with an optional shaded period and each pair's long run average.
Plot_DCC <- function(df, Highlight_From = lubridate::ymd(20260101), Title = "Dynamic Conditional Correlations", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !"Rho" %in% names(df)) stop("... for Plot_DCC to work, first run DCC_Cor_Calc to create the 'Rho' column") # stop early if the correlations are missing

    g <-
        df %>%
        group_by(Pair) %>% # work out each pair's own average
        mutate(Avg_Rho = mean(Rho, na.rm = T)) %>% # the full sample average correlation of the pair
        ungroup() %>% # remove the grouping again
        ggplot() + # start the plot
        annotate("rect", xmin = Highlight_From, xmax = max(df$date), ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "steelblue") + # shade the most recent period that the question is about
        geom_line(aes(date, Rho, color = Pair), linewidth = 0.9, alpha = 0.85) + # the weekly correlation of each pair
        geom_line(aes(date, Avg_Rho, color = Pair), linetype = "dashed", alpha = 0.7) + # each pair's long run average, to judge whether today is high or low
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_cols() + # the crisp course colours
        labs(x = "", y = "Conditional correlation", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T) # finishing touches, with yearly date labels

}
