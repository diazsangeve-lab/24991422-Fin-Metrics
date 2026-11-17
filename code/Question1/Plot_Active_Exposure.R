# Plots a fund's factor exposure minus its benchmark's through time, so that persistent tilts and their changes are visible.
Plot_Active_Exposure <- function(df, Fund = "SafeCore", BM = "Capped SWIX", Title = "Active Factor Exposures", Subtitle = "", Caption = "Note:\nCalculation own"){

    Active <-
        df %>%
        filter(Portfolio %in% c(Fund, BM)) %>% # keep only the fund and its benchmark
        select(date, Portfolio, Factor, Score) %>% # keep only what the plot needs
        spread(Portfolio, Score) %>% # one column per portfolio
        mutate(Active = .data[[Fund]] - .data[[BM]]) %>% # the fund's exposure in excess of the benchmark's
        filter(!is.na(Active)) # both portfolios must have a score on the date

    g <-
        Active %>%
        ggplot() + # start the plot
        geom_hline(yintercept = 0, linetype = "dashed") + # zero means the fund has the same exposure as the benchmark
        geom_col(aes(date, Active, fill = Active > 0), width = 25, alpha = 0.85) + # one bar per month, coloured by the direction of the tilt
        facet_wrap(~Factor, ncol = 2) + # one panel per factor
        scale_fill_manual(values = c(`TRUE` = "darkorange", `FALSE` = "steelblue")) + # orange for an overweight tilt and blue for an underweight tilt
        guides(fill = "none") + # the colours are explained in the subtitle
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Active z-score relative to benchmark", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T) # finishing touches, with yearly date labels

}
