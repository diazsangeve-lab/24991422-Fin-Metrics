# Plots a fund's active factor exposures as small multiples, each filling the focal factor's tilt and showing the other factors as grey lines behind it.
Plot_Active_Exposure <- function(df, Fund = "SafeCore", BM = "Capped SWIX", Title = "Active Factor Exposures", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Active_Exposure, with inputs df, Fund and BM, plus the title, subtitle and caption text

    Active <- # build Active through the steps below
        df %>% # start from the input data
        filter(Portfolio %in% c(Fund, BM)) %>% # keep only the fund and its benchmark
        select(date, Portfolio, Factor, Score) %>% # keep only what the plot needs
        spread(Portfolio, Score) %>% # one column per portfolio
        mutate(Active = .data[[Fund]] - .data[[BM]]) %>% # the fund's exposure in excess of the benchmark's
        filter(!is.na(Active)) # both portfolios must have a score on the date

    Background <- Active %>% rename(Group = Factor) # a copy without the facet variable, so that every factor appears in every panel

    g <- # build the plot in layers and store it as g
        Active %>% # start from Active, built above
        ggplot() + # start the plot
        geom_line(data = Background, aes(date, Active, group = Group), color = "grey85", linewidth = 0.35) + # every other factor in light grey, as context
        geom_ribbon(aes(date, ymin = 0, ymax = pmax(Active, 0)), fill = "darkorange", alpha = 0.75) + # the overweight part of the focal factor
        geom_ribbon(aes(date, ymin = pmin(Active, 0), ymax = 0), fill = "steelblue", alpha = 0.75) + # the underweight part of the focal factor
        geom_line(aes(date, Active), color = "grey20", linewidth = 0.4) + # the outline of the focal factor
        geom_hline(yintercept = 0, color = "grey20") + # zero means the same exposure as the benchmark
        facet_wrap(~Factor, ncol = 2) + # one panel per factor
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Active z-score relative to benchmark", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T) # finishing touches, with yearly date labels

}
