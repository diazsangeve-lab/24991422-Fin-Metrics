# Plots the rolling volatility of a fund against its benchmark, shading the gap red where the fund is riskier and blue where it is calmer.
Plot_Roll_Vol_Gap <- function(df, Fund = "SafeCore", BM = "Capped SWIX", NSel = 252, Title = "Rolling Volatility Relative to the Benchmark", Subtitle = "", Caption = "Note:\nCalculation own"){

    Wide <-
        df %>%
        filter(Name %in% c(Fund, BM)) %>% # keep only the fund and its benchmark
        Roll_SD_Calc(NSel = NSel) %>% # rolling annualised volatility, using the function from Part A
        select(date, Name, RollSD) %>% # keep only what the plot needs
        spread(Name, RollSD) %>% # one column per series
        rename(Fund_SD = all_of(Fund), BM_SD = all_of(BM)) %>% # generic names so that the plot works for any fund
        filter(!is.na(Fund_SD), !is.na(BM_SD)) # both series must exist on a date

    g <-
        Wide %>%
        ggplot() + # start the plot
        geom_ribbon(aes(date, ymin = BM_SD, ymax = pmax(Fund_SD, BM_SD)), fill = "firebrick", alpha = 0.35) + # red where the fund is more volatile than the benchmark
        geom_ribbon(aes(date, ymin = pmin(Fund_SD, BM_SD), ymax = BM_SD), fill = "steelblue", alpha = 0.35) + # blue where the fund is calmer than the benchmark
        geom_line(aes(date, BM_SD, color = BM), linewidth = 0.9) + # the benchmark's rolling volatility
        geom_line(aes(date, Fund_SD, color = Fund), linewidth = 0.9) + # the fund's rolling volatility
        scale_color_manual(values = purrr::set_names(c("grey30", "darkorange"), c(BM, Fund))) + # a neutral colour for the benchmark and a bright one for the fund
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Rolling annualised volatility", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1) # finishing touches, with yearly date labels and a percentage axis

}
