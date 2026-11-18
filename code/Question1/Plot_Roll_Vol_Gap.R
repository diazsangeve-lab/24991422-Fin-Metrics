# Plots a fund's rolling volatility against its benchmark with inline labels, and a strip underneath showing the gap between the two.
Plot_Roll_Vol_Gap <- function(df, Fund = "SafeCore", BM = "Capped SWIX", NSel = 252, Title = "Rolling Volatility Relative to the Benchmark", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Roll_Vol_Gap, with inputs df, Fund, BM and NSel, plus the title, subtitle and caption text

    Wide <- # build Wide through the steps below
        df %>% # start from the input data
        filter(Name %in% c(Fund, BM)) %>% # keep only the fund and its benchmark
        Roll_SD_Calc(NSel = NSel) %>% # rolling annualised volatility, using the function from Part A
        select(date, Name, RollSD) %>% # keep only what the plot needs
        spread(Name, RollSD) %>% # one column per series
        rename(Fund_SD = all_of(Fund), BM_SD = all_of(BM)) %>% # generic names so that the plot works for any fund
        filter(!is.na(Fund_SD), !is.na(BM_SD)) %>% # both series must exist on a date
        mutate(Gap = Fund_SD - BM_SD) # positive when the fund is more volatile than the benchmark

    Ends <- Wide %>% filter(date == max(date)) # the last date, where the inline labels go
    Year_Breaks <- seq(lubridate::floor_date(min(Wide$date), "year"), max(Wide$date), by = "year") # one tick per year, only where there is data

    g_top <- # build this panel of the figure in layers and store it as g_top
        Wide %>% # start from Wide, built above
        ggplot() + # start the main panel
        geom_ribbon(aes(date, ymin = BM_SD, ymax = pmax(Fund_SD, BM_SD)), fill = "firebrick", alpha = 0.35) + # red where the fund is more volatile than the benchmark
        geom_ribbon(aes(date, ymin = pmin(Fund_SD, BM_SD), ymax = BM_SD), fill = "steelblue", alpha = 0.35) + # blue where the fund is calmer than the benchmark
        geom_line(aes(date, BM_SD), color = "grey30", linewidth = 0.8) + # the benchmark's rolling volatility
        geom_line(aes(date, Fund_SD), color = "darkorange", linewidth = 0.8) + # the fund's rolling volatility
        annotate("text", x = Ends$date, y = Ends$Fund_SD, label = Fund, color = "darkorange", hjust = -0.1, vjust = -0.3, fontface = "bold", size = fmxdat::ggpts(9)) + # label the fund at the end of its line
        annotate("text", x = Ends$date, y = Ends$BM_SD, label = BM, color = "grey30", hjust = -0.1, vjust = 1.3, fontface = "bold", size = fmxdat::ggpts(9)) + # label the benchmark at the end of its line
        scale_x_date(breaks = Year_Breaks, expand = expansion(mult = c(0.01, 0.16))) + # yearly grid lines and room on the right for the inline labels
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # percentage axis
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22)) + # the clean course theme
        theme(axis.text.x = element_blank()) + # the dates are shown on the strip underneath
        labs(x = "", y = "Rolling annualised volatility") # axis names

    g_gap <- # build this panel of the figure in layers and store it as g_gap
        Wide %>% # start from Wide, built above
        ggplot() + # start the gap strip
        geom_col(aes(date, Gap, fill = Gap > 0), width = 1) + # one thin bar per day, coloured by the sign of the gap
        geom_hline(yintercept = 0, color = "grey30") + # zero means the two are equally volatile
        scale_fill_manual(values = c(`TRUE` = "firebrick", `FALSE` = "steelblue"), guide = "none") + # red when the fund runs more risk and blue when it runs less
        scale_x_date(breaks = Year_Breaks, date_labels = "%Y", expand = expansion(mult = c(0.01, 0.16))) + # yearly date labels, aligned with the main panel
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.5)) + # percentage axis
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22)) + # the clean course theme
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # vertical date labels
        labs(x = "", y = "Gap") # axis names

    patchwork::wrap_plots(g_top, g_gap, ncol = 1, heights = c(3, 1)) + # stack the main panel above the gap strip
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, # one title for the combined figure
                                   theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # the course theme for the shared title

}
