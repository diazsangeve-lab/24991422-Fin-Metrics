# Plots the fund's market capture asymmetry quadrant alongside its key return and risk measures relative to the benchmark, as two panels in one figure.
Plot_Fund_Capture <- function(Stats_All, BM_Name = "Capped SWIX", Fund_Name = "SafeCore", Early = "2015-2026", Late = "2020-2026", # define Plot_Fund_Capture, with inputs Stats_All, BM_Name, Fund_Name, Early and Late
                              Title = "Return and Risk Profile of SafeCore Against the Capped SWIX", Subtitle = "", Caption = "Note:\nCalculation own"){ # the default title, subtitle and caption text

    # For stepping through uncomment:
    # BM_Name <- "Capped SWIX" ; Fund_Name <- "SafeCore" ; Early <- "2015-2026" ; Late <- "2020-2026"

    if( !all(c("Name", "Period", "Up Capture", "Down Capture") %in% names(Stats_All)) ) stop("... for Plot_Fund_Capture to work, first run Fund_Stats_Table and add a 'Period' column") # stop early if the statistics are not in the expected shape

    # Panel A, market capture asymmetry

    Cap_Data <- Stats_All %>% filter(Name == Fund_Name) %>% select(Period, Up = `Up Capture`, Down = `Down Capture`) # the fund's up and down capture in each period
    Cap_Early <- Cap_Data %>% filter(Period == Early) # the earlier period, where the arrow starts
    Cap_Late <- Cap_Data %>% filter(Period == Late) # the later period, where the arrow ends
    BM_Point <- tibble(Up = 1, Down = 1) # the benchmark always captures exactly its own moves

    g1 <- # build this panel of the figure in layers and store it as g1
        ggplot() + # start the panel
        annotate("rect", xmin = 1, xmax = 1.2, ymin = 0.85, ymax = 1, fill = "seagreen", alpha = 0.08) + # shade the zone where the fund gains more than the market in up months and loses less in down months
        annotate("rect", xmin = 0.95, xmax = 1.2, ymin = 1, ymax = 1.2, fill = "firebrick", alpha = 0.06) + # shade the zone where the fund loses more than the market in down months
        geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "grey50", linewidth = 0.5) + # the diagonal, where up and down capture are equal
        geom_vline(xintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.5) + # full up capture
        geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.5) + # full down capture
        annotate("segment", x = Cap_Early$Up, y = Cap_Early$Down, xend = Cap_Late$Up, yend = Cap_Late$Down, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "grey30", linewidth = 1) + # an arrow from the earlier to the later period
        geom_point(data = BM_Point, aes(Up, Down), shape = 15, size = 4, color = "black") + # the benchmark
        geom_text(data = BM_Point, aes(Up, Down, label = glue::glue(" {BM_Name}\n (1.00, 1.00)")), hjust = 1.1, vjust = 0.5, size = fmxdat::ggpts(8.5), fontface = "bold") + # label the benchmark
        geom_point(data = Cap_Data, aes(Up, Down, color = Period), size = 4.5) + # the fund in each period
        geom_text(data = Cap_Early, aes(Up, Down, label = paste0(Period, "\n(Up: ", sprintf("%.2f", Up), ", Down: ", sprintf("%.2f", Down), ")")), hjust = -0.15, vjust = 1.1, size = fmxdat::ggpts(8), color = "steelblue", fontface = "bold") + # label the earlier period below its point
        geom_text(data = Cap_Late, aes(Up, Down, label = paste0(Period, "\n(Up: ", sprintf("%.2f", Up), ", Down: ", sprintf("%.2f", Down), ")")), hjust = -0.15, vjust = -0.2, size = fmxdat::ggpts(8), color = "firebrick", fontface = "bold") + # label the later period above its point
        annotate("text", x = 1.15, y = 0.88, label = "Defensive alpha zone\n(up > 1, down < 1)", color = "seagreen", fontface = "italic", size = fmxdat::ggpts(7.5), hjust = 0.5) + # name the green zone
        annotate("text", x = 1.01, y = 1.17, label = "Downside vulnerable\n(down capture > 1)", color = "firebrick", fontface = "italic", size = fmxdat::ggpts(7.5), hjust = 0) + # name the red zone
        annotate("text", x = 1.18, y = 1.18, label = "Symmetric (y = x)", color = "grey40", angle = 45, size = fmxdat::ggpts(7), vjust = -0.5) + # name the diagonal
        scale_color_manual(values = purrr::set_names(c("steelblue", "firebrick"), c(Early, Late))) + # blue for the earlier period and red for the later one
        scale_x_continuous(limits = c(0.95, 1.2), labels = scales::percent_format(accuracy = 1)) + # up capture as a percentage
        scale_y_continuous(limits = c(0.85, 1.2), labels = scales::percent_format(accuracy = 1)) + # down capture as a percentage
        guides(color = "none") + # the points are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        labs(x = "Up-market capture ratio", y = "Down-market capture ratio", title = "Panel A. Market capture asymmetry", # axis names and panel title
             subtitle = glue::glue("Down capture moved from {scales::percent(Cap_Early$Down, accuracy = 1)} to {scales::percent(Cap_Late$Down, accuracy = 1)} after 2020")) # a subtitle computed from the data

    # Panel B, return, volatility and tail risk

    Metric_Order <- c("Return (Ann.)", "SD (Ann.)", "Max Drawdown", "CVaR Loss (95%)") # the measures shown, from top to bottom

    g2 <- # build this panel of the figure in layers and store it as g2
        Stats_All %>% # start from the statistics table
        mutate(`CVaR Loss (95%)` = abs(`Modified CVaR (95%)`)) %>% # show the tail loss as a positive number so that all bars point the same way
        select(Name, Period, all_of(Metric_Order)) %>% # keep only the measures shown
        gather(Metric, Value, -Name, -Period) %>% # long format, one row per fund, period and measure
        mutate(Metric = factor(Metric, levels = rev(Metric_Order)), Label = scales::percent(Value, accuracy = 0.1)) %>% # fixed order and readable labels
        ggplot(aes(x = Value, y = Metric, fill = Name)) + # start the panel
        geom_col(position = position_dodge(width = 0.7), width = 0.6) + # fund and benchmark side by side for each measure
        geom_text(aes(label = Label), position = position_dodge(width = 0.7), hjust = -0.1, size = fmxdat::ggpts(7.5), fontface = "bold") + # print each value at the end of its bar
        facet_wrap(~Period, ncol = 1) + # one panel per period
        scale_fill_manual(values = purrr::set_names(c("darkorange", "grey35"), c(Fund_Name, BM_Name))) + # orange for the fund and grey for the benchmark
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.25))) + # percentage axis with room for the labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        theme(legend.position = "top", legend.justification = "right") + # the legend above the bars
        labs(x = "Annualised rate or loss", y = "", fill = "", title = "Panel B. Return, volatility and tail risk", # axis names and panel title
             subtitle = "Higher returns with slightly higher volatility and slightly smaller tail losses") # the main message of the panel

    patchwork::wrap_plots(g1, g2, nrow = 1) + # the two panels side by side
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, # one title for the combined figure
                                   theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # the course theme for the shared title

}
