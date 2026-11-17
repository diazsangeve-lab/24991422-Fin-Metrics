# Plots the government bond yields next to the total return indexes built from them, with labels running along each line, so that the link between falling or rising yields and bond returns is visible.
Plot_Bond_Construction <- function(Bond_Rets, Cols = c(US_10Yr = "#00441B", EURO_10Yr = "#41AB5D", UK_10Yr = "#2171B5"), Labels = c(US_10Yr = "US 10 year", EURO_10Yr = "Euro 10 year", UK_10Yr = "UK 10 year"), Title = "From Yields to Bond Returns", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <- Bond_Rets %>% arrange(date) %>% group_by(Name) %>% mutate(Index = cumprod(1 + Returns), Lab = Labels[Name]) %>% ungroup() # the growth of one unit invested in each constructed index

    g1 <-
        D %>%
        ggplot(aes(date, Yield, color = Name, label = Lab)) + # dates along the bottom and yields up the side
        geomtextpath::geom_textline(linewidth = 1, size = fmxdat::ggpts(9), fontface = "bold", hjust = 0.62, vjust = -0.3, text_smoothing = 40) + # each yield with its name written along the line, placed where the three lines are furthest apart
        geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + # a zero yield, which the euro yield dipped below
        scale_color_manual(values = Cols, guide = "none") + # the bond colours
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # yields as percentages
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        labs(x = "", y = "Yield to maturity", title = "Ten year government bond yields") # axis names and panel title

    g2 <-
        D %>%
        ggplot(aes(date, Index, color = Name, label = Lab)) + # dates along the bottom and growth up the side
        annotate("rect", xmin = lubridate::ymd(20220101), xmax = lubridate::ymd(20221231), ymin = -Inf, ymax = Inf, fill = "firebrick", alpha = 0.08) + # shade the 2022 bond sell-off
        annotate("text", x = lubridate::ymd(20220701), y = min(D$Index) + 0.15, label = "2022 rate shock", color = "firebrick", fontface = "italic", size = fmxdat::ggpts(8.5), angle = 90, hjust = 0) + # name the shaded year, below the lines
        geomtextpath::geom_textline(linewidth = 1, size = fmxdat::ggpts(9), fontface = "bold", hjust = 0.55, vjust = -0.3, text_smoothing = 40) + # each index with its name written along the line, placed where the three lines are furthest apart
        scale_color_manual(values = Cols, guide = "none") + # the bond colours
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        labs(x = "", y = "Growth of one unit", title = "Constructed total return indexes") # axis names and panel title

    patchwork::wrap_plots(g1, g2, nrow = 1) + # the two panels side by side
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # one title for the figure

}
