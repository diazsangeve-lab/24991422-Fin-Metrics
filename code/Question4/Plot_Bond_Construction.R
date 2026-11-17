# Plots the government bond yields next to the total return indexes built from them, with labels running along each line, so that the link between falling or rising yields and bond returns is visible.
Plot_Bond_Construction <- function(Bond_Rets, Cols = c(US_10Yr = "#00441B", EURO_10Yr = "#2E8B3E", UK_10Yr = "#2171B5"), Labels = c(US_10Yr = "US 10 year", EURO_10Yr = "Euro 10 year", UK_10Yr = "UK 10 year"), Title = "From Yields to Bond Returns", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <- Bond_Rets %>% arrange(date) %>% group_by(Name) %>% mutate(Index = cumprod(1 + Returns), Lab = Labels[Name]) %>% ungroup() # the growth of one unit invested in each constructed index

    g1 <-
        D %>%
        ggplot(aes(date, Yield, color = Name, label = Lab)) + # dates along the bottom and yields up the side
        geom_line(linewidth = 0.9) + # each yield
        ggrepel::geom_text_repel(data = ~dplyr::filter(dplyr::group_by(., Name), date == max(date)), aes(label = Lab), hjust = 0, nudge_x = 400, direction = "y", fontface = "bold", size = fmxdat::ggpts(9), segment.color = NA, seed = 1) + # each name at the end of its line, pushed apart so the labels never overlap
        geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + # a zero yield, which the euro yield dipped below
        scale_color_manual(values = Cols, guide = "none") + # the bond colours
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # yields as percentages
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        scale_x_date(breaks = seq(lubridate::ymd(20050101), max(D$date), by = "5 years"), date_labels = "%Y", expand = expansion(mult = c(0.02, 0.3))) + # labels every five years that stop at the data, with room on the right for the names
        labs(x = "", y = "Yield to maturity", title = "Ten year government bond yields") # axis names and panel title

    g2 <-
        D %>%
        ggplot(aes(date, Index, color = Name, label = Lab)) + # dates along the bottom and growth up the side
        annotate("rect", xmin = lubridate::ymd(20220101), xmax = lubridate::ymd(20221231), ymin = -Inf, ymax = Inf, fill = "firebrick", alpha = 0.08) + # shade the 2022 bond sell-off
        annotate("text", x = lubridate::ymd(20220701), y = min(D$Index) + 0.15, label = "2022 rate shock", color = "firebrick", fontface = "italic", size = fmxdat::ggpts(8.5), angle = 90, hjust = 0) + # name the shaded year, below the lines
        geom_line(linewidth = 0.9) + # each index
        ggrepel::geom_text_repel(data = ~dplyr::filter(dplyr::group_by(., Name), date == max(date)), aes(label = Lab), hjust = 0, nudge_x = 400, direction = "y", fontface = "bold", size = fmxdat::ggpts(9), segment.color = NA, seed = 1) + # each name at the end of its line, pushed apart so the labels never overlap
        scale_color_manual(values = Cols, guide = "none") + # the bond colours
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        scale_x_date(breaks = seq(lubridate::ymd(20050101), max(D$date), by = "5 years"), date_labels = "%Y", expand = expansion(mult = c(0.02, 0.3))) + # labels every five years that stop at the data, with room on the right for the names
        labs(x = "", y = "Growth of one unit", title = "Constructed total return indexes") # axis names and panel title

    patchwork::wrap_plots(g1, g2, nrow = 1) + # the two panels side by side
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # one title for the figure

}
