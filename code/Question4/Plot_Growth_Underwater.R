# Plots the growth of one unit invested in each portfolio rule with the rule's name written along its line, above an underwater chart of drawdowns from the previous peak.
Plot_Growth_Underwater <- function(Rets, Pal, Title = "Growth and Drawdowns of the Four Portfolios", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Growth_Underwater, with inputs Rets and Pal, plus the title, subtitle and caption text

    D <- Rets %>% arrange(date) %>% group_by(Strategy) %>% mutate(Growth = cumprod(1 + Returns), Drawdown = Growth / cummax(pmax(Growth, 1)) - 1) %>% ungroup() %>% mutate(Strategy = factor(Strategy, levels = names(Pal$Strategy))) # wealth and drawdown paths, counting the starting value as the first peak
    Start <- D %>% group_by(Strategy) %>% slice(1) %>% mutate(date = date - months(1), Growth = 1, Drawdown = 0) %>% ungroup() # a starting point of one at the first rebalance
    D <- bind_rows(Start, D) %>% arrange(Strategy, date) # every path begins at one

    g1 <- # build this panel of the figure in layers and store it as g1
        D %>% # start from D, built above
        ggplot(aes(date, Growth, color = Strategy)) + # dates along the bottom and wealth up the side
        geom_hline(yintercept = 1, color = "grey50", linetype = "dashed") + # the starting value
        geom_line(linewidth = 1.1) + # each rule's path
        ggrepel::geom_text_repel(data = ~filter(., date == max(date)), aes(label = glue::glue("{Strategy} ({sprintf('%.2f', Growth)})")), hjust = 0, nudge_x = 60, direction = "y", fontface = "bold", size = fmxdat::ggpts(8.5), segment.color = NA, seed = 1) + # each rule's name and final value at the end of its line, pushed apart so the labels never overlap
        scale_color_manual(values = Darken_Col(Pal$Strategy, 0.1), guide = "none") + # the rule colours, slightly darkened for the labels
        scale_x_date(breaks = seq(lubridate::floor_date(min(D$date), "year"), max(D$date), by = "1 year"), date_labels = "%Y", expand = expansion(mult = c(0.02, 0.22))) + # yearly labels with room on the right
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        theme(axis.text.x = element_blank()) + # the panel below carries the dates
        labs(x = "", y = "Growth of one unit") # axis names

    g2 <- # build this panel of the figure in layers and store it as g2
        D %>% # start from D, built above
        ggplot(aes(date, Drawdown, color = Strategy, fill = Strategy)) + # dates along the bottom and drawdown up the side
        geom_area(position = "identity", alpha = 0.12, linewidth = 0.6) + # the drawdown of each rule, overlaid
        scale_color_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_fill_manual(values = Pal$Strategy, guide = "none") + # the same colours for the fills
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # drawdowns as percentages
        scale_x_date(breaks = seq(lubridate::floor_date(min(D$date), "year"), max(D$date), by = "1 year"), date_labels = "%Y", expand = expansion(mult = c(0.02, 0.22))) + # the same dates as the panel above
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        labs(x = "", y = "Below previous peak") # axis names

    patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(2.4, 1)) + # growth above the underwater chart
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # one title for the figure

}
