# Plots the growth of one unit invested in each portfolio rule with the rule's name written along its line, above an underwater chart of drawdowns from the previous peak.
Plot_Growth_Underwater <- function(Rets, Pal, Title = "Growth and Drawdowns of the Four Portfolios", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <- Rets %>% arrange(date) %>% group_by(Strategy) %>% mutate(Growth = cumprod(1 + Returns), Drawdown = Growth / cummax(pmax(Growth, 1)) - 1) %>% ungroup() %>% mutate(Strategy = factor(Strategy, levels = names(Pal$Strategy))) # wealth and drawdown paths, counting the starting value as the first peak
    Start <- D %>% group_by(Strategy) %>% slice(1) %>% mutate(date = date - months(1), Growth = 1, Drawdown = 0) %>% ungroup() # a starting point of one at the first rebalance
    D <- bind_rows(Start, D) %>% arrange(Strategy, date) # every path begins at one

    g1 <-
        D %>%
        mutate(Above = Strategy %in% c("Maximum Sharpe", "Risk parity"), Hj = case_when(Strategy == "Maximum Sharpe" ~ 0.8, Strategy == "Constrained equal weight" ~ 0.8, TRUE ~ 0.97)) %>% # which labels sit above their line and where along it, chosen where the lines separate
        ggplot(aes(date, Growth, color = Strategy, label = Strategy, hjust = Hj)) + # dates along the bottom and wealth up the side
        geom_hline(yintercept = 1, color = "grey50", linetype = "dashed") + # the starting value
        geomtextpath::geom_textline(data = ~filter(., Above), linewidth = 1.1, size = fmxdat::ggpts(8.5), fontface = "bold", vjust = -0.35, text_smoothing = 50) + # the upper rules with their names above the line
        geomtextpath::geom_textline(data = ~filter(., !Above), linewidth = 1.1, size = fmxdat::ggpts(8.5), fontface = "bold", vjust = 1.35, text_smoothing = 50) + # the lower rules with their names below the line
        scale_color_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # yearly labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        theme(axis.text.x = element_blank()) + # the panel below carries the dates
        labs(x = "", y = "Growth of one unit") # axis names

    g2 <-
        D %>%
        ggplot(aes(date, Drawdown, color = Strategy, fill = Strategy)) + # dates along the bottom and drawdown up the side
        geom_area(position = "identity", alpha = 0.12, linewidth = 0.6) + # the drawdown of each rule, overlaid
        scale_color_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_fill_manual(values = Pal$Strategy, guide = "none") + # the same colours for the fills
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # drawdowns as percentages
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # yearly labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20)) + # the clean course theme
        labs(x = "", y = "Below previous peak") # axis names

    patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(2.4, 1)) + # growth above the underwater chart
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # one title for the figure

}
