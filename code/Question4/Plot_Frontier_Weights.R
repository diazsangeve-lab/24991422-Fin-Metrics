# Plots the weights of every portfolio along the efficient frontier as stacked areas against its volatility, with the four portfolio rules marked, so that it is clear which assets enter as the target risk rises.
Plot_Frontier_Weights <- function(Fw, W, Pal, Title = "Portfolio Weights Along the Frontier", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Frontier_Weights, with inputs Fw, W and Pal, plus the title, subtitle and caption text

    D <- Fw %>% mutate(Asset = factor(Asset, levels = names(Pal$Asset))) # assets in family order
    Rules <- W %>% filter(date == max(date)) %>% distinct(Strategy, Vol = ExAnte_Vol) # the volatility of each rule on the same date
    Inner <- D %>% filter(Vol >= quantile(unique(Vol), 0.1), Vol <= quantile(unique(Vol), 0.9)) # the inner part of the frontier, away from its edges
    Top <- Inner %>% group_by(Asset) %>% filter(max(Weight) >= 0.08) %>% slice_max(Weight, n = 1, with_ties = FALSE) %>% ungroup() %>% mutate(y = map2_dbl(Vol, Asset, function(v, a){ s <- D %>% filter(Vol == v) %>% arrange(desc(Asset)); cs <- cumsum(s$Weight); cs[s$Asset == a] - s$Weight[s$Asset == a] / 2 })) # a label at the widest point of each large holding

    ggplot(D) + # start the plot
        geom_area(aes(Vol, Weight, fill = Asset), color = "white", linewidth = 0.15) + # the weights of each frontier portfolio
        geom_label(data = Top, aes(Vol, y, label = Asset), size = fmxdat::ggpts(7), fontface = "bold", fill = "white", color = "grey15", label.size = 0, alpha = 0.85) + # name the large holdings on their bands
        geom_vline(data = Rules, aes(xintercept = Vol, color = Strategy), linewidth = 1, linetype = "dashed") + # the volatility of each rule
        scale_fill_manual(values = Pal$Asset) + # the asset colours
        scale_color_manual(values = Darken_Col(Pal$Strategy, 0.1)) + # the rule colours
        scale_x_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0, 0)) + # volatility as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) + # weights as percentages
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "right", legend.key.size = unit(0.4, "cm")) + # the legends at the side
        guides(fill = guide_legend(ncol = 1, order = 2), color = guide_legend(ncol = 1, order = 1)) + # one column each
        labs(x = "Expected annualised volatility of the frontier portfolio", y = "Weight", fill = "", color = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
