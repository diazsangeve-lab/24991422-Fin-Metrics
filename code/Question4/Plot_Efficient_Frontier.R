# Plots the efficient frontier on the latest rebalance date, within the mandate and under the long only constraint alone, with every asset and the four portfolio rules marked.
Plot_Efficient_Frontier <- function(Fr, W, Pal, Title = "The Efficient Frontier", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Efficient_Frontier, with inputs Fr, W and Pal, plus the title, subtitle and caption text

    Last <- max(Fr$Frontiers$date) # the latest rebalance date
    Now <- Fr$Frontiers %>% filter(date == Last) # both frontiers on that date
    Assets <- Fr$Assets %>% filter(date == Last) %>% mutate(Family = Pal$Asset_Family[Asset]) # each asset's expected return and volatility, with its family
    Ports <- W %>% filter(date == Last) %>% distinct(Strategy, Vol = ExAnte_Vol, Ret = ExAnte_Ret) # the four portfolio rules

    ggplot() + # start the plot
        geom_path(data = Now, aes(Vol, Ret, linetype = Frontier), color = "grey15", linewidth = 1.1) + # the two frontiers
        geom_point(data = Assets, aes(Vol, Ret, color = Family), size = 3.2) + # each asset
        ggrepel::geom_text_repel(data = Assets, aes(Vol, Ret, label = Asset), color = "grey20", size = fmxdat::ggpts(7.5), min.segment.length = 0.3, seed = 1) + # label each asset in dark text
        geom_point(data = Ports, aes(Vol, Ret, fill = Strategy), shape = 23, size = 4.5, color = "black") + # the four portfolio rules
        ggrepel::geom_label_repel(data = Ports, aes(Vol, Ret, label = Strategy), fill = "white", color = Darken_Col(Pal$Strategy[Ports$Strategy], 0.2), fontface = "bold", size = fmxdat::ggpts(7.5), nudge_x = -0.03, direction = "y", min.segment.length = 0, seed = 2) + # name each rule
        scale_color_manual(values = Pal$Family) + # one colour per family
        scale_fill_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_linetype_manual(values = c(`Within the mandate` = "solid", `Long only, no other limits` = "dashed")) + # solid for the mandate and dashed for the looser frontier
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # return as a percentage
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "bottom", legend.box = "vertical") + # the legends below the plot
        labs(x = "Expected annualised volatility", y = "Expected annualised return", color = "", linetype = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
