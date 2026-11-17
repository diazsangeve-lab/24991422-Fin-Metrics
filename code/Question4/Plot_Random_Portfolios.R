# Plots a cloud of random portfolios that satisfy the mandate, coloured by return per unit of risk, beneath the efficient frontier and the four portfolio rules.
Plot_Random_Portfolios <- function(Rp, Fr, W, Pal, Title = "Random Portfolios Within the Mandate", Subtitle = "", Caption = "Note:\nCalculation own"){

    Last <- max(Fr$Frontiers$date) # the latest rebalance date
    Front <- Fr$Frontiers %>% filter(date == Last, Frontier == "Within the mandate") # the mandate frontier on that date
    Rules <- W %>% filter(date == Last) %>% distinct(Strategy, Vol = ExAnte_Vol, Ret = ExAnte_Ret) # the four rules

    ggplot() + # start the plot
        geom_point(data = Rp, aes(Vol, Ret, color = Sharpe), size = 0.7, alpha = 0.45) + # every random portfolio
        geom_path(data = Front, aes(Vol, Ret), color = "black", linewidth = 1.3) + # the efficient frontier
        geom_point(data = Rules, aes(Vol, Ret, fill = Strategy), shape = 23, size = 5, color = "black") + # the four rules
        ggrepel::geom_label_repel(data = Rules, aes(Vol, Ret, label = Strategy), color = Darken_Col(Pal$Strategy[Rules$Strategy], 0.2), fill = "white", fontface = "bold", size = fmxdat::ggpts(7.5), min.segment.length = 0, box.padding = 0.6, seed = 3) + # name each rule
        scale_color_viridis_c(option = "mako", direction = -1, labels = function(x) sprintf("%.2f", x)) + # light for poor and dark for good return per unit of risk
        scale_fill_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + # return as a percentage
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "right") + # the colour bar at the side
        labs(x = "Expected annualised volatility", y = "Expected annualised return", color = "Return per\nunit of risk", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
