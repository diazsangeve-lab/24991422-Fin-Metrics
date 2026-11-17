# Plots the efficient frontier on every rebalance date as faint trails, the latest frontiers within and without the mandate, each asset as a bubble sized by the chosen portfolio's weight, and the four portfolio rules as stars.
Plot_Frontier_Galaxy <- function(Fr, W, Pal, Highlight = "Risk parity", Title = "The Efficient Frontier Within the Mandate", Subtitle = "", Caption = "Note:\nCalculation own"){

    Last <- max(Fr$Frontiers$date) # the latest rebalance date
    Trails <- Fr$Frontiers %>% filter(Frontier == "Within the mandate", date < Last) %>% mutate(Shade = grDevices::gray(0.9 - 0.4 * as.numeric(date - min(date)) / as.numeric(Last - min(date)))) # the earlier frontiers within the mandate, shaded from pale to darker over time
    Now <- Fr$Frontiers %>% filter(date == Last) # both frontiers on the latest date
    Bubbles <- Fr$Assets %>% filter(date == Last) %>% left_join(W %>% filter(date == Last, Strategy == Highlight) %>% select(Asset, Weight), by = "Asset") %>% mutate(Weight = coalesce(Weight, 0)) # each asset on the latest date, with the chosen portfolio's weight
    Stars <- W %>% filter(date == Last) %>% distinct(Strategy, Vol = ExAnte_Vol, Ret = ExAnte_Ret) # the four portfolio rules on the latest date

    ggplot() + # start the plot
        geom_path(data = Trails, aes(Vol, Ret, group = date), color = Trails$Shade, linewidth = 0.5) + # the frontier on each earlier rebalance date
        geom_path(data = Now, aes(Vol, Ret, linetype = Frontier), color = "black", linewidth = 1.3) + # the latest frontiers, within the mandate and long only alone
        geom_point(data = Bubbles, aes(Vol, Ret, size = Weight, fill = Asset), shape = 21, color = "white", stroke = 0.6, alpha = 0.9) + # each asset as a bubble sized by the chosen portfolio's weight
        ggrepel::geom_text_repel(data = Bubbles, aes(Vol, Ret, label = Asset, color = Asset), size = fmxdat::ggpts(7.5), fontface = "bold", max.overlaps = 30, min.segment.length = 0.2, seed = 1) + # label each asset in its own colour
        geom_point(data = Stars, aes(Vol, Ret), shape = 23, size = 5.5, fill = Pal$Strategy[Stars$Strategy], color = "black", stroke = 0.8) + # the four portfolio rules
        ggrepel::geom_label_repel(data = Stars, aes(Vol, Ret, label = Strategy), fill = "white", color = Pal$Strategy[Stars$Strategy], fontface = "bold", size = fmxdat::ggpts(8), nudge_x = -0.035, direction = "y", min.segment.length = 0, seed = 2) + # name each portfolio rule
        scale_fill_manual(values = Pal$Asset, guide = "none") + # the asset colours
        scale_color_manual(values = Pal$Asset, guide = "none") + # the same colours for the labels
        scale_size_area(max_size = 14, breaks = c(0.03, 0.06, 0.09), labels = scales::percent_format(accuracy = 1)) + # bubble area in proportion to weight, with three reference sizes in the legend
        scale_linetype_manual(values = c(`Within the mandate` = "solid", `Long only, no other limits` = "22")) + # solid for the mandate and dashed for the looser frontier
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # return as a percentage
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "bottom") + # the legends below the plot
        labs(x = "Expected annualised volatility", y = "Expected annualised return", size = glue::glue("{Highlight} weight"), linetype = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
