# Plots the average composite factor score of each portfolio as a labelled heatmap, centred on zero for the average stock.
Plot_Factor_Heatmap <- function(df, Start_Date, Port_Order = NULL, Cap = 0.5, Title = "Composite Factor Scores by Strategy", Subtitle = "", Caption = "Note:\nCalculation own"){

    Avg <-
        df %>%
        filter(date >= Start_Date) %>% # the period over which exposures are averaged
        group_by(Portfolio, Factor) %>% # one number per portfolio and factor
        summarise(Score = mean(Score, na.rm = T), .groups = "drop") # the average composite score over the period

    if( !is.null(Port_Order) ) Avg <- Avg %>% mutate(Portfolio = factor(Portfolio, levels = Port_Order)) # put the portfolios in the order given

    g <-
        Avg %>%
        ggplot() + # start the plot
        geom_tile(aes(Factor, Portfolio, fill = Score), color = "white", linewidth = 0.6) + # one tile per portfolio and factor
        geom_text(aes(Factor, Portfolio, label = sprintf("%.2f", Score)), size = fmxdat::ggpts(9)) + # print the score inside each tile
        scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkorange", midpoint = 0, limits = c(-Cap, Cap), oob = scales::squish) + # blue for a negative tilt, white for neutral and orange for a positive tilt, with the colours capped so that one extreme portfolio does not wash out the rest
        scale_y_discrete(limits = rev) + # the first portfolio at the top
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(panel.grid = element_blank(), legend.key.width = unit(1.5, "cm")) + # no grid lines behind the tiles and a wider legend
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption, fill = "Average\nz-score") # axis names, title and note

    fmxdat::finplot(g, x.vert = T) # finishing touches, with vertical factor labels

}
