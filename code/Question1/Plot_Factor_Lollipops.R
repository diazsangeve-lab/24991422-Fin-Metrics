# Plots each portfolio's average composite factor score as lollipops in small multiples, highlighting the fund and its benchmark against the strategy indexes.
Plot_Factor_Lollipops <- function(df, Start_Date, Fund = "SafeCore", BM = "Capped SWIX", Port_Order = NULL, Title = "Composite Factor Scores by Strategy", Subtitle = "", Caption = "Note:\nCalculation own"){

    Avg <-
        df %>%
        filter(date >= Start_Date) %>% # the period over which exposures are averaged
        group_by(Portfolio, Factor) %>% # one number per portfolio and factor
        summarise(Score = mean(Score, na.rm = T), .groups = "drop") %>% # the average composite score over the period
        mutate(Role = ifelse(Portfolio == Fund, Fund, ifelse(Portfolio == BM, BM, "Other strategies"))) # who to highlight

    if( !is.null(Port_Order) ) Avg <- Avg %>% mutate(Portfolio = factor(Portfolio, levels = rev(Port_Order))) # put the portfolios in the order given, top to bottom

    g <-
        Avg %>%
        ggplot() + # start the plot
        geom_vline(xintercept = 0, color = "grey40") + # zero is the average stock
        geom_segment(aes(x = 0, xend = Score, y = Portfolio, yend = Portfolio, color = Role), linewidth = 0.8) + # the stick of each lollipop
        geom_point(aes(Score, Portfolio, color = Role, size = Role)) + # the head of each lollipop
        geom_text(data = Avg %>% filter(Role == Fund), aes(Score, Portfolio, label = sprintf("%.2f", Score)), color = "darkorange", fontface = "bold", hjust = -0.45, size = fmxdat::ggpts(7.5)) + # print the fund's score to the right of its point
        facet_wrap(~Factor, nrow = 2) + # one small multiple per factor
        scale_color_manual(values = purrr::set_names(c("darkorange", "black", "grey65"), c(Fund, BM, "Other strategies"))) + # orange for the fund, black for the benchmark and grey for the rest
        scale_size_manual(values = purrr::set_names(c(3.5, 2.8, 2), c(Fund, BM, "Other strategies")), guide = "none") + # larger points for the fund and benchmark
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(axis.text.y = element_text(size = 8)) + # smaller portfolio names so that the panels fit
        labs(x = "Average composite z-score", y = "", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    fmxdat::finplot(g) # finishing touches

}
