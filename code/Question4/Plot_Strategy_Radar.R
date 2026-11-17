# Plots each portfolio rule's standing on several measures as a radar chart, scaling every measure so that the outer edge is the best of the four rules.
Plot_Strategy_Radar <- function(Stats, Pal, Title = "Strategy Scorecard", Subtitle = "", Caption = "Note:\nCalculation own"){

    Measures <- tibble(Col = c("Ret", "Ratio", "Vol", "MaxDD", "CVaR", "Turnover", "Eff_Risk"), # the measures compared
                       Label = c("Return", "Return per unit\nof volatility", "Low volatility", "Shallow\ndrawdown", "Small tail\nlosses", "Low turnover", "Diversified\nrisk"), # readable names
                       Higher_Better = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)) # whether a higher value is better

    K <- nrow(Measures) # the number of spokes
    Scores <-
        Measures %>%
        mutate(Angle = pi / 2 - 2 * pi * (row_number() - 1) / K) %>% # the angle of each spoke, starting at the top
        tidyr::crossing(Strategy = Stats$Strategy) %>% # one row per rule and measure
        mutate(Value = map2_dbl(Col, Strategy, ~Stats[[.x]][Stats$Strategy == .y])) %>% # the rule's value on the measure
        group_by(Col) %>% # scale within each measure
        mutate(Score = ifelse(Higher_Better, (Value - min(Value)) / (max(Value) - min(Value)), (max(Value) - Value) / (max(Value) - min(Value))), Score = 0.2 + 0.8 * Score) %>% # zero to one with the best rule at one, kept off the centre so that every shape is visible
        ungroup() %>%
        mutate(x = Score * cos(Angle), y = Score * sin(Angle), Strategy = factor(Strategy, levels = names(Pal$Strategy))) %>% # the point on each spoke
        arrange(Strategy, desc(Angle)) # order the points around each polygon

    Rings <- tidyr::crossing(r = c(0.2, 0.6, 1), a = seq(0, 2 * pi, length.out = 200)) %>% mutate(x = r * cos(a), y = r * sin(a)) # reference rings for worst, middle and best
    Spokes <- Measures %>% mutate(Angle = pi / 2 - 2 * pi * (row_number() - 1) / K, x = cos(Angle), y = sin(Angle)) # the spokes and their labels

    ggplot() + # start the plot
        geom_path(data = Rings, aes(x, y, group = r), color = "grey85") + # the reference rings
        geom_segment(data = Spokes, aes(0, 0, xend = x, yend = y), color = "grey80") + # the spokes
        geom_polygon(data = Scores, aes(x, y, fill = Strategy, color = Strategy), alpha = 0.12, linewidth = 1.1) + # one polygon per rule
        geom_point(data = Scores, aes(x, y, color = Strategy), size = 2.2) + # the points on each spoke
        geom_text(data = Spokes, aes(1.34 * x, 1.3 * y, label = Label), fontface = "bold", size = fmxdat::ggpts(8.5), lineheight = 0.85, color = "grey25") + # the spoke labels outside the rings
        facet_wrap(~Strategy, nrow = 1) + # one radar per rule, as small multiples
        scale_fill_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_color_manual(values = Pal$Strategy, guide = "none") + # the same colours for the outlines
        coord_equal(xlim = c(-1.85, 1.85), ylim = c(-1.5, 1.5)) + # a round radar with room for the labels
        theme_void() + # no axes, as in the gallery's radar chart
        theme(strip.text = element_text(face = "bold", size = 12), plot.title = element_text(face = "bold", size = 16), plot.subtitle = element_text(size = 12, color = "grey30"), plot.caption = element_text(hjust = 0, size = 9, color = "grey30"), plot.background = element_rect(fill = "white", color = NA)) + # titles in the course style
        labs(title = Title, subtitle = Subtitle, caption = Caption) # title and note

}
