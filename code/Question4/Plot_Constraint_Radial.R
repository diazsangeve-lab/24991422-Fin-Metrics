# Plots how much of each mandate limit every portfolio rule uses on average as a circular barplot, with a ring marking a fully used limit and gaps separating the rules.
Plot_Constraint_Radial <- function(W, Pal, Caps = c(Bonds = 0.25, Equity = 0.75, Commodity = 0.15), Global_Cap = 0.60, Single_Cap = 0.35, Title = "How Hard Each Rule Leans on the Mandate", Subtitle = "", Caption = "Note:\nCalculation own"){

    Use <-
        W %>%
        group_by(Strategy, date) %>% # one set of exposures per rule and date
        summarise(`Bonds (25%)` = sum(Weight[Class == "Bonds"]) / Caps[["Bonds"]], # bond exposure as a share of its limit
                  `Equity (75%)` = sum(Weight[Class == "Equity"]) / Caps[["Equity"]], # equity exposure as a share of its limit
                  `Commodities (15%)` = sum(Weight[Class == "Commodity"]) / Caps[["Commodity"]], # commodity exposure as a share of its limit
                  `Global (60%)` = sum(Weight[Region == "Global"]) / Global_Cap, # offshore exposure as a share of its limit
                  `Largest asset (35%)` = max(Weight) / Single_Cap, # the largest single holding as a share of its limit
                  .groups = "drop") %>%
        gather(Limit, Used, -Strategy, -date) %>% # long format, one row per limit
        group_by(Strategy, Limit) %>% summarise(Used = mean(Used), .groups = "drop") %>% # the average use over all rebalance dates
        mutate(Strategy = factor(Strategy, levels = names(Pal$Strategy))) %>% # the rules in a fixed order
        arrange(Strategy, Limit) # order the bars

    Gap <- 2 # empty bars between groups
    D <- Use %>% group_by(Strategy) %>% mutate(id = row_number() + (as.numeric(Strategy) - 1) * (n() + Gap)) %>% ungroup() # the position of each bar around the circle
    N <- max(D$id) + Gap # the total number of positions
    D <- D %>% mutate(Angle = 90 - 360 * (id - 0.5) / N, Hjust = ifelse(Angle < -90, 1, 0), Angle = ifelse(Angle < -90, Angle + 180, Angle)) # label angles that keep the text upright, as in the gallery's circular barplot
    Groups <- D %>% group_by(Strategy) %>% summarise(Start = min(id), End = max(id), Mid = mean(id), .groups = "drop") # the span of each rule

    ggplot(D) + # start the plot
        geom_hline(yintercept = c(0.5, 1), color = c("grey85", "black"), linetype = c("dotted", "dashed"), linewidth = c(0.4, 0.6)) + # rings at half and fully used
        geom_col(aes(id, Used, fill = Strategy), width = 0.85, alpha = 0.9) + # one bar per limit
        geom_text(aes(id, Used + 0.06, label = glue::glue("{Limit} {scales::percent(Used, accuracy = 1)}"), angle = Angle, hjust = Hjust), size = fmxdat::ggpts(6.8), fontface = "bold", color = "grey20") + # the limit and its average use beside each bar
        geom_segment(data = Groups, aes(x = Start - 0.4, xend = End + 0.4, y = -0.08, yend = -0.08, color = Strategy), linewidth = 1.6) + # an arc under each rule's bars
        geom_text(data = Groups, aes(Mid, -0.32, label = gsub(" ", "\n", Strategy), color = Strategy), fontface = "bold", size = fmxdat::ggpts(7.5), lineheight = 0.85) + # the rule's name inside the ring
        annotate("text", x = N, y = 1.03, label = "fully used", size = fmxdat::ggpts(6.5), fontface = "italic", hjust = 0.5) + # name the outer ring
        scale_fill_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_color_manual(values = Pal$Strategy, guide = "none") + # the same colours for the arcs and names
        scale_x_continuous(limits = c(0, N)) + # leave the gaps around the circle
        scale_y_continuous(limits = c(-0.7, 1.55)) + # a hole in the middle and room for the labels outside
        coord_polar(start = 0) + # bend the bars into a circle
        theme_void() + # no axes, as in the gallery's circular barplot
        theme(plot.title = element_text(face = "bold", size = 16), plot.subtitle = element_text(size = 12, color = "grey30"), plot.caption = element_text(hjust = 0, size = 9, color = "grey30"), plot.background = element_rect(fill = "white", color = NA)) + # titles in the course style
        labs(title = Title, subtitle = Subtitle, caption = Caption) # title and note

}
