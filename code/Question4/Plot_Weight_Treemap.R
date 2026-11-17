# Plots the latest weights of each portfolio rule as treemaps, nested by asset family, so that the size of every holding and every family can be compared at a glance.
Plot_Weight_Treemap <- function(W, Pal, Title = "Latest Portfolio Composition", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <-
        W %>%
        filter(date == max(date), Weight > 0.001) %>% # holdings on the latest rebalance date
        mutate(Family = Pal$Asset_Family[Asset], # each asset's family
               Strategy = factor(Strategy, levels = names(Pal$Strategy)), # the rules in a fixed order
               Lab = glue::glue("{Asset}\n{scales::percent(Weight, accuracy = 1)}")) # the asset and its weight

    g <-
        ggplot(D, aes(area = Weight, fill = Asset, subgroup = Family, label = Lab, color = Asset)) + # tile area in proportion to weight, grouped by family
        treemapify::geom_treemap(color = "white", size = 1.5) + # one tile per holding
        treemapify::geom_treemap_subgroup_border(color = "white", size = 4) + # thick borders around each family
        treemapify::geom_treemap_text(fontface = "bold", place = "centre", reflow = TRUE, min.size = 5, size = 10) + # the asset and weight inside each tile
        facet_wrap(~Strategy, ncol = 2) + # one treemap per rule
        scale_fill_manual(values = Pal$Asset, guide = "none") + # the asset colours
        scale_color_manual(values = Text_Contrast(Pal$Asset), guide = "none") + # black text on light tiles and white text on dark ones
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(title = Title, subtitle = Subtitle, caption = Caption) # title and note

    g

}
