# Plots each portfolio rule's average share of capital next to its average share of risk by asset family, as paired stacked bars, so that the gap between where money sits and where risk comes from can be read directly.
Plot_Capital_Risk_Bars <- function(W, Pal, Title = "Where the Capital Sits and Where the Risk Comes From", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Capital_Risk_Bars, with inputs W and Pal, plus the title, subtitle and caption text

    Fam_Cols <- c(`Global equity` = "#4292C6", `Local equity and property` = "#E6550D", `Global bonds` = "#41AB5D", `Local bonds` = "#756BB1", Commodities = "#D4A62A") # a mid shade for each family, which reads well as a fill

    D <- # build D through the steps below
        W %>% # start from W
        mutate(Family = Pal$Asset_Family[Asset]) %>% # each asset's family
        group_by(Strategy, date, Family) %>% # one value per family per rule and date
        summarise(Capital = sum(Weight), Risk = sum(pmax(Risk_Share, 0)), .groups = "drop") %>% # the family's capital and risk shares
        group_by(Strategy, Family) %>% summarise(Capital = mean(Capital), Risk = mean(Risk), .groups = "drop") %>% # averaged over all rebalance dates
        gather(Side, Share, Capital, Risk) %>% # long format, one row per side
        group_by(Strategy, Side) %>% mutate(Share = Share / sum(Share)) %>% ungroup() %>% # each bar sums to one
        mutate(Family = factor(Family, levels = rev(names(Fam_Cols))), # equities at the bottom of each bar
               Strategy = factor(Strategy, levels = names(Pal$Strategy)), # the rules in a fixed order
               Side = factor(Side, levels = c("Capital", "Risk"), labels = c("Share of\ncapital", "Share of\nrisk")), # readable bar names
               Lab = ifelse(Share >= 0.04, scales::percent(Share, accuracy = 1), "")) # print only shares large enough to fit

    ggplot(D, aes(Side, Share, fill = Family)) + # the two sides along the bottom
        geom_col(width = 0.72, color = "white", linewidth = 0.4) + # the stacked bars
        geom_text(aes(label = Lab, color = Family), position = position_stack(vjust = 0.5), fontface = "bold", size = fmxdat::ggpts(8)) + # each family's share inside its segment
        facet_wrap(~Strategy, nrow = 1) + # one pair of bars per rule
        scale_fill_manual(values = Fam_Cols, breaks = names(Fam_Cols)) + # the family colours
        scale_color_manual(values = Text_Contrast(Fam_Cols), guide = "none") + # black or white text, whichever reads better on each fill
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) + # shares as percentages
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "bottom", panel.grid.major.x = element_blank()) + # the family legend below
        labs(x = "", y = "", fill = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
