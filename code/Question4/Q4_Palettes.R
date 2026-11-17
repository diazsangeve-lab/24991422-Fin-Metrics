# Returns the colour palettes used across question 4, with assets shaded within families by region and asset class, and one colour per portfolio rule.
Q4_Palettes <- function(Meta){

    Family <- Meta %>% mutate(Family = case_when(Class == "Equity" & Region == "Global" ~ "Global equity", Class == "Equity" ~ "Local equity and property", Class == "Bonds" & Region == "Global" ~ "Global bonds", Class == "Bonds" ~ "Local bonds", TRUE ~ "Commodities")) # one family per region and class
    Ramps <- list(`Global equity` = c("#08306B", "#9ECAE1"), `Local equity and property` = c("#7F2704", "#FDAE6B"), `Global bonds` = c("#00441B", "#A1D99B"), `Local bonds` = c("#3F007D", "#BCBDDC"), Commodities = c("#8C6D1F", "#F2D57E")) # the darkest and lightest shade of each family
    Fam_Order <- names(Ramps) # the order in which families are stacked

    Asset_Cols <- Family %>% mutate(Family = factor(Family, levels = Fam_Order)) %>% arrange(Family) %>% group_by(Family) %>% mutate(Col = grDevices::colorRampPalette(Ramps[[as.character(dplyr::first(Family))]])(n())) %>% ungroup() # spread the shades of each family across its assets

    list(Asset = purrr::set_names(Asset_Cols$Col, Asset_Cols$Asset), # one colour per asset, in stacking order
         Family = purrr::set_names(map_chr(Ramps, 1), Fam_Order), # one colour per family
         Asset_Family = purrr::set_names(as.character(Asset_Cols$Family), Asset_Cols$Asset), # each asset's family
         Strategy = c(`Constrained equal weight` = "grey50", `Minimum variance` = "#2A9D8F", `Risk parity` = "darkorange", `Maximum Sharpe` = "#B2182B")) # one colour per portfolio rule

}
