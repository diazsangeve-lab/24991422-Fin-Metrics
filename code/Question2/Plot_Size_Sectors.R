# Plots each index's sector weights through time as small multiples of stacked areas, using a fixed sector palette.
Plot_Size_Sectors <- function(Holdings, Sector_Cols = c(Financials = "#4C78A8", Industrials = "#9ECAE9", Property = "#59A14F", Unclassified = "#E8998D", Resources = "#B2182B"), Title = "Sector Exposure by Size Index", Subtitle = "", Caption = "Note:\nCalculation own"){

    Sectors <-
        Holdings %>%
        group_by(Fund, date, Sector) %>% # one weight per sector per index and month
        summarise(Weight = sum(Weight), .groups = "drop") %>% # the sector's total weight
        tidyr::complete(Fund, date, Sector, fill = list(Weight = 0)) %>% # a sector that is not held gets a zero, so the stacked areas have no gaps
        mutate(Sector = factor(Sector, levels = names(Sector_Cols))) # resources last, so that they sit at the bottom of each stack

    Latest <- Sectors %>% filter(date == max(date), Sector == "Resources") # the latest resources weight of each index, printed on the panel

    g <-
        Sectors %>%
        ggplot() + # start the plot
        geom_area(aes(date, Weight, fill = Sector), alpha = 0.9, color = "white", linewidth = 0.15) + # stacked sector weights through time
        geom_label(data = Latest, aes(date, 0.04, label = glue::glue("Resources {scales::percent(Weight, accuracy = 1)}")), hjust = 1, size = fmxdat::ggpts(7.5), label.size = 0, fill = "white", alpha = 0.85) + # the latest resources weight in the corner of each panel
        facet_wrap(~Fund, nrow = 1) + # one panel per index
        scale_fill_manual(values = Sector_Cols) + # the fixed sector palette
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) + # weights as percentages
        scale_x_date(date_breaks = "4 years", date_labels = "%Y") + # date labels every four years so that the narrow panels stay readable
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Share of index weight", title = Title, subtitle = Subtitle, caption = Caption, fill = "") # axis names, title and note

    g

}
