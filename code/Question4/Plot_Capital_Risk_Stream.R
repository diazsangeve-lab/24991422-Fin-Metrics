# Plots each portfolio rule's capital weights above the axis and its shares of risk mirrored below it through time, as smoothed streams, so that the gap between where money sits and where risk comes from is visible.
Plot_Capital_Risk_Stream <- function(W, Pal, Title = "Capital and Risk Through Time", Subtitle = "", Caption = "Note:\nCalculation own"){

    Grid <- seq(min(W$date), max(W$date), by = "week") # a fine grid over which the streams are smoothed

    Smooth <- function(df, Col, Sign) { # smooth one measure and stack it above or below the axis
        df %>%
            select(date, Strategy, Asset, Value = all_of(Col)) %>% # the measure on each rebalance date
            tidyr::complete(date, Strategy, Asset = names(Pal$Asset), fill = list(Value = 0)) %>% # an asset that is not held contributes nothing
            group_by(Strategy, Asset) %>% # smooth each asset within each rule
            reframe(Value = pmax(stats::splinefun(as.numeric(date), Value, method = "monoH.FC")(as.numeric(Grid)), 0), date = Grid) %>% # a shape preserving curve through the quarterly values
            group_by(Strategy, date) %>% # work within each rule and date
            mutate(Value = Value / sum(Value), Asset = factor(Asset, levels = names(Pal$Asset))) %>% # rescale to sum to one and keep family order
            arrange(Asset, .by_group = TRUE) %>% # stack in family order
            mutate(Top = Sign * cumsum(Value), Bottom = Top - Sign * Value) %>% # the edges of each band, above the axis for capital and below it for risk
            ungroup()
    }

    D <- bind_rows(Smooth(W, "Weight", 1) %>% mutate(Side = "Capital"), Smooth(W %>% mutate(Risk_Share = pmax(Risk_Share, 0)), "Risk_Share", -1) %>% mutate(Side = "Risk")) %>% mutate(Strategy = factor(Strategy, levels = names(Pal$Strategy))) # both sides for every rule
    Side_Lab <- tibble(Strategy = factor(names(Pal$Strategy), levels = names(Pal$Strategy))) %>% tidyr::crossing(tibble(Side = c("CAPITAL", "RISK"), y = c(0.5, -0.5))) # labels for the two sides

    g <-
        ggplot(D) + # start the plot
        geom_ribbon(aes(date, ymin = Bottom, ymax = Top, fill = Asset, group = interaction(Asset, Side)), color = "white", linewidth = 0.06) + # one band per asset on each side
        geom_hline(yintercept = 0, color = "black", linewidth = 0.8) + # the axis that separates capital from risk
        geom_text(data = Side_Lab, aes(min(D$date) + 40, y, label = Side), hjust = 0, angle = 90, vjust = 1, fontface = "bold", color = "white", size = fmxdat::ggpts(9)) + # name each side
        facet_wrap(~Strategy, ncol = 2) + # one panel per rule
        scale_fill_manual(values = Pal$Asset) + # the asset colours
        scale_y_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.5)) + # shares on both sides shown as positive percentages
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) + # yearly labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "right", legend.key.size = unit(0.4, "cm"), panel.grid.major.x = element_blank()) + # the asset legend at the side
        guides(fill = guide_legend(ncol = 1)) + # one column of assets
        labs(x = "", y = "Share of capital (above) and of risk (below)", fill = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g

}
