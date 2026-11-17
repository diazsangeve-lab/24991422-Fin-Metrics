# Plots each sector's share of capital next to its share of portfolio risk through time, so that risk concentration stands out.
Plot_Risk_vs_Weight <- function(df, Title = "Where the Capital Sits and Where the Risk Comes From", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("date", "Sector", "Weight", "Risk_Share") %in% names(df)) ) stop("... for Plot_Risk_vs_Weight to work, first run Risk_Contribution_Calc") # stop early if the risk decomposition is missing

    g <-
        df %>%
        mutate(Sector = coalesce(Sector, "Unclassified")) %>% # give stocks with no sector a label
        group_by(date, Sector) %>% # aggregate the stocks into sectors
        summarise(`Share of capital` = sum(Weight), `Share of risk` = sum(Risk_Share), .groups = "drop") %>% # the sector's weight and its share of portfolio volatility
        tidyr::complete(date, Sector, fill = list(`Share of capital` = 0, `Share of risk` = 0)) %>% # give a sector a zero on dates when it is not held, so that the stacked areas have no gaps
        gather(Measure, Share, -date, -Sector) %>% # long format, so that the two measures can be faceted
        ggplot() + # start the plot
        geom_area(aes(date, Share, fill = Sector), alpha = 0.85, color = "white", linewidth = 0.2) + # stacked shares through time
        facet_wrap(~Measure, ncol = 2) + # capital on the left and risk on the right
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_fills() + # the crisp course fill colours
        labs(x = "", y = "Share of portfolio", title = Title, subtitle = Subtitle, caption = Caption, fill = "") # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "2 years", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1) # finishing touches, with date labels every two years and a percentage axis

}
