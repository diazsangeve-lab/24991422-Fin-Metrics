# Plots the largest contributors to portfolio risk on one date, comparing each stock's weight with its share of risk.
Plot_Top_Risk <- function(df, Top_N = 12, Title = "Top Contributors to Portfolio Risk", Subtitle = "", Caption = "Note:\nCalculation own"){

    g <-
        df %>%
        arrange(desc(Risk_Share)) %>% # rank the stocks by their contribution to risk
        slice_head(n = Top_N) %>% # keep only the largest contributors
        mutate(Tickers = forcats::fct_reorder(glue::glue("{Tickers} ({coalesce(Sector, 'Unclassified')})"), Risk_Share)) %>% # label each stock with its sector and order the bars
        select(Tickers, `Share of capital` = Weight, `Share of risk` = Risk_Share) %>% # the two measures to compare
        gather(Measure, Share, -Tickers) %>% # long format for side by side bars
        ggplot() + # start the plot
        geom_bar(aes(Tickers, Share, fill = Measure), stat = "identity", position = "dodge") + # weight and risk share side by side for each stock
        coord_flip() + # horizontal bars, so that the labels are easy to read
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        scale_fill_manual(values = c("grey60", "firebrick")) + # grey for capital and red for risk
        labs(x = "", y = "Share of portfolio", title = Title, subtitle = Subtitle, caption = Caption, fill = "") # axis names, title and note

    fmxdat::finplot(g, y.pct = T, y.pct_acc = 1) # finishing touches, with a percentage axis

}
