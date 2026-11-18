# Plots each sector's share of capital above the axis and its share of risk below it, as a mirrored stacked area with inline sector labels.
Plot_Risk_vs_Weight <- function(df, Sector_Order = c("Financials", "Industrials", "Property", "Unclassified", "Resources"), Sector_Cols = c(Financials = "#4C78A8", Industrials = "#9ECAE9", Property = "grey70", Unclassified = "#E8998D", Resources = "#B2182B"), Title = "Where the Capital Sits and Where the Risk Comes From", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("date", "Sector", "Weight", "Risk_Share") %in% names(df)) ) stop("... for Plot_Risk_vs_Weight to work, first run Risk_Contribution_Calc") # stop early if the risk decomposition is missing

    Shares <- # build Shares through the steps below
        df %>% # start from the input data
        mutate(Sector = coalesce(Sector, "Unclassified")) %>% # give stocks with no sector a label
        group_by(date, Sector) %>% # aggregate the stocks into sectors
        summarise(Capital = sum(Weight), Risk = sum(Risk_Share), .groups = "drop") %>% # the sector's weight and its share of portfolio volatility
        tidyr::complete(date, Sector, fill = list(Capital = 0, Risk = 0)) %>% # give a sector a zero on dates when it is not held, so that the stacked areas have no gaps
        mutate(Sector = factor(Sector, levels = Sector_Order)) # resources last, so that they sit against the zero line on both sides

    Labels <- # build Labels through the steps below
        Shares %>% # start from Shares, built above
        filter(date == max(date)) %>% # the latest date, where the labels go
        arrange(desc(Sector)) %>% # the stacking order from the zero line outwards
        mutate(Pos = cumsum(Capital) - Capital / 2) %>% # the middle of each sector's band on the capital side
        filter(Capital > 0.03) # skip slivers that are too thin to label

    g <- # build the plot in layers and store it as g
        Shares %>% # start from Shares, built above
        ggplot() + # start the plot
        geom_area(aes(date, Capital, fill = Sector), alpha = 0.9, color = "white", linewidth = 0.2) + # capital shares stacked upwards
        geom_area(aes(date, -Risk, fill = Sector), alpha = 0.9, color = "white", linewidth = 0.2) + # risk shares stacked downwards, as a mirror image
        geom_hline(yintercept = 0, color = "black", linewidth = 0.6) + # the axis between capital and risk
        annotate("text", x = min(Shares$date), y = 0.95, label = "SHARE OF CAPITAL", hjust = 0, fontface = "bold", color = "white", size = fmxdat::ggpts(9)) + # name the upper half
        annotate("text", x = min(Shares$date), y = -0.95, label = "SHARE OF RISK", hjust = 0, fontface = "bold", color = "white", size = fmxdat::ggpts(9)) + # name the lower half
        geom_text(data = Labels, aes(date, Pos, label = Sector, color = Sector), hjust = -0.1, fontface = "bold", size = fmxdat::ggpts(8.5)) + # label each sector to the right of its band
        scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1), breaks = seq(-1, 1, 0.25)) + # percentages on both sides of the axis
        scale_x_date(expand = expansion(mult = c(0.01, 0.16)), date_breaks = "2 years", date_labels = "%Y") + # room on the right for the sector labels
        scale_fill_manual(values = Sector_Cols) + # a palette that makes resources, the story of the chart, stand out against muted blues and greys
        scale_color_manual(values = Darken_Col(Sector_Cols, 0.35)) + # darker versions of the sector colours for the inline labels, so that the light shades stay legible on white
        guides(fill = "none", color = "none") + # the sectors are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g # return the finished plot

}
