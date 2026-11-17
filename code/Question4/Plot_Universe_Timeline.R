# Plots the data history of every asset as a timeline, marking when each becomes eligible under the five year rule, the estimation window and the backtest window, with excluded assets shown faded.
Plot_Universe_Timeline <- function(R_Long, Meta, Pal, Excluded = NULL, Est_Start = lubridate::ymd(20100101), Rebal_Dates, Min_Months = 60, Title = "The Investable Universe Through Time", Subtitle = "", Caption = "Note:\nCalculation own"){

    Spans <-
        R_Long %>%
        group_by(Asset) %>% # one row per asset
        summarise(Start = min(date), End = max(date), Eligible = sort(date)[Min_Months], .groups = "drop") %>% # the first and last return, and the month the five year rule is first met
        bind_rows(Excluded) %>% # assets left out, with their reason
        mutate(Family = ifelse(is.na(Pal$Asset_Family[Asset]), "Excluded", Pal$Asset_Family[Asset]), # each asset's family
               Col = ifelse(is.na(Pal$Asset[Asset]), "grey70", Pal$Asset[Asset]), # each asset's colour
               Asset = factor(Asset, levels = rev(c(names(Pal$Asset), Excluded$Asset)))) # assets listed from the top in family order

    Win <- tibble(xmin = c(Est_Start, min(Rebal_Dates)), xmax = c(min(Rebal_Dates), max(R_Long$date)), Lab = c("Estimation window only", "Quarterly rebalanced backtest"), Fill = c("grey85", "#FDD0A2")) # the two windows behind the timeline

    g <-
        ggplot(Spans) + # start the plot
        geom_rect(data = Win, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Fill), alpha = 0.45) + # shade the estimation and backtest windows
        scale_fill_identity() + # use the window colours as given
        geom_text(data = Win, aes(x = xmin + (xmax - xmin) / 2, y = length(levels(Spans$Asset)) + 0.9, label = Lab), fontface = "bold", size = fmxdat::ggpts(9), color = "grey25") + # name each window above the rows
        geom_segment(aes(x = Start, xend = End, y = Asset, yend = Asset, color = Col), linewidth = 2.2, alpha = 0.35, lineend = "round") + # the full data history, faded
        geom_segment(data = Spans %>% filter(!is.na(Eligible)), aes(x = pmax(Eligible, Start), xend = End, y = Asset, yend = Asset, color = Col), linewidth = 4.5, lineend = "round") + # the period in which the asset may be held
        geom_point(data = Spans %>% filter(!is.na(Eligible)), aes(Eligible, Asset), shape = 21, fill = "white", color = "black", size = 3.2, stroke = 1) + # the month the five year rule is met
        geom_text(data = Spans %>% filter(Family == "Excluded"), aes(End, Asset, label = Reason), hjust = 1.03, vjust = -0.9, size = fmxdat::ggpts(8), fontface = "italic", color = "grey40") + # the reason an asset is excluded
        geom_vline(xintercept = Est_Start, linetype = "dashed", color = "grey30") + # the start of the estimation window
        scale_color_identity() + # use the asset colours as given
        scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.02))) + # dates along the bottom
        scale_y_discrete(expand = expansion(add = c(0.6, 1.6))) + # room above the rows for the window labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(axis.text.y = element_text(color = Darken_Col(Spans$Col[match(levels(Spans$Asset), Spans$Asset)], 0.5), face = "bold")) + # colour each asset name by its family, in the order the axis lists them
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g

}
