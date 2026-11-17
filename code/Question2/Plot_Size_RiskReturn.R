# Plots each index's annualised risk and return in successive sub-periods as a connected scatter, one panel per index with the other indexes in grey behind it.
Plot_Size_RiskReturn <- function(Period_Stats, Fund_Cols = c(`Capped SWIX` = "black", `Large Caps` = "#B2182B", `Mid Caps` = "darkorange", `Small Caps` = "#4C78A8"), Title = "Risk and Return by Sub-Period", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("Name", "Period", "Ret", "Vol", "MDD") %in% names(Period_Stats)) ) stop("... for Plot_Size_RiskReturn to work, first run Size_Period_Calc") # stop early if the sub-period statistics are missing

    Background <- Period_Stats %>% rename(Other = Name) # a copy without the panel variable, so that every index appears in grey in every panel

    g <-
        Period_Stats %>%
        ggplot(aes(Vol, Ret)) + # volatility along the bottom and return up the side
        geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") + # zero return
        geom_path(data = Background, aes(group = Other), color = "grey85", linewidth = 0.6) + # the paths of all indexes in grey, for comparison
        geom_path(aes(color = Name), arrow = arrow(length = unit(0.22, "cm"), type = "closed"), linewidth = 1) + # the highlighted index, with arrows in time order
        geom_point(aes(color = Name, size = MDD), alpha = 0.9) + # one point per sub-period, sized by the maximum drawdown
        ggrepel::geom_label_repel(aes(label = Period), color = "grey15", fill = "white", label.size = 0, label.padding = 0.12, size = fmxdat::ggpts(7.5), fontface = "bold", show.legend = FALSE, point.padding = 0.6, box.padding = 0.5, min.segment.length = 0, seed = 1) + # label each point with its sub-period in dark text on a white box, kept clear of the points
        facet_wrap(~Name, nrow = 1) + # one panel per index
        scale_color_manual(values = Fund_Cols) + # a fixed colour per index
        scale_size_continuous(range = c(2, 7), labels = scales::percent_format(accuracy = 1)) + # larger points for deeper drawdowns
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # return as a percentage
        guides(color = "none") + # the panels are named, so no colour legend is needed
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Annualised volatility", y = "Annualised return", title = Title, subtitle = Subtitle, caption = Caption, size = "Maximum drawdown") # axis names, title and note

    g

}
