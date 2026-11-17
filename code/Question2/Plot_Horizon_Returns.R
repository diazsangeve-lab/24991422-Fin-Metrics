# Plots returns over several trailing horizons for each index as grouped bars, so that the ranking of the indexes over short and long horizons can be compared.
Plot_Horizon_Returns <- function(Hz, Cols, Title = "Returns Over Different Horizons", Subtitle = "", Caption = "Note:\nCalculation own"){

    ggplot(Hz, aes(Horizon, Ret, fill = Name)) + # horizons along the bottom
        geom_hline(yintercept = 0, color = "grey40") + # zero return
        geom_col(position = position_dodge(width = 0.82), width = 0.78) + # one bar per index
        geom_text(aes(label = scales::percent(Ret, accuracy = 0.1), vjust = ifelse(Ret >= 0, -0.35, 1.25)), position = position_dodge(width = 0.82), size = fmxdat::ggpts(7), fontface = "bold", color = "grey15") + # the value at the end of each bar
        scale_fill_manual(values = Cols) + # a fixed colour per index
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.08, 0.12))) + # returns as percentages
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "top") + # the legend above the bars
        labs(x = "Trailing horizon to September 2026 (one year or less cumulative, longer annualised)", y = "Return", fill = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
