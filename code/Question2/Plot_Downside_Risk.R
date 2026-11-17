# Plots downside risk measures for each index as dot plots, one panel per measure, so that the indexes can be ranked on every kind of loss.
Plot_Downside_Risk <- function(Dr, Cols, Title = "Downside Risk by Size Index", Subtitle = "", Caption = "Note:\nCalculation own"){

    ggplot(Dr, aes(Value, Name, color = Name)) + # loss along the bottom and one row per index
        geom_segment(aes(x = 0, xend = Value, y = Name, yend = Name), linewidth = 1, alpha = 0.5) + # a stem from zero to each value
        geom_point(size = 4) + # the value of each measure
        geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), hjust = -0.35, size = fmxdat::ggpts(7.5), fontface = "bold", color = "grey15") + # the value beside each point
        facet_wrap(~Measure, scales = "free_x", ncol = 3) + # one panel per measure
        scale_color_manual(values = Cols, guide = "none") + # a fixed colour per index
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.35))) + # losses as percentages, with room for the labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(panel.spacing.x = unit(1.2, "cm")) + # more space between panels so that their axis labels do not touch
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
