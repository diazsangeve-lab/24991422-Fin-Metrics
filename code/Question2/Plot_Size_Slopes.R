# Plots stratified returns and volatility as slopegraphs, with each index's line running from calm to stressed months of every state variable.
Plot_Size_Slopes <- function(Strat, Fund_Cols = c(`Large Caps` = "#B2182B", `Mid Caps` = "darkorange", `Small Caps` = "#4C78A8"), Title = "Size Indexes in Calm and Stressed Regimes", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <- # build D through the steps below
        Strat %>% # start from Strat
        select(State, Regime, Name, `Annualised return` = Ret_Ann, `Annualised volatility` = Vol_Ann) %>% # the two measures shown
        gather(Measure, Value, `Annualised return`, `Annualised volatility`) %>% # long format, one row per measure
        mutate(Regime = factor(ifelse(Regime == "Low", "Low", "High"), levels = c("Low", "High"))) # low months on the left and high months on the right

    Ends <- D %>% filter(Regime == "High") # the right hand end of each line, where the labels go

    g <- # build the plot in layers and store it as g
        D %>% # start from D, built above
        ggplot(aes(Regime, Value, group = Name, color = Name)) + # regimes along the bottom and one line per index
        geom_hline(data = tibble(Measure = "Annualised return"), aes(yintercept = 0), color = "grey50", linetype = "dashed") + # zero return in the return panels
        geom_line(linewidth = 1.1) + # the slope from low to high months
        geom_point(size = 2.6) + # the two ends of each slope
        ggrepel::geom_text_repel(data = Ends, aes(label = scales::percent(Value, accuracy = 1)), hjust = 0, nudge_x = 0.12, direction = "y", size = fmxdat::ggpts(7.5), fontface = "bold", segment.color = NA, show.legend = FALSE) + # the value at the high end, without overlaps
        facet_grid(Measure ~ State, scales = "free_y") + # rows for the two measures and columns for the state variables
        scale_color_manual(values = Fund_Cols) + # a fixed colour per index
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # values as percentages
        scale_x_discrete(expand = expansion(add = c(0.3, 0.6))) + # room on the right for the labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Months in the bottom (Low) and top (High) quintile of each state variable", y = "", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    g # return the finished plot

}
