# Plots stratified volatility for several indexes and state variables as a grid of bubbles, sized by the volatility level and coloured by the ratio to normal.
Plot_Strat_Heatmap <- function(df, Title = "Stratified Volatility Across Local Indexes", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Strat_Heatmap, with input df, plus the title, subtitle and caption text

    if( !all(c("Name", "State", "Regime", "SD", "Ratio") %in% names(df)) ) stop("... for Plot_Strat_Heatmap to work, first run Vol_Stratify") # stop early if the stratification results are missing

    g <- # build the plot in layers and store it as g
        df %>% # start from the input data
        mutate(Regime = factor(glue::glue("{Regime} regime months"), levels = c("High regime months", "Low regime months"))) %>% # readable panel labels in a fixed order
        mutate(State = forcats::fct_reorder(State, Ratio, .fun = max)) %>% # order the state variables from the weakest to the strongest effect
        ggplot() + # start the plot
        geom_point(aes(Name, State, size = SD, fill = Ratio), shape = 21, color = "white", stroke = 0.8) + # one bubble per index and state variable, sized by the volatility level
        geom_text(aes(Name, State, label = scales::percent(SD, accuracy = 1), color = abs(Ratio - 1) > 0.25), size = fmxdat::ggpts(8), fontface = "bold") + # the volatility level inside each bubble
        scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"), guide = "none") + # white text on strongly coloured bubbles and dark text on pale ones
        facet_wrap(~Regime) + # high and low regimes side by side
        scale_size_area(max_size = 17, guide = "none") + # bubble area in proportion to volatility
        scale_fill_gradient2(low = "steelblue", mid = "grey92", high = "firebrick", midpoint = 1) + # blue for calmer than normal, grey for normal and red for more volatile than normal
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.key.width = unit(1.5, "cm")) + # a wider legend
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption, fill = "Volatility relative\nto full sample") # axis names, title and note

    fmxdat::finplot(g, x.vert = T) # finishing touches, with vertical index labels

}
