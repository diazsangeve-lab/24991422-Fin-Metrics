# Plots stratified volatility for several indexes and state variables as a labelled heatmap, coloured by the volatility level and annotated with the ratio to normal.
Plot_Strat_Heatmap <- function(df, Title = "Stratified Volatility Across Local Indexes", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("Name", "State", "Regime", "SD", "Ratio") %in% names(df)) ) stop("... for Plot_Strat_Heatmap to work, first run Vol_Stratify") # stop early if the stratification results are missing

    g <-
        df %>%
        mutate(Regime = factor(glue::glue("{Regime} regime months"), levels = c("High regime months", "Low regime months"))) %>% # readable panel labels in a fixed order
        mutate(State = forcats::fct_reorder(State, Ratio, .fun = max)) %>% # order the state variables from the weakest to the strongest effect
        mutate(Label = glue::glue("{scales::percent(SD, accuracy = 0.1)}\n({sprintf('%.2f', Ratio)}x)")) %>% # the volatility level on top and the ratio to normal underneath
        ggplot() + # start the plot
        geom_tile(aes(Name, State, fill = SD), color = "white", linewidth = 0.6) + # one tile per index and state variable, coloured by how volatile the index was
        geom_text(aes(Name, State, label = Label), size = fmxdat::ggpts(8), lineheight = 0.9) + # print the level and the ratio inside each tile
        facet_wrap(~Regime) + # high and low regimes side by side
        scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = scales::percent_format(accuracy = 1)) + # pale for calm and dark red for volatile
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(panel.grid = element_blank(), legend.key.width = unit(1.5, "cm")) + # no grid lines behind the tiles and a wider legend so its labels do not overlap
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption, fill = "Annualised\nvolatility") # axis names, title and note

    fmxdat::finplot(g, x.vert = T) # finishing touches, with vertical index labels

}
