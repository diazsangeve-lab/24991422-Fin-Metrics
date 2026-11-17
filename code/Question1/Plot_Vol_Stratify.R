# Plots how volatile one index is in the calm and stressed months of each state variable as a dumbbell chart, with the gap between the two in a side column.
Plot_Vol_Stratify <- function(df, Index_Sel = "Capped SWIX", Title = "Volatility by Global Regime", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !Index_Sel %in% unique(df$Name) ) stop(glue::glue("... {Index_Sel} is not in the stratification results provided")) # stop early if the chosen index was not stratified

    D <-
        df %>%
        filter(Name == Index_Sel) %>% # keep only the chosen index
        select(State, Regime, Ratio) %>% # keep only what the plot needs
        spread(Regime, Ratio) %>% # one column for calm months and one for stressed months
        mutate(Gap = High - Low, State = forcats::fct_reorder(State, Gap)) # the distance between the two, used to order the rows

    Gap_X <- max(D$High) + 0.3 # where the gap column sits, to the right of the dumbbells and their labels
    Top <- levels(D$State)[length(levels(D$State))] # the top row, where the direct legend goes

    g <-
        D %>%
        ggplot() + # start the plot
        annotate("rect", xmin = Gap_X - 0.1, xmax = Gap_X + 0.1, ymin = -Inf, ymax = Inf, fill = "grey93") + # a shaded strip for the gap column
        geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") + # one marks normal, full sample volatility
        geom_segment(aes(x = Low, xend = High, y = State, yend = State), color = "grey80", linewidth = 3) + # the bar of each dumbbell
        geom_point(aes(Low, State), color = "steelblue", size = 5) + # calm months
        geom_point(aes(High, State), color = "firebrick", size = 5) + # stressed months
        geom_text(aes(Low - 0.035, State, label = sprintf("%.2f", Low)), color = "steelblue", hjust = 1, size = fmxdat::ggpts(8.5), fontface = "bold") + # the calm month ratio to the left of its point
        geom_text(aes(High + 0.035, State, label = sprintf("%.2f", High)), color = "firebrick", hjust = 0, size = fmxdat::ggpts(8.5), fontface = "bold") + # the stressed month ratio to the right of its point
        geom_text(aes(Gap_X, State, label = sprintf("+%.2f", Gap)), fontface = "bold", size = fmxdat::ggpts(9)) + # the gap between stressed and calm months
        annotate("text", x = D$Low[D$State == Top], y = Top, label = "Calm months", color = "steelblue", vjust = -1.4, fontface = "bold", size = fmxdat::ggpts(8)) + # direct legend for the calm point
        annotate("text", x = D$High[D$State == Top], y = Top, label = "Stressed months", color = "firebrick", vjust = -1.4, fontface = "bold", size = fmxdat::ggpts(8)) + # direct legend for the stressed point
        annotate("text", x = Gap_X, y = Top, label = "Gap", vjust = -1.4, fontface = "bold", size = fmxdat::ggpts(8)) + # header for the gap column
        scale_y_discrete(expand = expansion(add = c(0.6, 1))) + # leave room above the top row for the direct legend
        scale_x_continuous(expand = expansion(mult = c(0.12, 0.02))) + # leave room on the left for the calm month labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Volatility relative to full sample (1 = normal)", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g) # finishing touches

}
