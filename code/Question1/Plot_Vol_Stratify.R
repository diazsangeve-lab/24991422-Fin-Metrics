# Plots how much more or less volatile one index is than normal in the high and low regimes of each state variable.
Plot_Vol_Stratify <- function(df, Index_Sel = "Capped SWIX", Title = "Volatility by Global Regime", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !Index_Sel %in% unique(df$Name) ) stop(glue::glue("... {Index_Sel} is not in the stratification results provided")) # stop early if the chosen index was not stratified

    g <-
        df %>%
        filter(Name == Index_Sel) %>% # keep only the chosen index
        mutate(State = forcats::fct_reorder(State, Ratio, .fun = max)) %>% # order the state variables from the weakest to the strongest effect
        ggplot() + # start the plot
        geom_bar(aes(State, Ratio, fill = Regime), stat = "identity", position = "dodge") + # one bar per regime, side by side
        geom_hline(yintercept = 1, linetype = "dashed") + # the line at one marks normal, full sample volatility
        geom_label(aes(State, Ratio, label = round(Ratio, 2), group = Regime), position = position_dodge(width = 0.9), size = fmxdat::ggpts(8), alpha = 0.6) + # print the ratio on each bar
        coord_flip() + # horizontal bars, so that the state names are easy to read
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_fills() + # the crisp course fill colours
        labs(x = "", y = "Volatility relative to full sample (1 = normal)", title = Title, subtitle = Subtitle, caption = Caption, fill = "Regime of state variable") # axis names, title and note

    fmxdat::finplot(g) # finishing touches

}
