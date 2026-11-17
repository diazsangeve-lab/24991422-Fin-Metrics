# Plots the share of active managers beating the benchmark and the spread of their excess returns in each dispersion regime, following the dispersion of active managers practical.
Plot_Dispersion_Regimes <- function(Opp, N_Groups = 5, Title = "Manager Outcomes by Dispersion Regime", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <-
        Opp %>%
        tidyr::drop_na(Disp12, Share_Outperform, Mgr_Spread) %>% # only months with all three measures
        mutate(Regime = ntile(Disp12, N_Groups)) %>% # sort the months into groups by trailing dispersion
        group_by(Regime) %>% # one bar per group
        summarise(Disp = mean(Disp12), `Share of managers beating the Capped SWIX` = mean(Share_Outperform), `Spread of manager excess returns` = mean(Mgr_Spread), Months = n(), .groups = "drop") %>% # the averages in each group
        mutate(Label = glue::glue("Q{Regime}\n{scales::percent(Disp, accuracy = 0.1)}")) %>% # the group and its average dispersion
        gather(Measure, Value, `Share of managers beating the Capped SWIX`, `Spread of manager excess returns`) # long format, one row per measure

    ggplot(D, aes(Label, Value, fill = Measure)) + # dispersion groups along the bottom
        geom_col(width = 0.7) + # one bar per group
        geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), vjust = -0.4, size = fmxdat::ggpts(8.5), fontface = "bold", color = "grey15") + # the value above each bar
        geom_hline(data = tibble(Measure = "Share of managers beating the Capped SWIX", y = 0.5), aes(yintercept = y), linetype = "dashed", color = "grey30") + # half of managers, for reference
        facet_wrap(~Measure, scales = "free_y") + # one panel per measure
        scale_fill_manual(values = c(`Share of managers beating the Capped SWIX` = "#4C78A8", `Spread of manager excess returns` = "darkorange"), guide = "none") + # one colour per measure
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) + # values as percentages
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Quintile of trailing 12 month weighted dispersion (average dispersion)", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
