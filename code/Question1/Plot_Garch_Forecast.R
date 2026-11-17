# Plots each series' recent GARCH volatility and its forecast path towards the long run level, so that it is clear whether elevated volatility is expected to persist.
Plot_Garch_Forecast <- function(Fc_df, Cols, Title = "GARCH Volatility Forecasts", Subtitle = "", Caption = "Note:\nCalculation own"){

    Start_Fc <- Fc_df %>% filter(Part == "Forecast") %>% pull(date) %>% min() # the first forecast day
    Lab <- Fc_df %>% group_by(Name) %>% summarise(Long_Run = dplyr::first(Long_Run), Half_Life = dplyr::first(Half_Life), Model = dplyr::first(Model), .groups = "drop") # the long run level and half life of each series

    ggplot(Fc_df) + # start the plot
        annotate("rect", xmin = Start_Fc, xmax = max(Fc_df$date), ymin = -Inf, ymax = Inf, fill = "grey92") + # shade the forecast horizon
        geom_hline(data = Lab, aes(yintercept = Long_Run, color = Name), linetype = "dashed", linewidth = 0.6) + # the long run level of each series
        geom_line(data = Fc_df %>% filter(Part == "Fitted"), aes(date, Sigma, color = Name), linewidth = 0.7) + # the recent fitted volatility
        geom_line(data = Fc_df %>% filter(Part == "Forecast"), aes(date, Sigma, color = Name), linewidth = 1.5) + # the forecast path
        geom_label(data = Lab, aes(Start_Fc, Inf, label = glue::glue("{Model}, long run {scales::percent(Long_Run, accuracy = 0.1)}, half life {round(Half_Life)} days")), hjust = -0.03, vjust = 1.3, size = fmxdat::ggpts(7.5), label.size = 0, fill = "white", color = "grey15") + # the model, long run level and half life in each panel
        facet_wrap(~Name, ncol = 1) + # one panel per series
        scale_color_manual(values = Cols, guide = "none") + # a fixed colour per series
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # quarterly date labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # vertical date labels
        labs(x = "", y = "Annualised conditional volatility", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
