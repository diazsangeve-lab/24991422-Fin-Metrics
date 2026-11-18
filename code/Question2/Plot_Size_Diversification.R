# Plots the average constituent volatility against the index volatility for each size index, filling the gap between them, which is the risk removed by diversification.
Plot_Size_Diversification <- function(SV, Smooth_N = 3, Title = "Constituent and Index Volatility by Size", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Size_Diversification, with inputs SV and Smooth_N, plus the title, subtitle and caption text

    D <- # build D through the steps below
        SV %>% # start from the size volatility data
        arrange(date) %>% # order by date before smoothing
        group_by(Fund) %>% # smooth each index on its own
        mutate(across(c(Avg_Vol, Index_Vol), ~RcppRoll::roll_mean(., n = Smooth_N, fill = NA, align = "right"))) %>% # a short moving average so that month to month noise does not hide the pattern
        ungroup() %>% # remove the grouping again
        filter(!is.na(Avg_Vol)) # drop the first few months, where the average is not yet full

    Labels <- SV %>% group_by(Fund) %>% summarise(DR = mean(Div_Ratio), DR_Last = dplyr::last(Div_Ratio), y = max(D$Avg_Vol) * 1.22, x = min(D$date), .groups = "drop") # the average and latest diversification ratio of each index

    g <- # build the plot in layers and store it as g
        D %>% # start from D, built above
        ggplot() + # start the plot
        geom_ribbon(aes(date, ymin = Index_Vol, ymax = Avg_Vol), fill = "darkorange", alpha = 0.3) + # the gap between holding stocks on their own and holding the index
        geom_line(aes(date, Avg_Vol), color = "darkorange", linewidth = 0.9) + # the weighted average constituent volatility
        geom_line(aes(date, Index_Vol), color = "grey20", linewidth = 0.9) + # the volatility of the index
        geom_label(data = Labels, aes(x, y, label = glue::glue("Diversification ratio\naverage {sprintf('%.1f', DR)}x, latest {sprintf('%.1f', DR_Last)}x")), hjust = 0, vjust = 1, size = fmxdat::ggpts(7.5), label.size = 0, fill = "white", alpha = 0.85, lineheight = 0.9) + # the diversification ratio in the corner of each panel
        facet_wrap(~Fund, nrow = 1) + # one panel per index
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        scale_x_date(date_breaks = "3 years", date_labels = "%Y") + # date labels every three years so that the narrow panels stay readable
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Annualised volatility (orange = average constituent, black = index)", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g # return the finished plot

}
