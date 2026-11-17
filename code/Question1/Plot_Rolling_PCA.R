# Plots the share of constituent return variance explained by the first three principal components over time as stacked areas, so that periods in which one common factor dominates stand out.
Plot_Rolling_PCA <- function(Pca_df, Highlight_From = lubridate::ymd(20260101), Title = "How Much One Factor Explains", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <- Pca_df %>% select(date, `First component` = PC1, `Second component` = PC2, `Third component` = PC3) %>% gather(Component, Share, -date) %>% mutate(Component = factor(Component, levels = c("Third component", "Second component", "First component"))) # long format, with the first component at the bottom
    Peak <- Pca_df %>% slice_max(PC1, n = 1) # the month in which the first component explained the most

    ggplot(D) + # start the plot
        annotate("rect", xmin = Highlight_From, xmax = max(D$date), ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.08) + # shade 2026
        geom_area(aes(date, Share, fill = Component), color = "white", linewidth = 0.2) + # the stacked shares
        geom_label(data = Peak, aes(date, PC1, label = glue::glue("{format(date, '%b %Y')}: {scales::percent(PC1, accuracy = 1)}")), vjust = -0.4, size = fmxdat::ggpts(8), label.size = 0, fill = "white", color = "grey15") + # label the peak of the first component
        scale_fill_manual(values = c(`First component` = "#0B2545", `Second component` = "#5B8DB8", `Third component` = "#A9C6E0"), breaks = c("First component", "Second component", "Third component")) + # dark to light from the first component to the third
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) + # shares as percentages
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "bottom") + # the legend below the plot
        labs(x = "", y = "Share of return variance explained", fill = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
