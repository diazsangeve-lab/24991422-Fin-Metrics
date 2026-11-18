# Plots the distribution of daily returns for each portfolio with its Value at Risk and Expected Shortfall marked by method, so that the size of the left tail can be compared.
Plot_VaR_ES <- function(df, Risk_df, Title = "Daily Tail Risk", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_VaR_ES, with inputs df and Risk_df, plus the title, subtitle and caption text

    Method_Cols <- c(Historical = "#B2182B", Gaussian = "#4C78A8", Modified = "#E08214") # one colour per method
    Lines <- Risk_df %>% mutate(x = -Loss) # the measures as points on the return axis
    Lab <- Risk_df %>% select(Name, Method, Type, Loss) %>% spread(Type, Loss) %>% group_by(Name) %>% summarise(Text = paste0(Method, "  VaR ", scales::percent(VaR, accuracy = 0.01), ", ES ", scales::percent(ES, accuracy = 0.01), collapse = "\n"), .groups = "drop") # a small table of the measures for each panel

    ggplot(df) + # start the plot
        geom_histogram(aes(Returns, after_stat(density)), bins = 120, fill = "grey75", color = NA) + # the distribution of daily returns
        geom_vline(data = Lines, aes(xintercept = x, color = Method, linetype = Type), linewidth = 0.9) + # each measure as a vertical line
        geom_label(data = Lab, aes(Inf, Inf, label = Text), hjust = 1.02, vjust = 1.1, size = fmxdat::ggpts(7.5), label.size = 0, fill = "white", color = "grey15", lineheight = 0.9) + # the values in the corner of each panel
        facet_wrap(~Name, ncol = 1) + # one panel per portfolio
        scale_color_manual(values = Method_Cols) + # the method colours
        scale_linetype_manual(values = c(VaR = "solid", ES = "dashed")) + # solid for VaR and dashed for ES
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.08, 0.08)) + # daily returns as percentages, trimmed to the central range
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "bottom") + # the legends below the panels
        labs(x = "Daily return", y = "Density", color = "", linetype = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
