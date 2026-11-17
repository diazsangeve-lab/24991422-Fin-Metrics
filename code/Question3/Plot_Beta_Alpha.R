# Plots every fund's market beta against its annualised alpha, so that funds offering return beyond the market stand apart, with the client fund highlighted.
Plot_Beta_Alpha <- function(Stats, Fund_Label = "ABC Fund", Title = "Market Beta and Alpha, 2015 to 2025", Subtitle = "", Caption = "Note:\nCalculation own"){

    Peers <- Stats %>% filter(Group == "Peer") # the peer funds
    Fund <- Stats %>% filter(Group == Fund_Label) # the client fund
    Med_Beta <- median(Peers$Beta) # the typical peer's beta

    g <-
        ggplot() + # start the plot
        annotate("rect", xmin = -Inf, xmax = Med_Beta, ymin = 0, ymax = Inf, fill = "darkorange", alpha = 0.07) + # shade the zone of low market exposure and positive alpha
        geom_hline(yintercept = 0, color = "grey40") + # zero alpha
        geom_vline(xintercept = Med_Beta, linetype = "dashed", color = "grey40") + # the median peer beta
        geom_point(data = Peers, aes(Beta, Alpha), color = "steelblue", alpha = 0.55, size = 3) + # every peer fund
        geom_point(data = Fund, aes(Beta, Alpha), shape = 23, fill = "darkorange", color = "white", size = 8) + # the client fund
        geom_label(data = Fund, aes(Beta, Alpha, label = glue::glue("ABC Fund\nbeta {sprintf('%.2f', Beta)}, alpha {scales::percent(Alpha, accuracy = 0.1)}")), nudge_y = 0.012, color = "darkorange3", fontface = "bold", size = fmxdat::ggpts(9.5), linewidth = 0, fill = "white", alpha = 0.85, lineheight = 0.9) + # label the client fund
        annotate("text", x = min(Stats$Beta), y = max(Stats$Alpha), label = "Less market exposure,\nmore return of its own", hjust = 0, vjust = 1, color = "darkorange3", fontface = "italic", size = fmxdat::ggpts(9.5), lineheight = 0.9) + # name the shaded zone
        annotate("text", x = Med_Beta, y = min(Stats$Alpha), label = glue::glue(" Median peer beta {sprintf('%.2f', Med_Beta)}"), hjust = 0, vjust = 0, color = "grey30", size = fmxdat::ggpts(9)) + # label the median beta
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # alpha as a percentage
        Slide_Theme() + # the slide theme
        labs(x = "Market beta to the Capped SWIX", y = "Annualised alpha", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g

}
