# Plots the client fund's standing against its peers on several measures as bullet bars, each showing the share of peers the fund beats, with the fund's value and the peer median printed beside it.
Plot_Peer_Scorecard <- function(Stats, Fund_Label = "ABC Fund", Title = "ABC Fund Against Its Peers, 2015 to 2025", Subtitle = "", Caption = "Note:\nCalculation own"){

    Metrics <- tibble(Col = c("Ret", "Excess", "IR", "Vol", "MaxDD", "Beta"), # the measures shown
                      Label = c("Annualised return", "Return above the benchmark", "Information ratio", "Volatility", "Maximum drawdown", "Market beta"), # readable names
                      Higher_Better = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), # whether a higher value is better for the client
                      Fmt = c("pct", "pct", "num", "pct", "pct", "num")) # how each value is printed

    D <-
        Metrics %>%
        mutate(Fund = map_dbl(Col, ~Stats[[.x]][Stats$Group == Fund_Label]), # the fund's value on each measure
               Peer_Median = map_dbl(Col, ~median(Stats[[.x]][Stats$Group == "Peer"])), # the typical peer's value
               Beat = pmap_dbl(list(Col, Fund, Higher_Better), function(c, f, h) mean(if(h) Stats[[c]][Stats$Group == "Peer"] < f else Stats[[c]][Stats$Group == "Peer"] > f)), # the share of peers the fund beats, allowing for whether higher or lower is better
               Show = ifelse(Fmt == "pct", glue::glue("{scales::percent(Fund, accuracy = 0.1)} vs {scales::percent(Peer_Median, accuracy = 0.1)}"), glue::glue("{sprintf('%.2f', Fund)} vs {sprintf('%.2f', Peer_Median)}")), # the fund against the median peer
               Label = factor(Label, levels = rev(Label))) # first measure at the top

    Bands <- tibble(xmin = c(0, 0.25, 0.5, 0.75), xmax = c(0.25, 0.5, 0.75, 1), Shade = c("grey94", "grey89", "grey84", "grey78")) # quartile bands behind each bar

    g <-
        ggplot(D) + # start the plot
        geom_rect(data = tidyr::crossing(Bands, Label = D$Label), aes(xmin = xmin, xmax = xmax, ymin = as.numeric(Label) - 0.32, ymax = as.numeric(Label) + 0.32, fill = Shade)) + # the quartile bands for every measure
        scale_fill_identity() + # use the band colours as given
        geom_segment(aes(x = 0, xend = Beat, y = Label, yend = Label), color = "darkorange", linewidth = 5) + # a bar out to the share of peers beaten
        geom_point(aes(Beat, Label), color = "darkorange3", size = 6) + # the end of each bar
        geom_text(aes(Beat, Label, label = scales::percent(Beat, accuracy = 1)), color = "white", size = fmxdat::ggpts(7), fontface = "bold") + # the share inside the end point
        geom_text(aes(1.03, Label, label = Show), hjust = 0, size = fmxdat::ggpts(10)) + # the fund's value against the median peer
        geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40") + # the median peer
        annotate("text", x = 1.03, y = nrow(D) + 0.6, label = "ABC vs median peer", hjust = 0, fontface = "bold", size = fmxdat::ggpts(10)) + # header for the value column
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = expansion(add = c(0.01, 0.42))) + # share of peers beaten, with room for the values
        scale_y_discrete(expand = expansion(add = c(0.5, 0.9))) + # room above the top bar for the header
        Slide_Theme() + # the slide theme
        labs(x = "Share of peers ABC Fund beats", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g

}
