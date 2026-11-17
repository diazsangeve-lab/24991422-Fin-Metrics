# Plots monthly realised volatility as a calendar heatmap, with years down the side and months along the top.
Plot_Vol_Heatmap <- function(df, Cap = 0.4, Title = "Monthly Volatility Calendar", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !"Vol" %in% names(df)) stop("... for Plot_Vol_Heatmap to work, first run Monthly_Vol_Calc to create the 'Vol' column") # stop early if the monthly numbers are missing

    g <-
        df %>%
        mutate(Vol_Capped = pmin(Vol, Cap)) %>% # cap the colour scale so that the Covid months do not wash out every other month
        ggplot() + # start the plot
        geom_tile(aes(Month, factor(Year), fill = Vol_Capped), color = "white", linewidth = 0.4) + # one tile per month, coloured by how volatile it was
        facet_wrap(~Name, nrow = 1) + # one calendar per series, side by side
        scale_y_discrete(limits = rev) + # put the most recent year at the bottom so the calendar reads downwards in time
        scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = scales::percent_format(accuracy = 1), limits = c(0, Cap)) + # pale for calm months and dark red for volatile ones
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7), panel.grid = element_blank(), legend.key.width = unit(1.5, "cm")) + # vertical month labels and no grid lines behind the tiles and a wider legend so its labels do not overlap
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption, fill = "Annualised\nvolatility") # axis names, title and note

    g

}
