# Plots the effective number of stocks in each index through time with inline labels, and shades the period in which two indexes overlap.
Plot_Size_Concentration <- function(Conc, Overlap_From = NULL, Overlap_Y = 20, Fund_Cols = c(`Capped SWIX` = "black", `Large Caps` = "#B2182B", `Mid Caps` = "darkorange", `Small Caps` = "#4C78A8"), Title = "Stock Concentration by Size Index", Subtitle = "", Caption = "Note:\nCalculation own"){

    Ends <- Conc %>% filter(date == max(date)) # the last month, where the line labels go
    Breaks <- seq(lubridate::floor_date(min(Conc$date), "year"), max(Conc$date), by = "2 years") # date labels every two years that stop at the last month, so that the label space has no empty years

    g <- # build the plot in layers and store it as g
        Conc %>% # start from Conc
        ggplot() + # start the plot
        geom_line(aes(date, Eff_N, color = Fund), linewidth = 1) + # the effective number of stocks in each index
        geom_text(data = Ends, aes(date, Eff_N, label = glue::glue("{Fund} ({round(Eff_N)})"), color = Fund), hjust = -0.08, fontface = "bold", size = fmxdat::ggpts(8.5)) + # label each line with its latest value
        scale_color_manual(values = Fund_Cols) + # a fixed colour per index
        scale_x_date(expand = expansion(mult = c(0.01, 0.2)), breaks = Breaks, date_labels = "%Y") + # room on the right for the labels
        guides(color = "none") + # the lines are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Effective number of stocks (1 / HHI)", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    if( !is.null(Overlap_From) ) g <- g + annotate("rect", xmin = Overlap_From, xmax = max(Conc$date), ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.08) + annotate("text", x = Overlap_From, y = Overlap_Y, label = "Mid caps begin to share\nstocks with large caps", hjust = -0.05, vjust = 0.5, size = fmxdat::ggpts(8), color = "#8C3B00", fontface = "italic") # shade and explain the overlap period

    fmxdat::finplot(g, x.vert = T) # finishing touches, with vertical date labels

}
