# Plots monthly rank correlations between own and vendor scores with inline labels, next to a heatmap of what each vendor factor tracks on average.
Plot_Score_Cor <- function(df, Diag = NULL, Title = "Do the Scores Agree?", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Score_Cor, with inputs df and Diag, plus the title, subtitle and caption text

    Ends <- df %>% group_by(Label) %>% filter(date == max(date)) %>% ungroup() # the last month of each pair, where the inline labels go

    g_main <- # build this panel of the figure in layers and store it as g_main
        df %>% # start from the input data
        group_by(Label) %>% # work out each pair's own average
        mutate(Avg = mean(Rho)) %>% # the average rank correlation of the pair
        ungroup() %>% # remove the grouping again
        ggplot() + # start the main panel
        geom_hline(yintercept = 0, linetype = "dotted") + # zero means the two scores rank stocks unrelatedly
        geom_line(aes(date, Avg, color = Label), linetype = "dashed", alpha = 0.6) + # the average of each pair
        geom_line(aes(date, Rho, color = Label), linewidth = 1, alpha = 0.9) + # the monthly rank correlation of each pair
        geom_text(data = Ends, aes(date, Rho, label = stringr::str_wrap(Label, 18), color = Label), hjust = -0.08, lineheight = 0.85, fontface = "bold", size = fmxdat::ggpts(7.5)) + # label each line at its end instead of using a legend
        scale_x_date(breaks = seq(lubridate::floor_date(min(df$date), "year"), max(df$date), by = "year"), date_labels = "%Y", expand = expansion(mult = c(0.01, 0.32))) + # one tick per year where there is data, and room on the right for the inline labels
        scale_color_manual(values = c("#1F3A6B", "#B5451B", "#1E7B3A")) + # dark, distinct colours that stay legible as text on white
        guides(color = "none") + # the lines are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22)) + # the clean course theme
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # vertical date labels
        labs(x = "", y = "Cross-sectional rank correlation") # axis names

    if( is.null(Diag) ) return(g_main + labs(title = Title, subtitle = Subtitle, caption = Caption)) # without diagnostics the main panel is the whole figure

    g_diag <- # build this panel of the figure in layers and store it as g_diag
        Diag %>% # start from Diag
        ggplot() + # start the diagnostic panel
        geom_tile(aes(Vendor, Own, fill = Rho), color = "white", linewidth = 0.6) + # one tile per own measure and vendor factor
        geom_text(aes(Vendor, Own, label = sprintf("%.2f", Rho)), size = fmxdat::ggpts(7.5), fontface = "bold") + # print the average rank correlation in each tile
        scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkorange", midpoint = 0, limits = c(-1, 1), guide = "none") + # blue for negative, white for none and orange for positive agreement
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(24), subtitle.size = fmxdat::ggpts(20), caption.size = fmxdat::ggpts(22)) + # the clean course theme
        theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text = element_text(size = 8)) + # no grid and vertical vendor labels
        labs(x = "", y = "", title = "What the vendor scores track", subtitle = "Average rank correlation") # a small title for the panel

    patchwork::wrap_plots(g_main, g_diag, nrow = 1, widths = c(2.1, 1)) + # the time series on the left and the diagnostic heatmap on the right
        patchwork::plot_annotation(title = Title, subtitle = Subtitle, caption = Caption, # one title for the combined figure
                                   theme = fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T)) # the course theme for the shared title

}
