# Plots monthly rank correlations between own and third party scores, with the average of each pair as a dashed line.
Plot_Score_Cor <- function(df, Title = "Do the Scores Agree?", Subtitle = "", Caption = "Note:\nCalculation own"){

    g <-
        df %>%
        group_by(Label) %>% # work out each pair's own average
        mutate(Avg = mean(Rho)) %>% # the average rank correlation of the pair
        ungroup() %>% # remove the grouping again
        ggplot() + # start the plot
        geom_hline(yintercept = 0, linetype = "dotted") + # zero means the two scores rank stocks unrelatedly
        geom_line(aes(date, Rho, color = Label), linewidth = 1, alpha = 0.85) + # the monthly rank correlation of each pair
        geom_line(aes(date, Avg, color = Label), linetype = "dashed", alpha = 0.7) + # the average of each pair
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_cols() + # the crisp course colours
        guides(color = guide_legend(ncol = 1)) + # stack the long legend labels
        labs(x = "", y = "Cross-sectional rank correlation", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T) # finishing touches, with yearly date labels

}
