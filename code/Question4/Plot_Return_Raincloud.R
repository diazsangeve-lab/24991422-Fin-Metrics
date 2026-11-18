# Plots the distribution of each portfolio rule's monthly returns as a raincloud, combining a half density, a narrow boxplot and the individual months as rain below.
Plot_Return_Raincloud <- function(Rets, Pal, Title = "Monthly Return Distributions", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Return_Raincloud, with inputs Rets and Pal, plus the title, subtitle and caption text

    D <- Rets %>% mutate(Strategy = factor(Strategy, levels = rev(names(Pal$Strategy)))) # the rules from top to bottom in a fixed order
    Worst <- D %>% group_by(Strategy) %>% slice_min(Returns, n = 1) %>% ungroup() # each rule's worst month, labelled

    ggplot(D, aes(Returns, Strategy, fill = Strategy, color = Strategy)) + # returns along the bottom and one row per rule
        geom_vline(xintercept = 0, color = "grey50", linetype = "dashed") + # a zero return
        ggdist::stat_halfeye(adjust = 0.6, scale = 0.5, justification = -0.25, .width = 0, point_colour = NA, alpha = 0.75) + # the half density above each row, scaled so that it stays within its row
        geom_boxplot(width = 0.14, outlier.shape = NA, alpha = 0.35, color = "grey20") + # a narrow boxplot on the row
        ggdist::stat_dots(side = "left", justification = 1.12, binwidth = 0.0022, dotsize = 1) + # the individual months as rain below the row
        geom_text(data = Worst, aes(Returns, Strategy, label = glue::glue("Worst month\n{format(date, '%b %Y')}, {scales::percent(Returns, accuracy = 0.1)}")), hjust = 0, vjust = -0.5, nudge_x = 0.002, size = fmxdat::ggpts(7), lineheight = 0.85, color = "grey25") + # the worst month of each rule, above the box
        scale_fill_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        scale_color_manual(values = Pal$Strategy, guide = "none") + # the same colours for the dots
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.08, 0.04))) + # returns as percentages, with room on the left for the labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Monthly return", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
