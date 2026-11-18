# Plots the largest contributors to portfolio risk as arrows running from each stock's share of capital to its share of risk.
Plot_Top_Risk <- function(df, Top_N = 12, Title = "Top Contributors to Portfolio Risk", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Top_Risk, with inputs df and Top_N, plus the title, subtitle and caption text

    D <- # build D through the steps below
        df %>% # start from the input data
        arrange(desc(Risk_Share)) %>% # rank the stocks by their contribution to risk
        slice_head(n = Top_N) %>% # keep only the largest contributors
        mutate(Label = forcats::fct_reorder(glue::glue("{Tickers} ({coalesce(Sector, 'Unclassified')})"), Risk_Share), # label each stock with its sector and order the rows
               Multiple = Risk_Share / Weight, # how many times its weight the stock contributes in risk
               Type = ifelse(Risk_Share > Weight, "Amplifies risk", "Dilutes risk")) # whether the stock adds more risk than capital

    X_Text <- max(c(D$Risk_Share, D$Weight)) + 0.03 # where the multiple column sits

    g <- # build the plot in layers and store it as g
        D %>% # start from D, built above
        ggplot() + # start the plot
        geom_segment(aes(x = Weight, xend = Risk_Share, y = Label, yend = Label, color = Type), linewidth = 1.1, arrow = arrow(length = unit(0.18, "cm"), type = "closed")) + # an arrow from the share of capital to the share of risk
        geom_point(aes(Weight, Label), color = "grey45", size = 2.5) + # the share of capital at the start of each arrow
        geom_text(aes(X_Text, Label, label = sprintf("%.1fx", Multiple), color = Type), hjust = 0, fontface = "bold", size = fmxdat::ggpts(8.5), show.legend = FALSE) + # the risk multiple in a column on the right
        annotate("text", x = X_Text, y = Top_N + 0.7, label = "Risk / capital", hjust = 0, fontface = "bold", size = fmxdat::ggpts(8)) + # header for the multiple column
        scale_color_manual(values = c(`Amplifies risk` = "firebrick", `Dilutes risk` = "steelblue")) + # red for stocks that add more risk than capital and blue for the rest
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.02, 0.15))) + # percentage axis with room for the multiple column
        scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) + # room above the top row for the header
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Grey dot = share of capital, arrow head = share of risk", y = "", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    fmxdat::finplot(g) # finishing touches

}
