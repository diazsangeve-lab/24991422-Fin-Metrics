# Plots every fund's correlation with the first two principal components of peer returns inside the unit circle, with the client fund highlighted, in the style of the variable plot from Practical 4.
Plot_Peer_PCA <- function(Pc, Fund_Label = "ABC Fund", Title = "A Map of the Peer Group", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Peer_PCA, with inputs Pc and Fund_Label, plus the title, subtitle and caption text

    Circle <- tibble(a = seq(0, 2 * pi, length.out = 200), x = cos(a), y = sin(a)) # the unit circle
    Fund <- Pc %>% filter(Name == Fund_Label) # the client fund

    ggplot(Pc, aes(PC1, PC2)) + # first component along the bottom and second up the side
        geom_path(data = Circle, aes(x, y), color = "grey70") + # the unit circle, the largest possible correlation
        geom_hline(yintercept = 0, color = "grey80") + geom_vline(xintercept = 0, color = "grey80") + # the axes through the origin
        geom_segment(data = Fund, aes(0, 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "darkorange", linewidth = 1.1) + # an arrow out to the client fund
        geom_point(data = Pc %>% filter(Name != Fund_Label), color = "steelblue", alpha = 0.6, size = 2.6) + # every peer fund
        geom_point(data = Fund, shape = 23, size = 6, fill = "darkorange", color = "black") + # the client fund
        geom_label(data = Fund, aes(label = glue::glue("ABC Fund\nPC1 {sprintf('%.2f', PC1)}, PC2 {sprintf('%.2f', PC2)}")), hjust = 1.12, vjust = 0.5, size = fmxdat::ggpts(8.5), fontface = "bold", color = "#8C3B00", fill = "white", label.size = 0, lineheight = 0.9) + # label the client fund
        coord_equal(xlim = c(-0.05, 1.05), ylim = c(-0.6, 0.6)) + # equal scales, focused on where the funds sit
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = glue::glue("First component ({scales::percent(Pc$Share1[1], accuracy = 1)} of variance)"), y = glue::glue("Second component ({scales::percent(Pc$Share2[1], accuracy = 1)})"), title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
