# Plots the split of every fund's return variance into market, peer group and fund-specific parts as a sorted barcode of stacked bars, with the client fund marked.
Plot_Return_Drivers <- function(Drv, Fund_Label = "ABC Fund", Title = "What Drives Each Fund's Returns", Subtitle = "", Caption = "Note:\nCalculation own"){

    Part_Cols <- c("Moves with the market" = "steelblue", "Moves with other managers" = "#9ECAE9", "Unique to the fund" = "darkorange") # one colour per part

    Ordered <- Drv %>% arrange(desc(Market)) %>% mutate(Rank = row_number()) # funds sorted from most to least market driven
    Fund <- Ordered %>% filter(Group == Fund_Label) # the client fund's position
    Med <- Drv %>% filter(Group == "Peer") %>% summarise(across(c(Market, Shared, Unique), median)) # the typical peer's split

    D <-
        Ordered %>%
        select(Rank, Group, `Moves with the market` = Market, `Moves with other managers` = Shared, `Unique to the fund` = Unique) %>% # readable names for the three parts
        gather(Part, Share, -Rank, -Group) %>% # long format, one row per fund and part
        mutate(Part = factor(Part, levels = rev(names(Part_Cols)))) # market at the bottom of each bar

    g <-
        ggplot(D) + # start the plot
        geom_col(aes(Rank, Share, fill = Part, alpha = Group == Fund_Label), width = 1) + # one stacked bar per fund
        scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.55), guide = "none") + # the client fund at full strength and the peers faded
        annotate("segment", x = Fund$Rank, xend = Fund$Rank, y = 1.13, yend = 1.02, arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "black", linewidth = 1) + # an arrow down to the client fund
        annotate("text", x = Fund$Rank + 1, y = 1.16, label = glue::glue("ABC Fund: {scales::percent(Fund$Market, accuracy = 1)} market, {scales::percent(Fund$Unique, accuracy = 1)} unique"), hjust = 1, color = "darkorange3", fontface = "bold", size = fmxdat::ggpts(10)) + # the client fund's split, ending at its arrow
        annotate("text", x = 1, y = 1.16, label = glue::glue("Median peer: {scales::percent(Med$Market, accuracy = 1)} market, {scales::percent(Med$Unique, accuracy = 1)} unique"), hjust = 0, color = "steelblue4", fontface = "bold", size = fmxdat::ggpts(10)) + # the typical peer's split
        scale_fill_manual(values = Part_Cols, breaks = names(Part_Cols)) + # the part colours, in reading order
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, 0.25), expand = expansion(add = c(0, 0.03))) + # shares as percentages
        scale_x_continuous(expand = c(0.005, 0.005)) + # bars across the full width
        Slide_Theme() + # the slide theme
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom") + # no fund numbers along the bottom
        labs(x = glue::glue("{nrow(Drv)} funds with at least five years of returns, sorted from most to least market driven"), y = "Share of return variance", title = Title, subtitle = Subtitle, caption = Caption, fill = "") # axis names, title and note

    g

}
