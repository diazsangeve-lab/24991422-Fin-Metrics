# Plots the rolling betas of the peer group as boxplots laid over the individual fund points for each year, with the client fund's average rolling beta drawn across the years.
Plot_Roll_Beta_Box <- function(RB, Fund_Label = "ABC Fund", Min_Months = 6, Title = "Rolling 3 Year Beta to the Capped SWIX", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Roll_Beta_Box, with inputs RB, Fund_Label and Min_Months, plus the title, subtitle and caption text

    D <- # build D through the steps below
        RB %>% # start from RB
        mutate(Year = lubridate::year(date)) %>% # the year of each rolling estimate
        group_by(Year) %>% # work year by year
        filter(n_distinct(date) >= Min_Months) %>% # drop years with only a few months of full windows, such as the first year
        ungroup() %>% # remove the grouping again
        mutate(Year = factor(Year)) # years as categories for the boxplots
    Fund_Yr <- D %>% filter(Group == Fund_Label) %>% group_by(Year) %>% summarise(Beta = mean(Beta), .groups = "drop") # the fund's average rolling beta in each year
    Peer_Yr <- D %>% filter(Group == "Peer") %>% group_by(Year) %>% summarise(Med = median(Beta), .groups = "drop") # the typical peer's rolling beta in each year
    Last <- Fund_Yr %>% slice_tail(n = 1) %>% left_join(Peer_Yr, by = "Year") # the latest year, where the labels go

    g <- # build the plot in layers and store it as g
        ggplot() + # start the plot
        geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") + # a beta of one moves one for one with the market
        geom_jitter(data = D %>% filter(Group == "Peer"), aes(Year, Beta), width = 0.28, height = 0, color = "grey55", alpha = 0.06, size = 1.2) + # every peer fund's rolling beta in every month
        geom_boxplot(data = D %>% filter(Group == "Peer"), aes(Year, Beta), outlier.shape = NA, width = 0.55, fill = "steelblue", alpha = 0.25, color = "steelblue4") + # the spread of peer betas in each year
        geom_line(data = Fund_Yr, aes(Year, Beta, group = 1), color = "darkorange", linewidth = 1.6) + # the fund's path across the years
        geom_point(data = Fund_Yr, aes(Year, Beta), shape = 23, size = 5, fill = "darkorange", color = "white") + # the fund's average in each year
        geom_text(data = Last, aes(Year, Beta, label = glue::glue("ABC Fund {sprintf('%.2f', Beta)}")), nudge_x = 0.45, hjust = 0, color = "darkorange3", fontface = "bold", size = fmxdat::ggpts(10)) + # label the fund's latest value
        geom_text(data = Last, aes(Year, Med, label = glue::glue("Median peer {sprintf('%.2f', Med)}")), nudge_x = 0.45, hjust = 0, color = "steelblue4", fontface = "bold", size = fmxdat::ggpts(10)) + # label the typical peer's latest value
        annotate("text", x = 0.6, y = 1.02, label = "Moves one for one with the market", hjust = 0, vjust = 0, color = "grey35", size = fmxdat::ggpts(9), fontface = "italic") + # explain the dashed line
        scale_x_discrete(expand = expansion(add = c(0.6, 2.2))) + # room on the right for the labels
        scale_y_continuous(limits = c(0, 1.4)) + # the same range in every year
        Slide_Theme() + # the slide theme
        labs(x = "", y = "Rolling 3 year beta", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g # return the finished plot

}
