# Plots each portfolio rule's rank by calendar year return as a bump chart, with smooth curves between years and each year's return printed on its point.
Plot_Rank_Bump <- function(Rets, Pal, Title = "Calendar Year Rankings", Subtitle = "", Caption = "Note:\nCalculation own"){

    Yr <- Rets %>% mutate(Year = lubridate::year(date)) %>% group_by(Year, Strategy) %>% summarise(Ret = prod(1 + Returns) - 1, Months = n(), .groups = "drop") %>% group_by(Year) %>% mutate(Rank = rank(-Ret, ties.method = "first")) %>% ungroup() # each rule's return and rank in every calendar year

    Sigmoid <- function(x0, x1, y0, y1, n = 50) { t <- seq(-6, 6, length.out = n); tibble(x = seq(x0, x1, length.out = n), y = y0 + (y1 - y0) / (1 + exp(-t))) } # an s-shaped curve between two points, as in a bump chart
    Curves <- Yr %>% arrange(Strategy, Year) %>% group_by(Strategy) %>% mutate(Next_Year = lead(Year), Next_Rank = lead(Rank)) %>% filter(!is.na(Next_Year)) %>% ungroup() %>% mutate(Path = pmap(list(Year, Next_Year, Rank, Next_Rank), Sigmoid)) %>% select(Strategy, Year, Path) %>% tidyr::unnest(Path) # the curves linking each rule's ranks

    Ends <- Yr %>% filter(Year == max(Year)) # the latest year, where the names go
    YTD <- unique(Yr$Months[Yr$Year == max(Yr$Year)]) < 12 # whether the last year is incomplete

    ggplot() + # start the plot
        geom_path(data = Curves, aes(x, y, group = interaction(Strategy, Year), color = Strategy), linewidth = 3, alpha = 0.85, lineend = "round") + # the curves between years
        geom_point(data = Yr, aes(Year, Rank, color = Strategy), size = 11) + # one point per rule and year
        geom_text(data = Yr, aes(Year, Rank, label = scales::percent(Ret, accuracy = 1)), color = "white", fontface = "bold", size = fmxdat::ggpts(7.5)) + # the year's return inside each point
        geom_text(data = Ends, aes(Year + 0.32, Rank, label = Strategy, color = Strategy), hjust = 0, fontface = "bold", size = fmxdat::ggpts(9)) + # the rule's name at the right
        scale_y_reverse(breaks = 1:4, labels = c("1st", "2nd", "3rd", "4th")) + # first place at the top
        scale_x_continuous(breaks = unique(Yr$Year), labels = ifelse(unique(Yr$Year) == max(Yr$Year) & YTD, paste0(unique(Yr$Year), "*"), unique(Yr$Year)), expand = expansion(add = c(0.4, 1.9))) + # the years, with a star on an incomplete year
        scale_color_manual(values = Pal$Strategy, guide = "none") + # the rule colours
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(panel.grid = element_blank()) + # no grid, as in the gallery's bump chart
        labs(x = "", y = "Rank by calendar year return", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
