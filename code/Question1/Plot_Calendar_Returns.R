# Plots calendar year returns of a fund next to its benchmark, with the yearly difference printed above each pair, in the spirit of the calendar returns table of Practical 2.
Plot_Calendar_Returns <- function(df, Fund_Name = "SafeCore", BM_Name = "Capped SWIX", Title = "Calendar Year Returns", Subtitle = "", Caption = "Note:\nCalculation own"){

    Yr <- df %>% mutate(Year = lubridate::year(date)) %>% group_by(Name, Year) %>% summarise(Ret = prod(1 + Returns) - 1, Days = n(), .groups = "drop") # each portfolio's compounded return in every calendar year
    Gap <- Yr %>% select(Name, Year, Ret) %>% spread(Name, Ret) %>% mutate(Diff = .data[[Fund_Name]] - .data[[BM_Name]], Top = pmax(.data[[Fund_Name]], .data[[BM_Name]], 0)) # the fund's lead or lag in each year
    YTD <- max(Yr$Year[Yr$Days < 200], -Inf) # an incomplete final year, if there is one

    ggplot(Yr, aes(factor(Year), Ret, fill = Name)) + # years along the bottom
        geom_hline(yintercept = 0, color = "grey40") + # zero return
        geom_col(position = position_dodge(width = 0.75), width = 0.7) + # the fund and benchmark side by side
        geom_label(data = Gap, aes(factor(Year), Top + 0.02, label = sprintf("%+.1f", Diff * 100), fill = NULL, color = Diff > 0), size = fmxdat::ggpts(8), fontface = "bold", label.size = 0, fill = "white", vjust = 0) + # the yearly difference in percentage points
        scale_fill_manual(values = purrr::set_names(c("darkorange", "grey35"), c(Fund_Name, BM_Name))) + # orange for the fund and grey for the benchmark
        scale_color_manual(values = c(`TRUE` = "#1E7B3A", `FALSE` = "#B2182B"), guide = "none") + # green for a lead and red for a lag
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.12))) + # returns as percentages with room for the labels
        scale_x_discrete(labels = function(x) ifelse(x == as.character(YTD), paste0(x, "*"), x)) + # a star on an incomplete year
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "top") + # the legend above the bars
        labs(x = "", y = "Calendar year return", fill = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
