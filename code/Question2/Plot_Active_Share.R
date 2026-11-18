# Plots the active share of each size index against the benchmark through time, with each index's average as a dashed line and an inline label.
Plot_Active_Share <- function(AS, Fund_Cols = c(`Large Caps` = "#B2182B", `Mid Caps` = "darkorange", `Small Caps` = "#4C78A8"), Title = "Active Share Against the Capped SWIX", Subtitle = "", Caption = "Note:\nCalculation own"){

    D <- AS %>% group_by(Fund) %>% mutate(Avg = mean(Active_Share)) %>% ungroup() # each index's average active share
    Ends <- D %>% filter(date == max(date)) # the last month, where the line labels go
    Breaks <- seq(lubridate::floor_date(min(D$date), "year"), max(D$date), by = "2 years") # date labels every two years that stop at the last month

    g <- # build the plot in layers and store it as g
        D %>% # start from D, built above
        ggplot() + # start the plot
        geom_line(aes(date, Avg, color = Fund), linetype = "dashed", linewidth = 0.5) + # each index's average active share
        geom_line(aes(date, Active_Share, color = Fund), linewidth = 1.1) + # the monthly active share
        geom_text(data = Ends, aes(date, Active_Share, label = glue::glue("{Fund} ({scales::percent(Active_Share, accuracy = 1)})"), color = Fund), hjust = -0.08, fontface = "bold", size = fmxdat::ggpts(8.5)) + # label each line with its latest value
        scale_color_manual(values = Fund_Cols) + # a fixed colour per index
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) + # active share as a percentage between zero and one hundred
        scale_x_date(expand = expansion(mult = c(0.01, 0.2)), breaks = Breaks, date_labels = "%Y") + # room on the right for the labels
        guides(color = "none") + # the lines are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Active share", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g, x.vert = T) # finishing touches, with vertical date labels

}
