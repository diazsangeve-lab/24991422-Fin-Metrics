# Plots the client fund's rolling tracking error and information ratio inside the middle half of the peer group, so that its activeness and the reward for that activeness can be read together.
Plot_Roll_TE_IR <- function(TI, Fund_Label = "ABC Fund", Title = "Activeness and the Reward for It", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Roll_TE_IR, with inputs TI and Fund_Label, plus the title, subtitle and caption text

    D <- # build D through the steps below
        TI %>% # start from TI
        gather(Measure, Value, TE, IR) %>% # long format, one row per measure
        group_by(date, Measure) %>% # one set of bands per month and measure
        summarise(P25 = quantile(Value[Group == "Peer"], 0.25), P50 = median(Value[Group == "Peer"]), P75 = quantile(Value[Group == "Peer"], 0.75), Fund = Value[Group == Fund_Label][1], .groups = "drop") %>% # the peer quartiles and the fund
        filter(!is.na(Fund)) %>% # only months with a full window for the fund
        mutate(Measure = factor(recode(Measure, TE = "Rolling 3 year tracking error", IR = "Rolling 3 year information ratio"), levels = c("Rolling 3 year tracking error", "Rolling 3 year information ratio"))) # readable panel names
    Ends <- D %>% filter(date == max(date)) # the last month, where the labels go

    ggplot(D) + # start the plot
        geom_hline(data = tibble(Measure = factor("Rolling 3 year information ratio", levels = levels(D$Measure))), aes(yintercept = 0), color = "grey40") + # a zero information ratio
        geom_ribbon(aes(date, ymin = P25, ymax = P75), fill = "steelblue", alpha = 0.25) + # the middle half of peers
        geom_line(aes(date, P50), color = "steelblue4", linewidth = 0.8) + # the median peer
        geom_line(aes(date, Fund), color = "darkorange", linewidth = 1.5) + # the client fund
        ggrepel::geom_label_repel(data = Ends %>% select(date, Measure, `ABC Fund` = Fund, `Median peer` = P50) %>% gather(Who, y, `ABC Fund`, `Median peer`), aes(date, y, label = glue::glue("{Who} {ifelse(Measure == 'Rolling 3 year tracking error', scales::percent(y, accuracy = 0.1), sprintf('%.2f', y))}"), color = Who), hjust = 0, nudge_x = 120, direction = "y", size = fmxdat::ggpts(8.5), fontface = "bold", fill = "white", label.size = 0, min.segment.length = 0, seed = 1) + # the latest values of the fund and the median peer, pushed apart so they never overlap
        scale_color_manual(values = c(`ABC Fund` = "#8C3B00", `Median peer` = "steelblue4"), guide = "none") + # label colours matching the lines
        facet_wrap(~Measure, ncol = 1, scales = "free_y") + # one panel per measure
        scale_x_date(breaks = seq(lubridate::ymd(20180101), max(D$date), by = "2 years"), date_labels = "%Y", expand = expansion(mult = c(0.01, 0.22))) + # labels that stop at the data, with room on the right
        scale_y_continuous(labels = function(x) if( max(abs(x), na.rm = TRUE) < 0.3 ) scales::percent(x, accuracy = 1) else sprintf("%.1f", x)) + # each panel labels its own axis, so the tracking error panel shows percentages and the information ratio panel plain numbers
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

}
