# Plots rolling annualised returns of the size indexes and the benchmark with inline labels, shading periods in which small caps beat large caps.
Plot_Size_Rolling <- function(RR, Fund_Cols = c(`Capped SWIX` = "black", `Large Caps` = "#B2182B", `Mid Caps` = "darkorange", `Small Caps` = "#4C78A8"), Title = "Rolling 3 Year Annualised Returns", Subtitle = "", Caption = "Note:\nCalculation own"){

    Spread <- RR %>% select(date, Name, RollRet) %>% spread(Name, RollRet) %>% mutate(Small_Leads = `Small Caps` > `Large Caps`) # whether small caps were ahead of large caps on each date
    Spells <- # build Spells through the steps below
        Spread %>% # start from Spread, built above
        filter(!is.na(Small_Leads)) %>% # only dates where both rolling returns exist
        mutate(Spell = cumsum(Small_Leads != lag(Small_Leads, default = dplyr::first(Small_Leads)))) %>% # a new spell number each time the leader changes
        filter(Small_Leads) %>% # keep the spells in which small caps led
        group_by(Spell) %>% # one shaded block per spell
        summarise(Start = min(date), End = max(date), .groups = "drop") # the first and last month of each spell

    Ends <- RR %>% filter(date == max(date)) # the last month, where the line labels go
    Breaks <- seq(lubridate::floor_date(min(RR$date), "year"), max(RR$date), by = "1 year") # yearly date labels that stop at the last month

    g <- # build the plot in layers and store it as g
        RR %>% # start from RR
        ggplot() + # start the plot
        geom_rect(data = Spells, aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf), fill = "#4C78A8", alpha = 0.1) + # shade the spells in which small caps led large caps
        geom_hline(yintercept = 0, color = "grey40") + # zero return
        geom_line(aes(date, RollRet, color = Name), linewidth = 1) + # the rolling return of each index
        ggrepel::geom_text_repel(data = Ends, aes(date, RollRet, label = glue::glue("{Name} ({scales::percent(RollRet, accuracy = 0.1)})"), color = Name), hjust = 0, nudge_x = 60, direction = "y", fontface = "bold", size = fmxdat::ggpts(8.5), segment.color = NA) + # label each line with its latest value, pushed apart so the labels never overlap
        scale_color_manual(values = Fund_Cols) + # a fixed colour per index
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # returns as percentages
        scale_x_date(expand = expansion(mult = c(0.01, 0.22)), breaks = Breaks, date_labels = "%Y") + # yearly labels and room on the right
        guides(color = "none") + # the lines are labelled directly
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Rolling 3 year return (ann.)", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g, x.vert = T) # finishing touches, with vertical date labels

}
