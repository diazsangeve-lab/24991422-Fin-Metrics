# Compares annualised volatility between a baseline period and a target period as a directional shift plot, with full-sample benchmarks and the change in a side column.
Plot_Vol_Shift <- function(Vol_Periods, Base_Per = "2021-2024", Target_Per = "2026 YTD", Index_Sel = NULL,
                           Title = "Shift in Annualised Volatility from the Baseline to 2026", Subtitle = "", Caption = "Note:\nCalculation own"){

    # For stepping through uncomment:
    # Base_Per <- "2021-2024" ; Target_Per <- "2026 YTD" ; Index_Sel <- Index_Order

    if( !all(c("Name", "Period", "SD") %in% names(Vol_Periods)) ) stop("... for Plot_Vol_Shift to work, provide a data frame with 'Name', 'Period' and 'SD' columns") # stop early if the period table is not in the expected shape
    if( !all(c(Base_Per, Target_Per, "Full Sample") %in% Vol_Periods$Period) ) stop("... the baseline, target and full sample periods must all be present in Vol_Periods") # stop early if a period is missing

    if( !is.null(Index_Sel) ) Vol_Periods <- Vol_Periods %>% filter(Name %in% Index_Sel) # keep only the chosen indexes

    D <-
        Vol_Periods %>%
        filter(Period %in% c(Base_Per, Target_Per, "Full Sample")) %>% # the three periods the plot compares
        mutate(Period = as.character(Period)) %>% # plain text, so that spreading does not carry unused factor levels
        select(Name, Period, SD) %>% # keep only what the plot needs
        spread(Period, SD) %>% # one column per period
        rename(Base = all_of(Base_Per), Target = all_of(Target_Per), Full = `Full Sample`) %>% # generic names so that the plot works for any pair of periods
        mutate(Gap = Target - Base, # the change in volatility from the baseline to the target period
               Direction = ifelse(Gap >= 0, "Increased", "Decreased"), # whether volatility rose or fell
               Name = forcats::fct_reorder(Name, Gap)) # order the rows from the largest fall to the largest rise

    Gap_X <- max(c(D$Base, D$Target), na.rm = TRUE) + 0.055 # where the change column sits, to the right of every arrow and label
    N_Rows <- nrow(D) # the number of rows, used to place the column header above the top row

    g <-
        ggplot(D) + # start the plot
        annotate("rect", xmin = Gap_X - 0.025, xmax = Gap_X + 0.025, ymin = -Inf, ymax = Inf, fill = "grey93") + # a shaded strip for the change column
        geom_segment(aes(x = Base, xend = Target, y = Name, yend = Name, color = Direction), linewidth = 1.3, arrow = arrow(length = unit(0.22, "cm"), type = "closed")) + # an arrow from the baseline to the target period
        geom_point(aes(Full, Name), shape = 18, size = 4, color = "grey45") + # a grey diamond at the full sample norm
        geom_point(aes(Base, Name), color = "steelblue", size = 3.5) + # a blue dot at the baseline
        geom_text(aes(x = ifelse(Gap >= 0, Base - 0.008, Base + 0.008), y = ifelse(Gap < 0, as.numeric(Name) + 0.22, as.numeric(Name)), label = scales::percent(Base, accuracy = 0.1), hjust = ifelse(Gap >= 0, 1, 0.5)), color = "steelblue", size = fmxdat::ggpts(8.5), fontface = "bold") + # the baseline value, placed so that it never sits on the arrow
        geom_text(aes(x = ifelse(Gap >= 0, Target + 0.008, Target - 0.008), y = Name, label = scales::percent(Target, accuracy = 0.1), color = Direction, hjust = ifelse(Gap >= 0, 0, 1)), size = fmxdat::ggpts(8.5), fontface = "bold") + # the target value at the head of the arrow
        geom_text(aes(x = Gap_X, y = Name, label = sprintf("%+0.1f pp", Gap * 100), color = Direction), fontface = "bold", size = fmxdat::ggpts(9)) + # the change in percentage points in the side column
        annotate("text", x = Gap_X, y = N_Rows + 0.6, label = "Change", vjust = 0, fontface = "bold", size = fmxdat::ggpts(8.5)) + # header for the change column
        scale_color_manual(values = c("Increased" = "firebrick", "Decreased" = "seagreen")) + # red for higher volatility and green for lower
        scale_y_discrete(expand = expansion(add = c(0.6, 1.0))) + # leave room above the top row for the header
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.08, 0.05))) + # percentage axis with room on the left for the labels
        guides(color = "none") + # the colours are explained by the arrows and the axis title
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(28), subtitle.size = fmxdat::ggpts(24), caption.size = fmxdat::ggpts(20), CustomCaption = TRUE) + # the clean course theme with readable text sizes
        labs(x = glue::glue("Annualised volatility (blue dot = {Base_Per} baseline, arrow head = {Target_Per}, diamond = full sample norm)"), y = "", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g) # finishing touches

}
