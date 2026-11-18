# Plots the average stock variance split into its common part and its dispersion part, so that the alpha opportunity can be seen as a share of total stock risk.
Plot_Opportunity_Split <- function(df, Title = "Splitting Stock Risk into Common and Diversifiable Parts", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Opportunity_Split, with input df, plus the title, subtitle and caption text

    if( !all(c("date", "Common_Var", "Dispersion_Var") %in% names(df)) ) stop("... for Plot_Opportunity_Split to work, first run Avg_Corr_Calc") # stop early if the variance split is missing

    Long <- # build Long through the steps below
        df %>% # start from the input data
        select(date, `Common to the index` = Common_Var, `Dispersion (alpha opportunity)` = Dispersion_Var) %>% # the two parts of average stock variance
        gather(Part, Variance, -date) %>% # long format, one row per month and part
        mutate(Part = factor(Part, levels = c("Dispersion (alpha opportunity)", "Common to the index"))) # dispersion stacked on top of the common part

    Share <- df %>% mutate(Share = Dispersion_Var / (Common_Var + Dispersion_Var)) # the share of stock risk that is diversifiable in each month
    Marks <- bind_rows(Share %>% slice_max(Share, n = 1, with_ties = FALSE), Share %>% slice_min(Share, n = 1, with_ties = FALSE), Share %>% filter(date == max(date))) %>% distinct(date, .keep_all = TRUE) # the highest, lowest and latest shares, which get labels

    g <- # build the plot in layers and store it as g
        Long %>% # start from Long, built above
        ggplot() + # start the plot
        geom_area(aes(date, Variance, fill = Part), alpha = 0.85, color = "white", linewidth = 0.2) + # the two parts stacked, so that the total height is the average stock variance
        geom_label(data = Marks, aes(date, Common_Var + Dispersion_Var, label = glue::glue("{format(date, '%b %Y')}\n{scales::percent(Share, accuracy = 1)} diversifiable")), vjust = -0.15, size = fmxdat::ggpts(7.5), label.size = 0, fill = "white", alpha = 0.85, lineheight = 0.85) + # label the extreme and latest months with the diversifiable share
        scale_fill_manual(values = c(`Dispersion (alpha opportunity)` = "darkorange", `Common to the index` = "grey55")) + # orange for the opportunity and grey for the part shared with the index
        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # room above the areas for the labels
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # yearly labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "", y = "Average annualised stock variance", title = Title, subtitle = Subtitle, caption = Caption, fill = "") # axis names, title and note

    g # return the finished plot

}
