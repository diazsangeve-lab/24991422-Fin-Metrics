# Plots the distribution of GARCH conditional volatility in each year as overlapping ridgelines, so that shifts in the whole distribution are visible.
Plot_Garch_Sigma <- function(df, X_Max = 0.5, Cols = c("#1F4E79", "grey50", "darkorange"), Title = "Conditional Volatility from GARCH", Subtitle = "", Caption = "Note:\nCalculation own"){

    if( !all(c("Sigma", "Model", "Persistence") %in% names(df)) ) stop("... for Plot_Garch_Sigma to work, first run Best_Garch_Sigma") # stop early if the model output is missing

    g <-
        df %>%
        mutate(Label = glue::glue("{Name} ({Model}, persistence {sprintf('%.2f', Persistence)})")) %>% # name each series with its chosen model and persistence
        mutate(Year = factor(lubridate::year(date), levels = rev(sort(unique(lubridate::year(date)))))) %>% # years as rows, with the most recent year at the top
        ggplot(aes(Sigma, Year, fill = Label, color = Label)) + # volatility along the bottom and one row per year
        ggridges::geom_density_ridges(alpha = 0.22, scale = 1.5, linewidth = 0.6, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2) + # the distribution of daily conditional volatility in each year, outlined in its own colour, with a line at the median
        coord_cartesian(xlim = c(0, X_Max)) + # zoom in so that the long Covid tail does not squash every other year
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        scale_fill_manual(values = Cols) + # deliberate colours, so that overlapping ridges stay distinguishable
        scale_color_manual(values = Cols) + # the same colours for the outlines and median lines
        guides(fill = guide_legend(ncol = 1), color = guide_legend(ncol = 1)) + # stack the long legend labels
        labs(x = "Annualised conditional volatility", y = "", title = Title, subtitle = Subtitle, caption = Caption, fill = "", color = "") # axis names, title and note

    fmxdat::finplot(g) # finishing touches

}
