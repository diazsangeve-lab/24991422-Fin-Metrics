# Plots rolling volatility as small multiples, with every other series greyed out behind the highlighted one and its recent peak labelled.
Plot_Roll_Vol <- function(df, Highlight_From = lubridate::ymd(20260101), Ncol = 3, Title = "Rolling Realised Volatility", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Roll_Vol, with inputs df, Highlight_From and Ncol, plus the title, subtitle and caption text

    if( !"RollSD" %in% names(df)) stop("... for Plot_Roll_Vol to work, first run Roll_SD_Calc to create the 'RollSD' column") # stop early if the rolling numbers are missing

    Background <- df %>% rename(Group = Name) # a copy without the facet variable, so that every series appears in every panel
    Peaks <- df %>% filter(date >= Highlight_From) %>% group_by(Name) %>% slice_max(RollSD, n = 1, with_ties = FALSE) %>% ungroup() # the highest reading of each series in the highlighted period

    g <- # build the plot in layers and store it as g
        ggplot() + # start an empty plot, since each layer brings its own data
        annotate("rect", xmin = Highlight_From, xmax = max(df$date), ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.12) + # shade the recent period that the question is about
        geom_line(data = Background, aes(date, RollSD, group = Group), color = "grey82", linewidth = 0.35) + # every series in light grey, as context
        geom_line(data = df, aes(date, RollSD, color = Name), linewidth = 0.9) + # the highlighted series of each panel
        geom_hline(data = df %>% distinct(Name, Avg_SD), aes(yintercept = Avg_SD, color = Name), linetype = "dashed", linewidth = 0.4) + # each series' own long run average
        geom_point(data = Peaks, aes(date, RollSD, color = Name), size = 2.2) + # mark the recent peak
        geom_text(data = Peaks, aes(date, RollSD, label = scales::percent(RollSD, accuracy = 1), color = Name), vjust = -0.9, size = fmxdat::ggpts(9), fontface = "bold") + # print the recent peak above the point
        facet_wrap(~Name, ncol = Ncol) + # one panel per series
        guides(color = "none") + # the panel titles already name each series
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        fmxdat::fmx_cols() + # the crisp course colours
        labs(x = "", y = "Rolling annualised volatility", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    fmxdat::finplot(g, x.date.dist = "2 years", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1) # finishing touches, with date labels every two years and a percentage axis

}
