# Plots monthly dispersion against each macro variable as small multiples, with a fitted line and the months of the highlighted year picked out.
Plot_Macro_Scatter <- function(df, Y = "Dispersion_W", X_Vars, Highlight_Year = 2026, Title = "Dispersion and the Global Macro Backdrop", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Macro_Scatter, with inputs df, Y, X_Vars and Highlight_Year, plus the title, subtitle and caption text

    if( !all(c("date", Y, X_Vars) %in% names(df)) ) stop("... every variable must be a column in the data frame provided") # stop early if a variable is missing

    Long <- # build Long through the steps below
        df %>% # start from the input data
        select(date, Y = all_of(Y), all_of(X_Vars)) %>% # the outcome and the macro variables
        gather(Variable, X, -date, -Y) %>% # long format, one row per month and macro variable
        filter(!is.na(X), !is.na(Y)) %>% # complete pairs only
        mutate(Highlight = lubridate::year(date) == Highlight_Year, # whether the month belongs to the highlighted year
               Variable = factor(Variable, levels = X_Vars)) # keep the panels in the order given

    Cors <- Long %>% group_by(Variable) %>% summarise(Rho = cor(X, Y), X = max(X), Y = max(Long$Y), .groups = "drop") # the correlation in each panel and a corner to print it in

    g <- # build the plot in layers and store it as g
        Long %>% # start from Long, built above
        ggplot(aes(X, Y)) + # macro variable along the bottom and dispersion up the side
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "grey35", fill = "grey88", linewidth = 0.6) + # the fitted relationship in each panel
        geom_point(data = Long %>% filter(!Highlight), color = "grey60", alpha = 0.7, size = 1.6) + # the other months in grey
        geom_point(data = Long %>% filter(Highlight), color = "darkorange", size = 2.6) + # the highlighted year's months in orange
        geom_text(data = Cors, aes(X, Y, label = glue::glue("r = {sprintf('%.2f', Rho)}")), hjust = 1, vjust = 1, fontface = "bold", size = fmxdat::ggpts(8.5)) + # the correlation in the top right corner
        facet_wrap(~Variable, scales = "free_x", nrow = 2) + # one panel per macro variable, each with its own horizontal scale
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # dispersion as a percentage
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Monthly level of the macro variable", y = "Weighted dispersion", title = Title, subtitle = Subtitle, caption = Caption) # axis names, title and note

    g # return the finished plot

}
