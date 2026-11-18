# Plots standardised regression coefficients with confidence intervals for several models side by side, highlighting the coefficients that are statistically significant.
Plot_Macro_Coefs <- function(df, Title = "What Moves Alpha Opportunity", Subtitle = "", Caption = "Note:\nCalculation own"){ # define Plot_Macro_Coefs, with input df, plus the title, subtitle and caption text

    if( !all(c("Term", "Estimate", "Low", "High", "P", "Model", "R2") %in% names(df)) ) stop("... for Plot_Macro_Coefs to work, first run Macro_Regression") # stop early if the coefficient table is missing

    D <- # build D through the steps below
        df %>% # start from the input data
        mutate(Significant = ifelse(P < 0.05, "Significant at 5%", "Not significant"), # flag coefficients that are significant at the 5 percent level
               Panel = glue::glue("{Model} (R-squared {sprintf('%.2f', R2)})"), # panel titles that carry the model fit
               Panel = factor(Panel, levels = unique(Panel)), # keep the panels in the order the models were supplied
               Term = forcats::fct_reorder(Term, Estimate)) # order the variables by their effect

    g <- # build the plot in layers and store it as g
        D %>% # start from D, built above
        ggplot() + # start the plot
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") + # zero means no effect
        geom_errorbar(aes(xmin = Low, xmax = High, y = Term, color = Significant), width = 0.25, linewidth = 0.8, orientation = "y") + # the 95 percent confidence interval of each coefficient
        geom_point(aes(Estimate, Term, color = Significant), size = 3.2) + # the estimated coefficient
        facet_wrap(~Panel, scales = "free_x") + # one panel per model, each with its own horizontal scale
        scale_color_manual(values = c(`Significant at 5%` = "firebrick", `Not significant` = "grey60")) + # red for significant effects and grey for the rest
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(26), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        labs(x = "Effect of a one standard deviation move in the macro variable", y = "", title = Title, subtitle = Subtitle, caption = Caption, color = "") # axis names, title and note

    g # return the finished plot

}
