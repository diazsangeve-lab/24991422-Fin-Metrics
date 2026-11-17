# Regresses a monthly series on standardised macro variables and returns tidy coefficients with Newey-West standard errors, which allow for autocorrelation in overlapping monthly data.
Macro_Regression <- function(df, Y, X_Vars, Lag = 3, Label = Y){

    # For stepping through uncomment:
    # df <- Macro_Panel ; Y <- "Dispersion_W" ; X_Vars <- c("VIX", "MOVE") ; Lag <- 3

    if( !all(c(Y, X_Vars) %in% names(df)) ) stop("... every variable must be a column in the data frame provided") # stop early if a variable is missing

    D <- df %>% select(all_of(c(Y, X_Vars))) %>% tidyr::drop_na() %>% mutate(across(all_of(X_Vars), ~as.numeric(scale(.)))) # standardise every regressor, so that each coefficient is the effect of a one standard deviation move
    Fit <- lm(reformulate(paste0("`", X_Vars, "`"), response = Y), data = D) # ordinary least squares on the standardised regressors
    NW <- lmtest::coeftest(Fit, vcov. = sandwich::NeweyWest(Fit, lag = Lag, prewhite = FALSE)) # coefficient tests with Newey-West standard errors

    tibble(Term = gsub("`", "", rownames(NW)), Estimate = NW[, 1], SE = NW[, 2], P = NW[, 4]) %>% # tidy the coefficient table
        filter(Term != "(Intercept)") %>% # the intercept is not of interest
        mutate(Low = Estimate - 1.96 * SE, High = Estimate + 1.96 * SE, # a 95 percent confidence interval
               Model = Label, R2 = summary(Fit)$r.squared, N = nobs(Fit)) # the model label, its fit and its sample size

}
