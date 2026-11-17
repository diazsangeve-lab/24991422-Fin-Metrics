# Regresses one column on another and tests the slope with Newey-West standard errors, which allow for the autocorrelation created by overlapping windows.
NW_Slope_Test <- function(df, Y, X, Lag = 12){

    m <- lm(as.formula(paste(Y, "~", X)), data = df) # the simple regression
    ct <- lmtest::coeftest(m, vcov = sandwich::NeweyWest(m, lag = Lag)) # coefficients with Newey-West standard errors

    tibble(Estimate = ct[2, 1], SE = ct[2, 2], t = ct[2, 3], p = ct[2, 4], R2 = summary(m)$r.squared, N = nobs(m)) # the slope, its test and the fit

}
