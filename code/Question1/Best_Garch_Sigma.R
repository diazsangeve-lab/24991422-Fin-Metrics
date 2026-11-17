# Fits several GARCH specifications to one series, keeps the one with the lowest AIC, and returns its conditional volatility.
Best_Garch_Sigma <- function(df, Series, Models = c("sGARCH", "gjrGARCH", "eGARCH", "apARCH"), Ann = 252){

    # For stepping through uncomment:
    # Series <- "Capped SWIX"

    if( !Series %in% unique(df$Name) ) stop(glue::glue("... {Series} is not a Name in the data frame provided")) # stop early if the series does not exist

    xts_rtn <-
        df %>%
        filter(Name == Series) %>% # keep only the chosen series
        filter(!is.na(dlogret)) %>% # the model cannot handle missing returns
        tbl2xts::tbl_xts(cols_to_xts = dlogret) # rugarch needs the data in xts format

    Safe_Fit <- purrr::safely(rugarch::ugarchfit) # a version of the fitting function that does not break the loop if one model fails to converge

    Fit_List <-
        Models %>%
        purrr::set_names() %>% # name each list entry after its model
        map(~Safe_Fit(spec = rugarch::ugarchspec(variance.model = list(model = .x, garchOrder = c(1, 1)), # the variance equation, with one lag of the shock and one lag of the variance
                                                 mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), # an AR(1) mean equation, to clean out first order persistence in returns
                                                 distribution.model = "std"), # student t errors, as returns have fat tails
                      data = xts_rtn)$result) %>% # fit the model and keep the result, which is NULL if it failed
        purrr::compact() # drop the models that failed

    if( length(Fit_List) == 0 ) stop(glue::glue("... no GARCH model converged for {Series}")) # stop if nothing could be fitted

    AICs <- Fit_List %>% map_dbl(~rugarch::infocriteria(.x)[1]) # the Akaike criterion of each fitted model, where lower is better
    Best <- names(AICs)[which.min(AICs)] # the name of the best fitting model

    tibble(date = as.Date(zoo::index(rugarch::sigma(Fit_List[[Best]]))), # the dates of the fitted sample
           Name = Series, # the series name
           Sigma = as.numeric(rugarch::sigma(Fit_List[[Best]])) * sqrt(Ann), # the conditional volatility, scaled to a yearly number
           Model = Best, # the model that was chosen
           Persistence = rugarch::persistence(Fit_List[[Best]]), # how slowly a volatility shock dies out, where a value near one is very slow
           AIC = min(AICs)) # the fit statistic of the chosen model

}
