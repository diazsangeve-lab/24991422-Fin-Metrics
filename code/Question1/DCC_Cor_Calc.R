# Fits a DCC-GARCH model and returns the time-varying correlation between a base series and every other series.
DCC_Cor_Calc <- function(df, Base = "Capped SWIX"){

    if( !Base %in% unique(df$Name) ) stop(glue::glue("... {Base} is not a Name in the data frame provided")) # stop early if the base series does not exist

    xts_rtn <- df %>% tbl2xts::tbl_xts(cols_to_xts = dlogret, spread_by = Name, Colnames_Exact = T) # wide xts of returns, one column per series, keeping the names exactly as given
    xts_rtn <- xts_rtn[complete.cases(xts_rtn), ] # drop any week with a missing return

    uspec <- rugarch::ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), # a GJR variance equation for each series, to allow for leverage
                                 mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), # an AR(1) mean equation for each series
                                 distribution.model = "sstd") # skewed student t errors, as in the practical

    spec_dcc <- rmgarch::dccspec(uspec = rugarch::multispec(replicate(ncol(xts_rtn), uspec)), # repeat the univariate specification for every series
                                 dccOrder = c(1, 1), # one lag of shocks and one lag of the correlation process
                                 distribution = "mvnorm", # multivariate normal for the correlation step
                                 model = "DCC") # Engle's dynamic conditional correlation model

    fit_dcc <- rmgarch::dccfit(spec_dcc, data = xts_rtn, solver = "solnp", fit.control = list(eval.se = FALSE)) # fit the univariate models and then the correlation process

    Rho <- rmgarch::rcor(fit_dcc) # an array of correlation matrices, one for every week
    Pars <- rugarch::coef(fit_dcc) # the fitted parameters, including the DCC a and b

    Base_i <- which(colnames(xts_rtn) == Base) # the position of the base series, since the correlation array is indexed by position

    setdiff(seq_len(ncol(xts_rtn)), Base_i) %>% # the positions of every series other than the base
        map_df(~tibble(date = as.Date(zoo::index(xts_rtn)), # the week of each correlation
                       Pair = glue::glue("{Base} vs {colnames(xts_rtn)[.x]}"), # a readable label for the pair
                       Rho = Rho[Base_i, .x, ])) %>% # the correlation between the base and this series in every week
        mutate(dcc_a = Pars["[Joint]dcca1"], dcc_b = Pars["[Joint]dccb1"]) # attach the DCC parameters, where a + b close to one means correlations move slowly

}
