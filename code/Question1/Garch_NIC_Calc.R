# Refits a chosen GARCH model to one series and traces its news impact curve over a common grid of shocks.
Garch_NIC_Calc <- function(df, Series, Model, Shock_Range = 0.06, Ann = 252){

    # For stepping through uncomment:
    # Series <- "Capped SWIX" ; Model <- "eGARCH"

    if( !Series %in% unique(df$Name) ) stop(glue::glue("... {Series} is not a Name in the data frame provided")) # stop early if the series does not exist

    xts_rtn <-
        df %>%
        filter(Name == Series) %>% # keep only the chosen series
        filter(!is.na(dlogret)) %>% # the model cannot handle missing returns
        tbl2xts::tbl_xts(cols_to_xts = dlogret) # rugarch needs the data in xts format

    Fit <- rugarch::ugarchfit(spec = rugarch::ugarchspec(variance.model = list(model = Model, garchOrder = c(1, 1)), # the same variance equation chosen earlier by AIC
                                                         mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), # the same AR(1) mean equation
                                                         distribution.model = "std"), # the same student t errors
                              data = xts_rtn) # fit the model

    NI <- rugarch::newsimpact(z = seq(-Shock_Range, Shock_Range, length.out = 201), Fit) # the variance response to each size of shock, on the same grid for every series

    tibble(Shock = NI$zx, # the size of yesterday's unexpected return
           Vol = sqrt(NI$zy * Ann), # today's conditional volatility, turned from a daily variance into an annualised volatility
           Name = glue::glue("{Series} ({Model})")) # label the curve with the series and the model

}
