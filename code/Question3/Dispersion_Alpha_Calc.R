# Relates trailing market dispersion to the client fund's trailing alpha and to the alpha of the typical peer, where alpha is the return left after removing each fund's market exposure.
Dispersion_Alpha_Calc <- function(df, Disp, Fund_Label = "ABC Fund", NSel = 12, Min_Months = 36){

    if( !all(c("YM", "Disp") %in% names(Disp)) ) stop("... for Dispersion_Alpha_Calc to work, first run Market_Dispersion_Calc") # stop early if the dispersion series is missing

    Alphas <-
        df %>%
        filter(!is.na(BM)) %>% # only the months in which the benchmark is available
        group_by(Name) %>% # one beta per fund
        filter(n() >= Min_Months) %>% # funds need a minimum record for a stable beta
        mutate(Alpha = Returns - coef(lm(Returns ~ BM))[2] * BM) %>% # each month's return after removing the fund's market exposure
        arrange(date) %>% # order by date before rolling
        mutate(Alpha_Roll = RcppRoll::roll_sum(Alpha, NSel, fill = NA, align = "right"), # the alpha earned over the trailing window
               Span = as.numeric(date - lag(date, NSel - 1))) %>% # the number of days each window covers
        ungroup() %>% # remove the grouping again
        filter(!is.na(Alpha_Roll), Span < NSel * 31) # keep complete windows of consecutive months

    Disp_Roll <- Disp %>% arrange(date) %>% transmute(YM, Disp, Disp_Roll = RcppRoll::roll_mean(Disp, NSel, fill = NA, align = "right")) # the average dispersion over the same trailing window

    Alphas %>%
        group_by(date, YM) %>% # one row per month
        summarise(Fund_Alpha = Alpha_Roll[Group == Fund_Label][1], # the client fund's trailing alpha
                  Peer_Alpha = median(Alpha_Roll[Group == "Peer"]), # the typical peer's trailing alpha
                  .groups = "drop") %>%
        filter(!is.na(Fund_Alpha)) %>% # only months in which the client fund has a full window
        inner_join(Disp_Roll, by = "YM") %>% # attach the trailing dispersion
        filter(!is.na(Disp_Roll)) %>% # only months with a full dispersion window
        mutate(Tercile = factor(ntile(Disp_Roll, 3), labels = c("Low dispersion", "Middle", "High dispersion"))) # split the windows into thirds by dispersion

}
