# Relates trailing market dispersion to the client fund's trailing alpha and to the alpha of the typical peer, where alpha is the return left after removing each fund's market exposure, estimated with a trailing beta that uses no later data.
Dispersion_Alpha_Calc <- function(df, Disp, Fund_Label = "ABC Fund", NSel = 12, Min_Months = 36){ # define Dispersion_Alpha_Calc, with inputs df, Disp, Fund_Label, NSel and Min_Months

    if( !all(c("YM", "Disp") %in% names(Disp)) ) stop("... for Dispersion_Alpha_Calc to work, first run Market_Dispersion_Calc") # stop early if the dispersion series is missing

    Alphas <- # build Alphas through the steps below
        df %>% # start from the input data
        filter(!is.na(BM)) %>% # only the months in which the benchmark is available
        group_by(Name) %>% # roll within each fund
        filter(n() >= Min_Months) %>% # funds need a minimum record for a stable beta
        arrange(date) %>% # order by date before rolling
        mutate(Beta_Roll = (RcppRoll::roll_mean(Returns * BM, Min_Months, fill = NA, align = "right") - RcppRoll::roll_mean(Returns, Min_Months, fill = NA, align = "right") * RcppRoll::roll_mean(BM, Min_Months, fill = NA, align = "right")) / (RcppRoll::roll_mean(BM^2, Min_Months, fill = NA, align = "right") - RcppRoll::roll_mean(BM, Min_Months, fill = NA, align = "right")^2), # the fund's beta over the trailing beta window, so that no later months enter the estimate
               Alpha_Roll = RcppRoll::roll_sum(Returns, NSel, fill = NA, align = "right") - Beta_Roll * RcppRoll::roll_sum(BM, NSel, fill = NA, align = "right"), # the return earned over the trailing alpha window after removing the fund's market exposure
               Span = as.numeric(date - lag(date, Min_Months - 1))) %>% # the number of days the beta window covers, which contains the alpha window
        ungroup() %>% # remove the grouping again
        filter(!is.na(Alpha_Roll), Span < Min_Months * 31) # keep complete windows of consecutive months

    Disp_Roll <- Disp %>% arrange(date) %>% transmute(YM, Disp, Disp_Roll = RcppRoll::roll_mean(Disp, NSel, fill = NA, align = "right")) # the average dispersion over the same trailing window

    Alphas %>% # start from Alphas, built above
        group_by(date, YM) %>% # one row per month
        summarise(Fund_Alpha = Alpha_Roll[Group == Fund_Label][1], # the client fund's trailing alpha
                  Peer_Alpha = median(Alpha_Roll[Group == "Peer"]), # the typical peer's trailing alpha
                  .groups = "drop") %>% # drop the grouping once the summary is done
        filter(!is.na(Fund_Alpha)) %>% # only months in which the client fund has a full window
        inner_join(Disp_Roll, by = "YM") %>% # attach the trailing dispersion
        filter(!is.na(Disp_Roll)) %>% # only months with a full dispersion window
        mutate(Tercile = factor(ntile(Disp_Roll, 3), labels = c("Low dispersion", "Middle", "High dispersion"))) # split the windows into thirds by dispersion

}
