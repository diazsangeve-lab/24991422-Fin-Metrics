# Compares the annualised return and volatility of each index in the high and low quintile months of a set of state variables, following the stratification practical.
Size_Stratify <- function(Monthly_Rets, States, Hi_Q = 0.8, Lo_Q = 0.2){ # define Size_Stratify, with inputs Monthly_Rets, States, Hi_Q and Lo_Q

    # For stepping through uncomment:
    # Monthly_Rets <- Size_Monthly ; States <- Size_States

    if( !all(c("YM", "Name", "Returns") %in% names(Monthly_Rets)) ) stop("... Monthly_Rets must have columns called 'YM', 'Name' and 'Returns'") # stop early if the returns are not in the expected shape
    if( !all(c("YM", "State", "Value") %in% names(States)) ) stop("... States must have columns called 'YM', 'State' and 'Value'") # stop early if the states are not in the expected shape

    States %>% # start from the state variables
        filter(YM %in% unique(Monthly_Rets$YM)) %>% # only months in the return sample
        group_by(State) %>% # cut-offs are specific to each state variable
        mutate(Regime = ifelse(Value >= quantile(Value, Hi_Q, na.rm = T), "High", ifelse(Value <= quantile(Value, Lo_Q, na.rm = T), "Low", "Normal"))) %>% # top quintile, bottom quintile and everything else
        ungroup() %>% # remove the grouping again
        filter(Regime != "Normal") %>% # only the extreme months are compared
        inner_join(Monthly_Rets, by = "YM", relationship = "many-to-many") %>% # attach every index's return in those months, which is many to many by design
        group_by(State, Regime, Name) %>% # one set of numbers per state, regime and index
        summarise(Ret_Ann = (prod(1 + Returns))^(12 / n()) - 1, # the annualised geometric return in those months
                  Vol_Ann = sd(Returns) * sqrt(12), # the annualised volatility in those months
                  Months = n(), # how many months fall in the regime
                  .groups = "drop") # drop the grouping once the summary is done

}
