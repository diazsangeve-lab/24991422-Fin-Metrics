# Stratifies months by a set of state variables and compares index volatility in the high and low states with normal times.
Vol_Stratify <- function(df, States, Hi_Q = 0.8, Lo_Q = 0.2, Ann = 252){

    # For stepping through uncomment:
    # States <- State_df ; Hi_Q <- 0.8 ; Lo_Q <- 0.2

    if( !all(c("YM", "State", "Value") %in% names(States)) ) stop("... States must have columns called 'YM', 'State' and 'Value'") # stop early if the monthly state data is not in the expected shape

    df <- df %>% mutate(YM = format(date, "%Y%B")) # add a year and month label to match against the states

    States <-
        States %>%
        filter(YM %in% unique(df$YM)) %>% # only use months that are also in the return sample
        group_by(State) %>% # the cut-offs are specific to each state variable
        mutate(Regime = ifelse(Value >= quantile(Value, Hi_Q, na.rm = T), "High", # months in the top quintile of the state variable
                               ifelse(Value <= quantile(Value, Lo_Q, na.rm = T), "Low", "Normal"))) %>% # months in the bottom quintile, with everything else called normal
        ungroup() %>% # remove the grouping again
        filter(Regime != "Normal") # only the extreme months are compared with the full sample

    Full <-
        df %>%
        group_by(Name) %>% # one number per index
        summarise(Full_SD = sd(Returns, na.rm = T) * sqrt(Ann)) # the unconditional volatility of each index

    States %>%
        group_split(State, Regime) %>% # treat every state and regime combination on its own
        map_df(~df %>%
                   filter(YM %in% .x$YM) %>% # keep only the days that fall in this regime's months
                   group_by(Name) %>% # one number per index
                   summarise(SD = sd(Returns, na.rm = T) * sqrt(Ann)) %>% # volatility of the index inside the regime
                   mutate(State = unique(.x$State), Regime = unique(.x$Regime))) %>% # label the rows
        left_join(Full, by = "Name") %>% # attach the unconditional volatility
        mutate(Ratio = SD / Full_SD) # a ratio above one means the index is more volatile than normal in that regime

}
