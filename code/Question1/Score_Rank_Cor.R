# Calculates the monthly cross-sectional rank correlation between pairs of score columns, to test whether two scores rank stocks the same way.
Score_Rank_Cor <- function(df, Pairs){

    # For stepping through uncomment:
    # Pairs <- tibble(X = c("Momentum_Own"), Y = c("Momentum"), Label = c("Own 6-1 momentum vs vendor momentum"))

    if( !all(c("X", "Y", "Label") %in% names(Pairs)) ) stop("... Pairs must have columns called 'X', 'Y' and 'Label'") # stop early if the pairs are not in the expected shape

    Pairs %>%
        pmap_df(function(X, Y, Label){ # repeat for every pair of scores
            df %>%
                filter(!is.na(.data[[X]]), !is.na(.data[[Y]])) %>% # only stocks that have both scores
                group_by(date) %>% # one correlation per month
                summarise(Rho = cor(.data[[X]], .data[[Y]], method = "spearman"), N = n(), .groups = "drop") %>% # the Spearman rank correlation, which only cares about the ordering of stocks
                mutate(Label = Label) # label the pair
        })

}
