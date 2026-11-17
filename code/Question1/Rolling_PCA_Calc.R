# Runs a principal component analysis on the daily returns of the largest index constituents over a trailing window on chosen dates, and records the share of variance explained by the leading components, following Practical 4.
Rolling_PCA_Calc <- function(Constituents, Dates, Lookback = 126, Top_N = 40, Ret_Cap = 0.5, Min_Coverage = 0.9){

    Cons <- Constituents %>% mutate(Return = pmax(pmin(Return, Ret_Cap), -Ret_Cap)) # winsorise extreme daily returns
    All_Days <- sort(unique(Cons$date)) # every trading day

    map_df(Dates, function(d){ # repeat for every chosen date
        Window_Days <- tail(All_Days[All_Days <= d], Lookback) # the trailing window of trading days
        if( length(Window_Days) < Lookback ) return(NULL) # skip the date if the window is not yet full
        Top <- Cons %>% filter(date == max(Window_Days)) %>% slice_max(weight, n = Top_N, with_ties = FALSE) %>% pull(Tickers) # the largest constituents on the last day
        R <- Cons %>% filter(date %in% Window_Days, Tickers %in% Top) %>% select(date, Tickers, Return) %>% spread(Tickers, Return) %>% select(-date) # their daily returns, one column per stock
        R <- R[, colMeans(!is.na(R)) >= Min_Coverage, drop = FALSE] # keep stocks with almost complete data
        R <- R[complete.cases(R), , drop = FALSE] # keep days on which every remaining stock traded
        Eig <- prcomp(R, scale. = TRUE)$sdev^2 # the variance of each principal component, from standardised returns
        tibble(date = d, N = ncol(R), PC1 = Eig[1] / sum(Eig), PC2 = Eig[2] / sum(Eig), PC3 = Eig[3] / sum(Eig)) # the share of total variance each of the first three components explains
    })

}
