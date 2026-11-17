# Runs a principal component analysis on the monthly returns of the funds with a complete history, and returns each fund's loading on the first two components and the variance they explain, following Practical 4.
Peer_PCA_Calc <- function(df){

    Wide <- df %>% filter(!is.na(BM)) %>% group_by(Name) %>% filter(n() == n_distinct(df$YM[!is.na(df$BM)])) %>% ungroup() %>% select(YM, Name, Returns) %>% spread(Name, Returns) # funds with a return in every month, one column per fund
    Pca <- prcomp(Wide %>% select(-YM), scale. = TRUE) # principal components of the standardised returns
    Load <- Pca$rotation[, 1:2] %*% diag(Pca$sdev[1:2]) # each fund's correlation with the first two components

    tibble(Name = rownames(Pca$rotation), PC1 = Load[, 1] * sign(mean(Load[, 1])), PC2 = Load[, 2]) %>% # the loadings, with the first component pointing the same way as the funds
        mutate(Share1 = Pca$sdev[1]^2 / sum(Pca$sdev^2), Share2 = Pca$sdev[2]^2 / sum(Pca$sdev^2), N = ncol(Wide) - 1) # the variance each component explains and the number of funds

}
