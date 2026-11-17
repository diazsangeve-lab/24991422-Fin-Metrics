# Estimates the covariance matrix and expected returns from a window of monthly returns in which some assets have shorter histories, shrinking correlations towards their average and means towards their grand mean for stability.
Cov_Mu_Estimate <- function(R, Cor_Shrink = 0.25, Mu_Shrink = 0.5){

    # For stepping through uncomment:
    # R <- R_Window ; Cor_Shrink <- 0.25 ; Mu_Shrink <- 0.5

    C <- cor(R, use = "pairwise.complete.obs") # each pair's correlation over the months both assets have returns
    C_Bar <- mean(C[upper.tri(C)]) # the average correlation across all pairs
    C_S <- (1 - Cor_Shrink) * C + Cor_Shrink * C_Bar # pull every correlation part of the way towards the average, which tames noisy estimates
    diag(C_S) <- 1 # each asset is perfectly correlated with itself
    Vols <- apply(R, 2, sd, na.rm = TRUE) # each asset's volatility over the months it has returns
    Sigma <- diag(Vols) %*% C_S %*% diag(Vols) # rebuild the covariance matrix from the shrunk correlations
    Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat) # the nearest valid covariance matrix, since pairwise estimates need not be positive definite
    dimnames(Sigma) <- list(colnames(R), colnames(R)) # label the rows and columns by asset

    Mu <- colMeans(R, na.rm = TRUE) # each asset's average monthly return
    Mu <- (1 - Mu_Shrink) * Mu + Mu_Shrink * mean(Mu) # pull every mean half way towards the grand mean, since sample means are the noisiest input

    list(Sigma = Sigma, Mu = Mu) # the two inputs to the optimisers

}
