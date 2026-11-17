# Builds the linear constraints of the mandate for the eligible assets, in the form quadprog needs, with weights summing to one, long only, a single asset cap, caps per asset class and a cap on global exposure.
Constraint_Build <- function(Meta_Elig, Class_Caps = c(Bonds = 0.25, Equity = 0.75, Commodity = 0.15), Single_Cap = 0.35, Global_Cap = 0.60){

    # For stepping through uncomment:
    # Meta_Elig <- Meta %>% filter(Asset %in% Eligible) ; Class_Caps <- c(Bonds = 0.25, Equity = 0.75, Commodity = 0.15) ; Single_Cap <- 0.35 ; Global_Cap <- 0.60

    n <- nrow(Meta_Elig) # the number of eligible assets
    Class_Mat <- sapply(names(Class_Caps), function(g) as.numeric(Meta_Elig$Class == g)) # one column per asset class, marking its members
    Global_Vec <- as.numeric(Meta_Elig$Region == "Global") # marks the offshore assets

    Amat <- cbind(1, # weights sum to one
                  diag(n), # every weight is at least zero
                  -diag(n), # every weight is at most the single asset cap
                  -Class_Mat, # each asset class is at most its cap
                  -Global_Vec) # offshore assets together are at most the global cap
    bvec <- c(1, rep(0, n), rep(-Single_Cap, n), -Class_Caps, -Global_Cap) # the right hand sides, in the same order

    list(Amat = Amat, bvec = bvec, meq = 1, Class_Mat = Class_Mat, Global_Vec = Global_Vec, Class_Caps = Class_Caps, Single_Cap = Single_Cap, Global_Cap = Global_Cap) # everything the optimisers need

}
