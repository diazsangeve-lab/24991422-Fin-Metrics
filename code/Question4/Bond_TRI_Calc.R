# Builds monthly total returns for constant maturity government bonds from their yields, by repricing a par bond bought at last month's yield at this month's yield and adding the coupon earned in the month.
Bond_TRI_Calc <- function(Yields, Bond_Sel, Maturity = 10, Freq = 2){

    # For stepping through uncomment:
    # Yields <- Bond_Yields ; Bond_Sel <- c("US_10Yr", "EURO_10Yr", "UK_10Yr") ; Maturity <- 10 ; Freq <- 2

    if( !all(c("date", "Name", "TRI") %in% names(Yields)) ) stop("... Yields must have columns called 'date', 'Name' and 'TRI', where TRI holds the yield in percent") # stop early if the yields are not in the expected shape

    Bond_Price <- function(y, cpn, n){ # the price per unit face value of a bond paying a coupon cpn, Freq times a year, for n remaining periods, at yield y
        r <- y / Freq # the yield per coupon period
        ifelse(abs(r) < 1e-8, cpn / Freq * n + 1, cpn / Freq * (1 - (1 + r)^(-n)) / r + (1 + r)^(-n)) # the annuity of coupons plus the discounted face value, with the zero yield case handled separately
    }

    Yields %>%
        filter(Name %in% Bond_Sel) %>% # keep only the chosen bonds
        mutate(YM = format(date, "%Y%m")) %>% # a year and month label
        group_by(Name, YM) %>% # one yield per bond per month
        summarise(date = max(date), y = dplyr::last(TRI) / 100, .groups = "drop") %>% # the month end yield as a decimal
        arrange(date) %>% # order by date before lagging
        group_by(Name) %>% # work within each bond
        mutate(Returns = Bond_Price(y, cpn = lag(y), n = (Maturity - 1 / 12) * Freq) - 1 + lag(y) / 12) %>% # the price change of last month's par bond, now a month shorter, plus a month of coupon income
        ungroup() %>% # remove the grouping again
        filter(!is.na(Returns)) %>% # the first month has no return
        transmute(date = lubridate::ceiling_date(date, "month") - 1, Name, Yield = y, Returns) # date each return at the calendar month end

}
