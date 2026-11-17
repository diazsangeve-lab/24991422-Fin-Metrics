# Pulls a single value from a summary table and formats it for use as inline R code in the README prose, so that every number in the text is computed rather than typed.
Fact_Pull <- function(df, Key, Col, Key_Col = "Name", Pct = NULL, Digits = 2, Ordinal = FALSE){

    # For stepping through uncomment:
    # df <- Garch_Facts ; Key <- "Capped SWIX" ; Col <- "Avg_2026" ; Key_Col <- "Name" ; Pct <- 0.1

    if( !Key_Col %in% names(df) ) stop(glue::glue("... the key column {Key_Col} is not in the data frame provided")) # stop early if the key column is missing
    if( !Col %in% names(df) ) stop(glue::glue("... the column {Col} is not in the data frame provided")) # stop early if the value column is missing

    x <- df %>% filter(.data[[Key_Col]] == Key) %>% pull(all_of(Col)) # the value in the row whose key matches

    if( length(x) != 1 ) stop(glue::glue("... expected exactly one value for {Key} in {Col}, but found {length(x)}")) # a key must identify exactly one row

    if( is.character(x) | is.factor(x) ) return(as.character(x)) # text values are returned as they are
    if( Ordinal ) return(scales::ordinal(round(x))) # ranks are returned as first, second, third and so on
    if( !is.null(Pct) ) return(scales::percent(x, accuracy = Pct)) # shares are returned as percentages
    sprintf(paste0("%.", Digits, "f"), x) # everything else is returned with the chosen number of decimals

}
