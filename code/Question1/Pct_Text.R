# Formats a share as text such as "15 percent", so that inline numbers in the README read the same way as the typed prose.
Pct_Text <- function(x, accuracy = 1){

    if( !is.numeric(x) ) stop("... Pct_Text needs a number") # stop early if the input is not numeric

    scales::percent(x, accuracy = accuracy, suffix = " percent") # multiply by one hundred, round and add the word percent

}
