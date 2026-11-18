# Darkens colours by a chosen share, so that text drawn in a light series colour stays legible on a white background.
Darken_Col <- function(Cols, Amount = 0.45){ # define Darken_Col, with inputs Cols and Amount

    RGB <- grDevices::col2rgb(Cols) / 255 # each colour's red, green and blue shares
    Out <- grDevices::rgb(t(RGB * (1 - Amount))) # move every channel towards black
    stats::setNames(Out, names(Cols)) # keep the original names

}
