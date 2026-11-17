# Chooses black or white text for each background colour from its perceived brightness, so that labels drawn on coloured shapes stay legible.
Text_Contrast <- function(Cols, Threshold = 0.55){

    RGB <- grDevices::col2rgb(Cols) / 255 # each colour's red, green and blue shares
    Lum <- 0.299 * RGB[1, ] + 0.587 * RGB[2, ] + 0.114 * RGB[3, ] # perceived brightness, which weighs green most and blue least
    ifelse(Lum > Threshold, "black", "white") # dark text on light colours and light text on dark colours

}
