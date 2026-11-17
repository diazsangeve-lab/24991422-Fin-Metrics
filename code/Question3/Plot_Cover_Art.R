# Draws a minimal version of the peer fan chart with no axes or text, used as decorative artwork on the title slide of the presentation.
Plot_Cover_Art <- function(Pct){

    D <- Pct %>% filter(!is.na(Fund_Ret)) # the months in which the fund has a full window

    ggplot(D) + # start the plot
        geom_ribbon(aes(date, ymin = P05, ymax = P95), fill = "#5B8DB8", alpha = 0.25) + # the middle 90 percent of peers
        geom_ribbon(aes(date, ymin = P25, ymax = P75), fill = "#5B8DB8", alpha = 0.45) + # the middle half of peers
        geom_line(aes(date, P50), color = "#A9C6E0", linewidth = 0.8) + # the median peer
        geom_line(aes(date, Fund_Ret), color = "#F28C28", linewidth = 2) + # the client fund
        theme_void() + # no axes, grid or text
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_rect(fill = "transparent", color = NA)) # a transparent background so that the slide colour shows through

}
