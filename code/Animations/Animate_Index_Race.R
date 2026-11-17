# Animates the growth of one unit invested in each local index, drawing the lines month by month with each index's name and value travelling at the head of its line, and saves the result as a gif.
Animate_Index_Race <- function(Monthly, File, Cols, Title = "The Local Equity Market Since 2015", Subtitle = "", Caption = "Note:\nCalculation own", Width = 900, Height = 520, Frames = 120, Fps = 12){

    if( !all(c("date", "Name", "Returns") %in% names(Monthly)) ) stop("... Monthly must have columns called 'date', 'Name' and 'Returns'") # stop early if the returns are not in the expected shape

    D <-
        Monthly %>%
        arrange(date) %>% # order by date before compounding
        group_by(Name) %>% # compound each index on its own
        mutate(Growth = cumprod(1 + Returns)) %>% # the value of one unit invested at the start
        ungroup() # remove the grouping again

    Gap <- 0.09 * diff(range(D$Growth)) # the smallest vertical gap between two labels, so that they never overlap
    Breaks <- seq(lubridate::floor_date(min(D$date), "year"), max(D$date), by = "2 years") # date labels every two years that stop at the data

    D <-
        D %>%
        group_by(date) %>% # space the labels month by month
        arrange(Growth, .by_group = TRUE) %>% # from the lowest index to the highest
        mutate(Lab_Y = purrr::accumulate(Growth, ~max(.y, .x + Gap))) %>% # each label at its line's height, pushed up when it would touch the label below
        ungroup() # remove the grouping again

    g <-
        ggplot(D, aes(date, Growth, color = Name)) + # dates along the bottom and growth up the side
        geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") + # the starting value
        geom_line(linewidth = 1.3) + # the path of each index so far
        geom_point(size = 3) + # the head of each line
        geom_segment(aes(xend = date + 60, yend = Lab_Y), linewidth = 0.3, alpha = 0.6) + # a short leader from the head of each line to its label
        geom_text(aes(x = date + 75, y = Lab_Y, label = glue::glue("{Name}  {sprintf('%.2f', Growth)}")), hjust = 0, fontface = "bold", size = 4.2) + # the index name and its value, travelling with the head of the line and spaced so that the labels never overlap
        scale_color_manual(values = Cols, guide = "none") + # a fixed colour per index
        scale_x_date(breaks = Breaks, date_labels = "%Y", expand = expansion(mult = c(0.02, 0.24))) + # date labels that stop at the data, with room on the right for the travelling labels
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(34), subtitle.size = fmxdat::ggpts(28), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(plot.title = element_text(face = "bold")) + # a bold title
        labs(x = "", y = "Growth of one rand", title = Title, subtitle = paste(Subtitle, "{format(frame_along, '%B %Y')}"), caption = Caption) + # titles, with the current month shown in the subtitle
        gganimate::transition_reveal(date) # draw the lines through time

    A <- gganimate::animate(g, nframes = Frames, fps = Fps, width = Width, height = Height, res = 110, end_pause = 25, renderer = gganimate::gifski_renderer()) # render the frames into a gif, pausing on the final frame
    gganimate::anim_save(File, animation = A) # save the gif where the README can display it
    invisible(File) # return the file path

}
