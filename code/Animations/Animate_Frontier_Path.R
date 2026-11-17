# Animates the efficient frontier within the mandate and under the long only constraint alone through every quarterly rebalance date, with the four portfolio rules moving along with it, and saves the result as a gif.
Animate_Frontier_Path <- function(Fr, W, Pal, File, Title = "The Efficient Frontier Through Time", Subtitle = "", Caption = "Note:\nCalculation own", Width = 900, Height = 560, Fps = 10){

    Fronts <- Fr$Frontiers %>% mutate(State = format(date, "%B %Y")) # both frontiers on every rebalance date, labelled by month
    Ports <- W %>% distinct(date, Strategy, Vol = ExAnte_Vol, Ret = ExAnte_Ret) %>% mutate(State = format(date, "%B %Y")) # the four rules on every rebalance date
    Order <- format(sort(unique(Fronts$date)), "%B %Y") # the months in time order
    Fronts <- Fronts %>% mutate(State = factor(State, levels = Order)) # keep the states in time order
    Ports <- Ports %>% mutate(State = factor(State, levels = Order)) # the same order for the portfolios

    g <-
        ggplot() + # start the plot
        geom_path(data = Fronts, aes(Vol, Ret, linetype = Frontier, group = Frontier), color = "grey15", linewidth = 1.2) + # the two frontiers on each date
        geom_point(data = Ports, aes(Vol, Ret, fill = Strategy, group = Strategy), shape = 23, size = 5.5, color = "black") + # the four rules on each date
        scale_fill_manual(values = Pal$Strategy) + # the rule colours
        scale_linetype_manual(values = c(`Within the mandate` = "solid", `Long only, no other limits` = "dashed")) + # solid for the mandate and dashed for the looser frontier
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + # volatility as a percentage
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # return as a percentage
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(34), subtitle.size = fmxdat::ggpts(28), caption.size = fmxdat::ggpts(22), CustomCaption = T) + # the clean course theme with readable text sizes
        theme(legend.position = "bottom", legend.box = "vertical") + # the legends below the plot
        labs(x = "Expected annualised volatility", y = "Expected annualised return", fill = "", linetype = "", title = Title, subtitle = paste(Subtitle, "{closest_state}"), caption = Caption) + # titles, with the current rebalance date shown in the subtitle
        gganimate::transition_states(State, transition_length = 2, state_length = 1, wrap = FALSE) + # move smoothly from one rebalance date to the next, ending on the latest date
        gganimate::ease_aes("cubic-in-out") # ease the movement in and out of each date

    A <- gganimate::animate(g, nframes = length(Order) * 6, fps = Fps, width = Width, height = Height, res = 110, end_pause = 15, renderer = gganimate::gifski_renderer()) # render the frames into a gif, six frames per rebalance date
    gganimate::anim_save(File, animation = A) # save the gif where the README can display it
    invisible(File) # return the file path

}
