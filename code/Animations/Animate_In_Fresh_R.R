# Runs one of the animation functions in a fresh R session, since gganimate stalls when it renders inside a knitting document, and returns the path of the saved gif.
Animate_In_Fresh_R <- function(Fn, Args, Code_Dir = "code/"){

    callr::r(function(Fn, Args, Code_Dir){ # the work done in the separate session
        suppressMessages(library(tidyverse)) # the pipe and dplyr verbs the functions use
        invisible(lapply(list.files(Code_Dir, full.names = TRUE, recursive = TRUE, pattern = "\\.R$"), source)) # source every function in the code folder
        do.call(Fn, Args) # call the chosen animation function with its arguments
    }, args = list(Fn = Fn, Args = Args, Code_Dir = normalizePath(Code_Dir)), wd = getwd()) # pass the inputs across and keep the same working directory

}
