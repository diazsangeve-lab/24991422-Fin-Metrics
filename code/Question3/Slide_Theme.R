# Adds larger text to the course theme so that figures stay readable when projected on a slide.
Slide_Theme <- function(Base = 15){ # define Slide_Theme, with input Base

    list(fmxdat::theme_fmx(title.size = fmxdat::ggpts(34), subtitle.size = fmxdat::ggpts(28), caption.size = fmxdat::ggpts(22), CustomCaption = T), # the clean course theme with slide sized titles
         theme(axis.text = element_text(size = Base - 2), # larger tick labels
               axis.title = element_text(size = Base), # larger axis titles
               legend.text = element_text(size = Base - 2), # larger legend text
               strip.text = element_text(size = Base, face = "bold"))) # larger panel labels

}
