# Draws the correlations between assets as a chord diagram, linking pairs whose correlation is strong, with link width set by its strength, blue links for positive and red links for negative correlations.
Plot_Cor_Chord <- function(R_Long, Pal, Est_Start = lubridate::ymd(20100101), Pos_Threshold = 0.5, Neg_Threshold = -0.15, Title = "Correlation Links Across the Universe"){

    C <- R_Long %>% filter(date >= Est_Start) %>% select(date, Asset, Returns) %>% spread(Asset, Returns) %>% select(-date) %>% cor(use = "pairwise.complete.obs") # pairwise correlations of monthly returns since the estimation start
    C <- C[names(Pal$Asset), names(Pal$Asset)] # assets in family order around the circle
    Links <- C # a copy that will hold only the links drawn
    Links[(C < Pos_Threshold & C > Neg_Threshold) | row(C) == col(C)] <- 0 # keep only strong positive and notable negative links

    Alpha <- 0.25 + 0.6 * pmax(as.vector(Links) - Pos_Threshold, 0) / (1 - Pos_Threshold) # stronger positive links are drawn more opaque
    Link_Col <- matrix(ifelse(as.vector(Links) > 0, mapply(grDevices::adjustcolor, "#2171B5", alpha.f = Alpha), grDevices::adjustcolor("firebrick", alpha.f = 0.75)), nrow = nrow(Links), dimnames = dimnames(Links)) # deeper blue for stronger positive links and red for negative links, one colour per pair

    circlize::circos.clear() # reset any earlier circular layout
    circlize::circos.par(start.degree = 90, gap.degree = 2, track.margin = c(0.01, 0.01)) # start at the top with small gaps between assets
    Linked <- names(Pal$Asset)[rowSums(abs(Links)) > 0] # assets with at least one link, as the others cannot be drawn
    circlize::chordDiagram(abs(Links[Linked, Linked]), symmetric = TRUE, order = Linked, grid.col = Pal$Asset[Linked], col = Link_Col[Linked, Linked], transparency = 0, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.25), link.border = NA) # the chord diagram, with link widths set by the strength of each correlation
    circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) { # label each asset around the outside
        circlize::circos.text(circlize::CELL_META$xcenter, circlize::CELL_META$ylim[1] + 0.1, circlize::CELL_META$sector.index, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.62, font = 2, col = Pal$Asset[circlize::CELL_META$sector.index]) # the asset name in its own colour
    }, bg.border = NA) # no border around the label track
    graphics::title(Title, cex.main = 1, font.main = 2, adj = 0) # the title at the top left
    graphics::legend("bottomleft", legend = c(glue::glue("Correlation above {Pos_Threshold}"), glue::glue("Correlation below {Neg_Threshold}")), fill = c("#2171B5", "firebrick"), border = NA, bty = "n", cex = 0.7) # explain the link colours
    circlize::circos.clear() # leave the layout clean for later plots

    invisible(C) # return the correlation matrix for use in the text

}
