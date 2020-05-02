#' Panel of percentage of negative (significant negative%) and positive% (significant positive%)
#' 
#' @param z numeric vector
#' @param mask boolean vector with the same length as z, indicating whether 
#' corresponding z value is significant.
#' @param xpos,ypos The x and y position of positive and negative percentage label.
#' @param ... ignored
#' 
#' @export 
panel.sign <- function(z = NULL, mask = NULL, xpos = 0.1, ypos = 0.9, ...) {
    # val <- sign(d[[value.var]]) # 只考虑-1, 1，不考虑0
    val <- sign(z)
    val %<>% factor(c(-1, 0, 1), c("neg", NA, "pos"))
    # N   <- table(Z)
    if (!is.null(mask)) {
        mask <- mask %>% as.character() %>% factor(c("FALSE", "TRUE"))
        tbl <- table(mask, val)
        # print(tbl)
        # browser()
        N <- sum(as.numeric(tbl))
        perc <- tbl / N * 100
        # browser()
        str_neg <- sprintf("N: %.1f%% (%.1f%%)", sum(perc[, 1]), perc[2, 1])
        str_pos <- sprintf("P: %.1f%% (%.1f%%)", sum(perc[, 3]), perc[2, 3])
    } else {
        tbl <- table(val)
        N   <- sum(as.numeric(tbl))
        perc <- tbl / N * 100

        str_neg <- sprintf("N: %.1f%%", sum(perc[1]))
        str_pos <- sprintf("P: %.1f%%", sum(perc[3]))
    }

    width  <- max(stringWidth(str_neg), stringWidth(str_pos)) * 1.2
    height <- max(stringHeight(str_neg), stringHeight(str_pos))*2

    xpos <- unit(xpos, "npc")
    ypos <- unit(ypos, "npc")

    family <- get_family()
    # browser()

    grid.rect(xpos, ypos, width = width, height = height*2, just = c(0, 1), gp = gpar(col = "transparent"))
    grid.text(str_neg, 0.1, ypos, just = c(0, 1),
              name = "label_perc.neg",
              gp = gpar(col = "blue", fill = "white", fontfamily = family))
    grid.text(str_pos, 0.1, ypos - height , just = c(0, 1),
              name = "label_perc.pos",
              gp = gpar(col = "red", fill = "white", fontfamily = family))
    # data.frame(str_neg, str_pos)
}
