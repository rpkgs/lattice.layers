#' Panel of percentage of negative (significant negative%) and positive% (significant positive%)
#'
#' @inheritParams panel.spatial
#' @param z numeric vector
#' @param mask boolean vector with the same length as z, indicating whether
#' corresponding z value is significant.
#' @param xpos,ypos The x and y position of positive and negative percentage label.
#' @param ... ignored
#'
#' @examples
#' \dontrun{
#' panel.signPerc(z = NULL, mask = NULL, xpos = 0.1, ypos = 0.9, ...)
#' }
#' @export
panel.signPerc <- function(z = NULL, mask = NULL, xpos = 0.1, ypos = 0.9,
    col.regions = c("blue", "red"), ...)
{
    # val <- sign(d[[value.var]]) # 只考虑-1, 1，不考虑0
    val <- sign(z)
    val %<>% factor(c(-1, 0, 1), c("neg", NA, "pos"))
    # N   <- table(Z)
    if (!is.null(mask)) {
        mask <- mask %>% as.character() %>% factor(c("FALSE", "TRUE"))
        tbl <- table(mask, val)
        # print(tbl)
        N <- sum(as.numeric(tbl))
        perc <- tbl / N * 100
        str_neg <- sprintf("N: %.1f%% (%.1f%%)", sum(perc[, 1]), perc[2, 1])
        str_pos <- sprintf("P: %.1f%% (%.1f%%)", sum(perc[, 3]), perc[2, 3])
    } else {
        tbl <- table(val)
        N   <- sum(as.numeric(tbl))
        perc <- tbl / N * 100

        str_neg <- sprintf("N: %.1f%%", sum(perc[1]))
        str_pos <- sprintf("P: %.1f%%", sum(perc[3]))
    }
    # listk(str_neg, str_pos) %>% str() %>% print() # debug
    width  <- max(stringWidth(str_neg), stringWidth(str_pos)) * 1.2
    height <- max(stringHeight(str_neg), stringHeight(str_pos))*2

    xpos <- unit(xpos, "npc")
    ypos <- unit(ypos, "npc")

    family <- get_family()
    grid.rect(xpos, ypos, width = width*(1/1.2), height = height*2, just = c(0, 1), gp = gpar(col = "transparent"))

    ncolors <- length(col.regions)
    col.neg = col.regions[1]
    col.pos = col.regions[ncolors]
    if (ncolors >= 6) {
        col.neg = col.regions[2]
        col.pos = col.regions[ncolors-1]
    }

    grid.text(str_neg, xpos, ypos, just = c(0, 1),
              name = "label_perc.neg",
              gp = gpar(col = col.neg, fill = "transparent", fontfamily = .options$family))
    grid.text(str_pos, xpos, ypos - height , just = c(0, 1),
              name = "label_perc.pos",
              gp = gpar(col = col.pos, fill = "transparent", fontfamily = .options$family))
    # data.frame(str_neg, str_pos)
}
