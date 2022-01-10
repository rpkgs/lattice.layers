#' @export
layer_signPerc <- function(
    x = 0.05, y = 0.6,
    fill = "transparent",
    rm.zero = TRUE,
    # cex = 1.2, adj = c(0, 0),
    ...)
{
    # dots = listk(digit, include.sd, unit, FUN)
    dots = mget(ls())
    layer({
        ij = panel.number()
        mask = parent.frame(n = 2)$list.mask[[ij]]
        params <- listk(z = z[subscripts], mask, col.regions) %>% c(dots)
        do.call(panel.signPerc, params)
        # grid.text(label, dots3)
    }, data = listk(dots = dots))
}

#' Panel of percentage of negative (significant negative%) and positive% (significant positive%)
#' 
#' @inheritParams panel.spatial
#' @param z numeric vector
#' @param mask boolean vector with the same length as z, indicating whether
#' corresponding z value is significant.
#' @param x,y The x and y position of positive and negative percentage label.
#' @param ... ignored
#' @param fill background color
#' 
#' @examples
#' \dontrun{
#' panel.signPerc(z = NULL, mask = NULL, x = 0.1, y = 0.9, ...)
#' }
#' @export
panel.signPerc <- function(z = NULL, mask = NULL, col.regions = c("blue", "red"),
    rm.zero = TRUE,
    x = 0.05, y = 0.8, fill = "transparent")
{
    # val <- sign(d[[value.var]]) # 只考虑-1, 1，不考虑0
    val <- sign(z)
    val %<>% factor(c(-1, 0, 1), c("neg", NA, "pos"))
    # N   <- table(Z)
    if (!is.null(mask)) {
        mask <- mask %>% as.character() %>% factor(c("FALSE", "TRUE"))
        tbl <- table(mask, val)
        N <- ifelse(rm.zero, sum(as.numeric(tbl[, -2])), sum(as.numeric(tbl)))
        perc <- tbl / N * 100
        str_neg <- sprintf("N: %.1f%% (%.1f%%)", sum(perc[, 1]), perc[2, 1])
        str_pos <- sprintf("P: %.1f%% (%.1f%%)", sum(perc[, 3]), perc[2, 3])
    } else {
        tbl <- table(val)
        # info = data.table(type = rownames(tbl), N = as.numeric(tbl))[!is.na(type), ]
        # browser()
        N <- ifelse(rm.zero, sum(as.numeric(tbl)[-2]), sum(as.numeric(tbl)))
        perc <- tbl / N * 100
        str_neg <- sprintf("N: %.1f%%", sum(perc[1]))
        str_pos <- sprintf("P: %.1f%%", sum(perc[3]))
    }
    # print(tbl)
    # glue("{str_neg}, {str_pos}")
    # listk(str_neg, str_pos) %>% str() %>% print() # debug
    width  <- max(stringWidth(str_neg), stringWidth(str_pos))
    height <- max(stringHeight(str_neg), stringHeight(str_pos))*2

    x <- unit(x, "npc")
    y <- unit(y, "npc")

    family <- get_family()
    grid.rect(x, y, width = width*0.94, height = height*2, just = c(0, 1),
        gp = gpar(col = "transparent", fill = fill))

    ncolors <- length(col.regions)
    col.neg = col.regions[1]
    col.pos = col.regions[ncolors]
    if (ncolors >= 6) {
        col.neg = col.regions[2]
        col.pos = col.regions[ncolors-1]
    }

    y = y - height*0.2
    grid.text(str_neg, x, y, just = c(0, 1),
              name = "label_perc.neg",
              gp = gpar(col = col.neg, fill = "transparent", fontfamily = .options$family))
    grid.text(str_pos, x, y - height , just = c(0, 1),
              name = "label_perc.pos",
              gp = gpar(col = col.pos, fill = "transparent", fontfamily = .options$family))
    # data.frame(str_neg, str_pos)
}
