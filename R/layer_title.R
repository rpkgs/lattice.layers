#' layer_title
#'
#' @inheritParams grid::grid.text
#' @inheritParams grid::grid.text
#'
#' @param ... other parameters to [grid.text()]
#' @export
layer_title <- function(
    x = 0, y = 1, cex = 1.4,
    labels = NULL,
    hjust = 0, vjust = 1,
    # adj = c(0, 1),
    ...)
{
    gp = gpar(fontfamily = get_family(), cex = cex)
    # dots = listk(digit, include.sd, unit, FUN)
    layer({
        panel.titles_full = parent.frame(n = 2)$panel.titles_full
        panel.titles = parent.frame(n = 2)$panel.titles
        NO_begin = parent.frame(n = 2)$NO_begin

        if (is.null(labels)) {
            label = guess_panel_title(panel.titles_full, panel.titles, NO_begin)
        } else {
            if (is.call(labels) && length(labels) == 2) {
                label = labels
            } else {
                label = labels[panel.number()]
            }
        }
        # print(label)
        # print(as.expression(label))
        text.params$label = as.expression(label)
        do.call(grid.text, text.params)
    }, data = listk(text.params = listk(x, y, hjust, vjust, gp), labels))
}

# #' @export
# panel.text2 <- function(x, y,
#     pars,
#     ...)
# {
#     # family = get_family()
#     # panel.text(x, y, panel.title, cex = pars$title$cex, font = 2, adj = 0)
# }
#' @export
guess_panel_title <- function(panel.titles_full = NULL, panel.titles = NULL, NO_begin = 1, ...) {
    not.titles_full <- is.null(panel.titles_full)
    NO_panel <- panel.number()
    i <- ifelse(is.null(NO_begin), 0, NO_begin - 1) + NO_panel

    title <- if (!(not.titles_full && is.null(panel.titles[NO_panel]))) {
        ifelse(not.titles_full,
            paste0("(", letters[i], ") ", panel.titles[NO_panel]),
            # paste0(letters[i], ". ", panel.titles[NO_panel]),
            panel.titles_full[NO_panel]
        )
    } else {
        NULL
    }
    title
}
