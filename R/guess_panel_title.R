# #' @export
# panel.text2 <- function(x, y, 
#     pars,
#     ...)
# {
#     # family = get_family()
#     # panel.text(x, y, panel.title, cex = pars$title$cex, font = 2, adj = 0)
# }
guess_panel_title <- function(panel.titles_full = NULL, panel.titles = NULL, NO_begin = 1, ...) {
    not.titles_full = is.null(panel.titles_full)
    NO_panel = panel.number()
    i <- ifelse(is.null(NO_begin), 0, NO_begin - 1) + NO_panel

    title <- if (!(not.titles_full && is.null(panel.titles[NO_panel]))) {
        ifelse(not.titles_full,
            paste0("(", letters[i], ") ", panel.titles[NO_panel]),
            # paste0(letters[i], ". ", panel.titles[NO_panel]),
            panel.titles_full[NO_panel]
        )
    } else NULL
    title
}
