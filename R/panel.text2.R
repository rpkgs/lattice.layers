#' @export
panel.text2 <- function(x, y, panel.titles_full = NULL, panel.tiles = NULL, NO_begin = 1, ...) {
    not.titles_full = is.null(panel.titles_full)
    
    NO_panel = panel.number()
    i <- ifelse(is.null(NO_begin), 0, NO_begin - 1) + NO_panel
    
    if (!(not.titles_full && is.null(panel.titles[NO_panel]))) {
        panel.title <- ifelse(not.titles_full,
            paste0("(", letters[i], ") ", panel.titles[NO_panel]),
            panel.titles_full[NO_panel]
        )
        panel.text(x, y, panel.title, # english name: New_names[i])
            fontfamily = "Times", cex = pars$title$cex, font = 2, adj = 0
        )
    }
}
