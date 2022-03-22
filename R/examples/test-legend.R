library(grid)
library(rcolors)

{

    devtools::load_all()
    # devtools::load_all("/mnt/i/GitHub/rpkgs/lattice.layers.R")
    write_fig({
            grid.newpage()
            grid.rect()
            grid.circle()
            grid.draw(g)
    }, "a.pdf", 0.58, 5)
}

library(gtable)
library(ggplot2)


dim(p_table)

# add to the last column


{
    devtools::load_all()
    add_colorbar <- function(p, g, width = 1.4) {
        p_table <- ggplotGrob(p)
        dim = dim(p_table)

        loc = p_table$layout %>% subset(name == "panel")

        width = grid::grobWidth(g) * width
        p2 = p_table %>% gtable_add_cols(width)

        gtable_add_grob(p2, g, l = dim[2] + 1, t = loc$t, b = loc$b)
    }

    brk = c(-Inf, -1, 0, 1, 3, 6, 9, Inf)
    nbrk = length(brk) - 1
    cols = get_color(rcolors$amwg256, nbrk)



    p2 = add_colorbar(p, g, width = 1.1)
    # write_fig(p2, "a.pdf", 10, 4)
}

{
    g <- make_colorbar(
        at = brk, col = cols, cex = 1.1, height = 1,
        tck = 0.7,
        space = "right",
        # legend.text.just = c(0, 0.4),
        legend.text.just = c(0, 0.4),
        legend.text = list(fontfamily = "Times", cex = 1.1),
        hjust = 0.9
    )
    write_fig({
        grid.rect()
        grid.circle()
        grid.draw(g)
    }, "a.pdf", 2, 4)
}
# write_fig(g)
