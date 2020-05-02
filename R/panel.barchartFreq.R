#' Panel of frequency barchart
#'
#' @inheritParams lattice::levelplot
#' @inheritParams panel.annotation
#'
#' @inheritParams lattice::panel.barchart
#' @param yscale frequency range, `[0, 100]`
#' @param yticks yaxis ticks position
#'
#' @export
panel.barchartFreq <- function(z, subscripts, 
    bbox = c(0.05, 0.5, 0, 0.5),
    at, col.regions,
    box.width = 0.8, border = "transparent",
    yscale = NULL,  yticks = NULL, w = NULL, ...)
{
    d <- get_perc.factor(z, subscripts, at = at, ...)
    perc <- d$perc
    xpos <- seq_along(perc)
    xpos[1] <- 1
    ypos <- perc * 100

    if (is.null(yticks)) { yticks = pretty(ypos) }

    g <- as.grob(function(){
        panel.barchart(x = xpos, y = ypos, horizontal = F,
                       origin = 0,
                       reference = F, col = col.regions,
                       box.width = box.width, border = border,
                       ...)
        xticks = seq_along(xpos)
        xticks[1] = 0.4
        component_axis(ticks = xticks, origin = 0)
        component_axis(ticks = yticks, origin = 0.4, type = "yaxis")
    })

    xscale = c(-1.9, max(xpos)+1.4)
    if (is.null(yscale)) yscale = range(ypos)
    panel.annotation(g, bbox, xscale = xscale, yscale = yscale, clip = "off")
}

#' @importFrom grid unit
get_perc.factor <- function (z, subscripts, w = NULL, at, ...)
{
    z <- z[subscripts]
    if (!is.null(at) & !is.factor(z)) {
        z <- cut(z, at) %>% as.numeric()
    }

    if (is.null(w)) {
        z <- z[!is.na(z)]
        d <- table(z) %>% as.data.frame()
        d$perc <- d$Freq/length(z)
    } else {
        df <- data.table(z, w)
        w_sum <- df[!is.na(z), sum(w)]
        d <- df[!is.na(z), .(Freq = .N, perc = sum(w)/w_sum),
            .(z)][order(z)]
    }

    zchr = d$z %<>% as.character()
    if (is.na(suppressWarnings(as.numeric(zchr[1])))) {
        d$z <- seq_along(zchr)
    }
    else {
        d$z <- as.numeric(zchr)
    }
    n_missing <- d$z[1] - 1
    if (n_missing > 0) {
        d <- rbind(data.frame(z = 1:n_missing, Freq = 0, perc = 0), d)
    }
    d
}
