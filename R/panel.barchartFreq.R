#' Panel of frequency barchart
#'
#' @inheritParams lattice::panel.levelplot
#' @inheritParams lattice::panel.barchart
#' @inheritParams panel.annotation
#'
#' @param w weights for percentage, with the same length as `z[subscripts]`
#' @param yscale frequency range, `[0, 100]`
#' @param yticks yaxis ticks position, in the range of `[0, 100]`
#' @param ntick Integer, number of ticks
#' @param digit the digit in the labels
#'
#' @examples
#' \dontrun{
#' panel.barchartFreq(z, subscripts, bbox = c(0.05, 0.5, 0, 0.5), unit = "npc")
#' }
#' @export
panel.barchartFreq <- function(z, subscripts,
    bbox = c(0.05, 0.5, 0, 0.5), unit = "npc",
    at, col.regions,
    box.width = 0.8, border = "transparent",
    yscale = NULL,  yticks = NULL, ntick = NULL, w = NULL, digit = 2, ...)
{
    d <- get_perc.factor(z, subscripts, at = at, ...)
    perc <- d$perc
    xpos <- seq_along(perc)
    xpos[1] <- 1
    ypos    <- perc * 100

    yticks <- get_yticks(perc, yticks, ntick)*100
    if (is.null(yscale)) yscale = c(0, max(yticks))

    xticks <- seq_along(xpos)
    xlabels <- if (!is.null(at) & !is.factor(z)) {
        xticks  <- xticks - 0.5
        round(at, digit)
    } else levels(z)
    # ymax <- max(tick) + 0.1

    g <- as.grob(function(){
        panel.barchart(x = xpos, y = ypos, horizontal = F,
                       origin = 0,
                       reference = F, col = col.regions,
                       box.width = box.width, border = border,
                       ...)
        # panel.grid(y = yticks)
        # panel.abline(h = yticks)
        xticks[1] = 0.4
        component_axis(ticks = xticks, labels = xlabels, origin = 0, angle = 90)
        component_axis(ticks = yticks, origin = 0.4, type = "yaxis")
    })

    xscale = c(-1.9, max(xpos)+1.4)
    if (is.null(yscale)) yscale = range(ypos)
    panel.annotation(g, bbox, unit, xscale = xscale, yscale = yscale, clip = "off")
}

get_yticks <- function(perc, yticks, ntick) {
    ymax <- ceiling(max(perc) * 10) / 10
    # ymax <- round(max(perc), 1)

    if (ymax <= 0.1) {
        ntick <- 1
    } else if (ymax <= 0.2) {
        ntick <- 2
    }

    if (is.null(yticks)) {
        yticks <- if (is.null(ntick)) pretty(c(0, ymax)) else pretty(c(0, ymax), ntick)
        if (ymax >= 0.5 && ymax <= 0.6) tick <- c(0, 0.3, 0.6)
    }
    yticks
}

#' @importFrom grid unit
get_perc.factor <- function (z, subscripts, w = NULL, at, ...)
{
    z <- z[subscripts]
    if (!is.null(at) & !is.factor(z)) {
        z <- cut(z, at) #%>% as.numeric()
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
