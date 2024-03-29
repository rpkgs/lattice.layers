% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer_contourf.R
\name{layer_contourf}
\alias{layer_contourf}
\title{layer of contourf}
\usage{
layer_contourf(
  at = NULL,
  col = NULL,
  col.regions = NULL,
  contour = TRUE,
  region = FALSE,
  labels = FALSE,
  label.style = c("mixed", "flat", "align"),
  format = "\%f",
  ...
)
}
\arguments{
\item{at}{
    Numeric vector giving breakpoints along the range of
    \code{z}.  See \code{\link[lattice]{levelplot}} for details.
  }

\item{col.regions}{
    A vector of colors, or a function to produce a vecor of colors, to
    be used if \code{region=TRUE}.  Each interval defined by \code{at}
    is assigned a color, so the number of colors actually used is one
    less than the length of \code{at}.  See \code{\link[lattice]{level.colors}}
    for details on how the color assignment is done.
  }

\item{contour}{
    A logical flag, specifying whether contour lines should be drawn.
  }

\item{region}{
    A logical flag, specifying whether inter-contour regions should be
    filled with appropriately colored rectangles.
  }

\item{labels}{
    Either a logical scalar indicating whether the labels are to be
    drawn, or a character or expression vector giving the labels
    associated with the \code{at} values.  Alternatively, \code{labels}
    can be a list with the following components:
    
    \describe{
    
      \item{\code{labels}:}{ a character or expression vector giving the
	labels.  This can be omitted, in which case the defaults will be
	used. }

      \item{\code{col, cex, alpha}:}{ graphical parameters for label
	texts }

      \item{\code{fontfamily, fontface, font}:}{ font used for the
	labels }
    }
    
  }

\item{label.style}{
    Controls how label positions and rotation are determined.  A value
    of \code{"flat"} causes the label to be positioned where the contour
    is flattest, and the label is not rotated.  A value of
    \code{"align"} causes the label to be drawn as far from the
    boundaries as possible, and the label is rotated to align with the
    contour at that point.  The default is to mix these approaches,
    preferring the flattest location unless it is too close to the
    boundaries.
  }

\item{...}{other parameters to \code{\link[lattice:panel.levelplot]{lattice::panel.levelplot()}}}
}
\description{
layer of contourf
}
\note{
\itemize{
\item the parameter \code{brks} was renamed as \code{at}
}
}
\examples{
cols <- get_color("Blues")

print(lattice(z ~ x * y, dvolcano) +
    layer_contourf(
        labels = TRUE, format = "\%.0f",
        col = "black", lwd = 0.2, lty = 2, region = FALSE))
print(lattice(z ~ x * y, dvolcano) +
    layer_contourf(labels = TRUE, format = "\%.0f", col = cols, region = FALSE))

print(lattice(z ~ x * y, dvolcano) +
    layer_contourf(labels = TRUE, format = "\%.0f", region = FALSE))

print(lattice(z ~ x * y, dvolcano) +
    layer_contourf(labels = TRUE, format = "\%.1f", region = TRUE))
}
\seealso{
\code{\link[lattice:panel.levelplot]{lattice::panel.levelplot()}}
}
