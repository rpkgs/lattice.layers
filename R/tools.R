
alpha <- function(colour, alpha = NA) {
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))
    } else {
      colour <- rep(colour, length.out = length(alpha))
    }
  }
  rgb <- farver::decode_colour(colour, alpha = TRUE)
  rgb[!is.na(alpha), 4] <- alpha[!is.na(alpha)]
  farver::encode_colour(rgb, rgb[, 4])
}

clamp2 <- function(x, lims = c(0, 1), fill.na = FALSE) {
  if (fill.na) {
    x[x < lims[1]] <- NA_real_
    x[x > lims[2]] <- NA_real_
  } else {
    x[x < lims[1]] <- lims[1]
    x[x > lims[2]] <- lims[2]
  }
  x
}

# for sp_plot
#' @importFrom stringr str_extract str_split
parse.formula <- function(formula = x ~ s1 + s2 | b1 + b2) {
  str_formula <- as.character(formula)
  # str_formula[3] %>% str_extract("(?<=|).*")
  # str_formula <- gsub("s1 \\+ s2 *\\|*| ", "", ) %>%
  #     gsub("Var1 \\+ Var2 *\\|*| ", "", .)
  value.var <- str_formula[2]
  groups <- str_formula[3] %>%
    str_extract("(?<=\\| ).*") %>%
    gsub(" ", "", .) %>%
    str_split("\\+") %>%
    .[[1]]
  if (length(groups) == 1 && is.na(groups)) groups <- NULL
  list(value.var = value.var, groups = groups)
}

#' help of gpar
#'
#' @keywords internal
#' @export
help_gpar <- function() {
  list(
    col = "Colour for lines and borders.",
    fill = "Colour for filling rectangles, polygons, ...",
    alpha = "Alpha channel for transparency",
    cex = "Multiplier applied to fontsize",
    lty = "Line type",
    lwd = "Line width",
    lex = "Multiplier applied to line width",
    lineend = "Line end style (round, butt, square",
    linejoin = "Line join style (round, mitre, bevel",
    linemitre = "Line mitre limit (number greater than 1",
    lineheight = "The height of a line as a multiple of the size of text",
    fontsize = "The size of text (in points",
    fontfamily = "The font family",
    fontface = "The font face (bold, italic, ...",
    font = "Font face (alias for fontface; for backward compatibility"
  ) %>% str()
}

asign_func <- function(func_old, func_new) {
  name <- deparse(substitute(func_new))
  pkg <- str_extract(name, ".*(?=:)") %>% gsub(":", "", .)
  funcname <- str_extract(name, "(?<=:).*") %>% gsub(":", "", .)

  temp <- func_new
  cmd1 <- sprintf("environment(temp) <- environment(%s:::%s)", pkg, funcname)
  cmd2 <- sprintf('assignInNamespace("%s", temp, ns="%s")', funcname, pkg)
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
}


#' @export
dev_off <- function() {
  tryCatch({
    while (TRUE) {
      cat("closed\n")
      dev.off()
    }
  }, error = function(e) {
    return(invisible())
    # message(sprintf('%s', e))
  })
}

round_decade <- function(x) {
  p <- floor(log10(abs(x)))
  if (x > 1000) p <- p - 1
  times <- 10^p
  round(x / times) * times
}

# ' label_tag
# '
# ' @param labels character vector or expression vector
# ' @param tag boolean
# '
# ' @examples
# ' label_tag(1:5)
# ' char2expr(1:5)
# ' @export
label_tag <- function(labels, tag = TRUE, letter_begin = 1) {
  n <- length(labels)
  sapply(seq_along(labels), function(i) {
    name <- labels[[i]]
    data <- list(tag = letters[i + letter_begin - 1], x = name)
    if (tag) {
      eval(substitute(expression(bold("(" * tag * ")" ~ x)), data))
      # eval(substitute(expression(bold(tag * ". " ~ x)), data))
    } else {
      eval(substitute(expression(bold(x)), data))
    }
  })
  # sprintf("(%s) %s", letters[1:n], labels)
}


#' @keywords internal
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
