library(dplyr)
data("grid_avhrr")
grid <- grid_avhrr
SpatialPixel <- grid
show <- FALSE

stat <- list(show = TRUE, name = "mean", loc = c(82.5, 26.5), digit = 1, include.sd = TRUE)
pars <- list(
  title = list(x = 77, y = 39, cex = 1.5),
  hist = list(origin.x = 77, origin.y = 28, A = 12, by = 0.4)
)

test_that("sp_plot, stat", {
  # 1. spplot_grid
  expect_true({
    p <- sp_plot(grid_avhrr, stat = stat, interpolate = FALSE)
    write_fig(p, "ex-spplot_grid.pdf", 10, 7, show = show)
    TRUE
  })
})

test_that("sp_plot, levelplot2", {
  expect_true({
    d <- grid@data
    df <- list(x = d, y = d) %>%
      melt_list("type") %>%
      data.table()
    df <- list(a = df, b = df) %>%
      melt_list("kind") %>%
      mutate(mask = (X1982 > 270 | X1982 < 240))

    p <- sp_plot(grid_avhrr, df,
      formula = X1982 ~ lon + lat | type + kind,
      # df.mask,
      aspect = 0.75,
      NO_begin = 3,
      stat = stat, pars = pars,
      density = 0.5, interpolate = FALSE
    )
    write_fig(p, "ex-levelplot2.svg", 10, 7, show = show)
    TRUE
  })
})

# 3. levelplot-shade
test_that("sp_plot, levelplot2", {
  expect_true({
    # data <- coordinates(grid) %>% as.data.table() %>% cbind(grid@data)
    df <- grid@data %>%
      data.table() %>%
      mutate(mask = (X1982 > 270 | X1982 < 240))
    df.mask <- df[, .(mask)]
    p <- sp_plot(grid, df,
      formula = X1982 ~ lon + lat,
      df.mask = df.mask,
      aspect = 0.75,
      NO_begin = 3,
      stat = stat, pars = pars,
      density = 1, interpolate = FALSE
    )
    write_fig(p, "ex-levelplot2.pdf", show = show)
    TRUE
  })
})
