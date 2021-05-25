library(Ipaper)
load_all()

set_font()
data("grid_avhrr")
pars = list(title = list(x=77, y=39, cex=1.2))

trellis.par.set("add.text" = list(fontfamily = get_family(), cex = 2))
p <- sp_plot(grid_avhrr,
             pars = pars,
             interpolate = FALSE) +
    layer_barchart(x = 0.05, y = 0.15, width = 0.25) +
    layer_latFreq(zlim_ratio = c(0, 1), bbox = c(0.8, 1, 0, 1)) +
    layer_statistic(x = 0.5, y = 0.6, FUN = weightedMedian) +
    layer_signPerc(x = 0.05, y = 0.6) +
    layer_signDist()
write_fig(p, show = F)
# write_fig(p, "ex-sp_plot1.pdf", 9.8, 5)

## levelplot stype
df = grid_avhrr@data %>% data.frame() %>%
    list(a = ., b = .) %>% melt_list("type")
sp_plot(grid_avhrr, df,
              formula = X1982 ~ lon+lat | type,
              aspect = 0.5, pars = pars) +
    layer_barchart(x = 0.05, y = 0.15, width = 0.25) +
    layer_latFreq(zlim_ratio = c(0, 1), bbox = c(0.9, 1, 0, 1)) +
    layer_statistic(x = 0.5, y = 0.6, FUN = weightedMedian) +
    layer_signPerc(x = 0.05, y = 0.6) +
    layer_signDist()
# write_fig(p2 , "ex-sp_plot2.pdf", 6, 5)
## show strip
sp_plot(grid_avhrr, df, zcols = 2,
              # formula = X1982 ~ lon+lat | type,
              strip = TRUE,
              par.settings2 = list(axis.line = list(col = "black")),
              aspect = 0.5, pars = pars)
# write_fig(p3 , "ex-sp_plot3.pdf", 6, 5)
