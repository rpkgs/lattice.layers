## code to prepare `DATASET` dataset goes here

z = volcano
x = 1:ncol(z)
y = 1:nrow(z)
data = expand.grid(x = x, y = y)
data$z = as.numeric(t(z))
dvolcano = data
levelplot(z~x+y, data)

usethis::use_data(dvolcano, overwrite = TRUE)
