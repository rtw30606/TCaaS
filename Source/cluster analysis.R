# https://cran.r-project.org/web/packages/Ckmeans.1d.dp/vignettes/Ckmeans.1d.dp.html
# Occupancy analysis
# Cluster into two groups

# measure ppm on Sunday
ppmSunday <- Measure |> 
  mutate(mday = wday(datetime)) |>
  filter(mday == 7) |> 
  summarize(mean = mean(ppm), sd = sd(ppm))
cutpoint <- ppmSunday$mean + 2*ppmSunday$sd

k <- 2
x <- Measure$ppm
result <- Ckmeans.1d.dp(x, k)
plot(result)
# Report cluster analysis
cluster.analysis <- tibble(cluster$centers, cluster$size)
colnames(cluster.analysis) <- c('Center', 'Size')
cluster.analysis <- arrange(cluster.analysis, by = Center)
table <- gt(cluster.analysis) |> 
  tab_header(
    title = md("**Cluster analysis**")) |>
    fmt_number(decimals = 0)
table


