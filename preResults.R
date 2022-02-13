setwd("C:/Users/55219/Desktop/2022/CovidLP/BinomialNegativa")

library(PandemicLP)
library(stringr)

# Reading data
p = "Russia"
ex = "n"
mod = readRDS(url(paste0("https://github.com/CovidLP/Methodology/raw/main/AppChains/", p, "_", ex, ".rds")))

# Reading and predicting
dados = mod$Y

# Poison
pred.poi <- posterior_predict(mod, horizonLong = 30, horizonShort = 1)
stats.poi <- pandemic_stats(pred.poi)
int.poi <- apply(pred.poi$pastMu, 2, function(x) quantile(x, c(0.025, 0.975)))

poi = mod$fit@sim$samples

par(mfrow = c(2, 3), mar = c(4.5, 4.5, 0, 0))
ts.plot(poi[[1]]$`mu[100]`[ind], col = "red")
ts.plot(poi[[1]]$`mu[200]`[ind], col = "red")
ts.plot(poi[[1]]$`mu[300]`[ind], col = "red")
ts.plot(poi[[1]]$`mu[400]`[ind], col = "red")
ts.plot(poi[[1]]$`mu[500]`[ind], col = "red")
ts.plot(poi[[1]]$`mu[600]`[ind], col = "red")

# Negative Binomial
load(paste0(str_replace_all(p, " ", "-"), "_", ex, "_negbin.RData"))

pred.neg <- posterior_predict(mod, horizonLong = 30, horizonShort = 1)
stats.neg <- pandemic_stats(pred.neg)
int.neg <- apply(pred.neg$pastMu, 2, function(x) quantile(x, c(0.025, 0.975)))

neg = mod$fit@sim$samples

n = length(neg[[1]]$`mu[100]`)

ind = seq(10e3, 10e3 + 5e3)
par(mfrow = c(2, 3), mar = c(4.5, 4.5, 0, 0))
ts.plot(neg[[1]]$`mu[100]`[ind], col = "red")
lines(neg[[2]]$`mu[100]`[ind], col = "blue")
ts.plot(neg[[1]]$`mu[200]`[ind], col = "red")
lines(neg[[2]]$`mu[200]`[ind], col = "blue")
ts.plot(neg[[1]]$`mu[300]`[ind], col = "red")
lines(neg[[2]]$`mu[300]`[ind], col = "blue")
ts.plot(neg[[1]]$`mu[400]`[ind], col = "red")
lines(neg[[2]]$`mu[400]`[ind], col = "blue")
ts.plot(neg[[1]]$`mu[500]`[ind], col = "red")
lines(neg[[2]]$`mu[500]`[ind], col = "blue")
ts.plot(neg[[1]]$`mu[600]`[ind], col = "red")
lines(neg[[2]]$`mu[600]`[ind], col = "blue")

# Saving
stats.list = list(stats.neg = stats.neg,
                  int.neg = int.neg,
                  stats.poi = stats.poi,
                  int.poi = int.poi,
                  data = dados)
save(stats.list, file = paste0(str_replace_all(p, " ", "-"), "_", ex, "_stats.RData"))
