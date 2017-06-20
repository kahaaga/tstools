#RMS = readRDS("plateRMSexample.RData") %>% as.vector %>% zoo::rollmean(., 3)
data = precip %>% as.vector
surr = iaaft(data)
n = length(data)

library(ggplot2)

ts = data.frame(x = 1:length(data), y = data, surr = surr)
tsacf = data.frame(x = 1:length(data),
                y = acf(data, n - 1, plot = F)$acf,
                surr = acf(iaaft(data), n - 1, plot = F)$acf)

tsp = ggplot() +
    geom_line(data = ts, mapping = aes(x = x, y = y, col = "Original"), size = 1.2) +
    geom_line(data = ts, mapping = aes(x = x, y = surr, col = "Surrogate")) +
    theme_bw() +
    ylab("Value") +
    xlab("Time (unit step)")
    ggtitle("Time series")


acfp = ggplot() +
    geom_line(data = tsacf, aes(x = x, y = y, col = "Original"), size = 1.2) +
    geom_line(data = tsacf, aes(x = x, y = surr, col = "Surrogate")) +
    theme_bw() +
    ylab("Value") +
    xlab("Lag (time steps)") +
    ggtitle("Autocorrelation function")

cowplot::plot_grid(tsp, acfp, nrow = 2)
#
# autocorr = plot(x = 1:(n - 1), y = acf(RMS, n - 1, plot = F)$acf, type = "l", lwd=3) %>%
#     lines(x = 1:(n - 1), y = acf(RMS, n - 1, plot = F)$acf, type = "l", col = "blue")
