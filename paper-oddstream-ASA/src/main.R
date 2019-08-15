### Supplemental Materials (Data and scripts)
###  Anomaly Detection in Streaming Nonstationary Temporal Data







## ---- load
library(tidyverse)
library(gridExtra)
library(ggpubr)
# install.packages("devtools")
# devtools::install_github("pridiltal/oddstream")
library(oddstream)

# loading data
anomalous_stream <- load("data/anomalous_stream_1.rda")







## ---- mvtsplot1
# Figure 1 : Multiple parallel time series plot of a real world data set
# obtained using a fiber optic cable
t <- nrow(anomalous_stream_1)
f <- ncol(anomalous_stream_1)
g <- as_tibble(anomalous_stream_1) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, 0.1, max(anomalous_stream_1))
  ) +
  ylab("Cable (Sensor ID / Time Series ID)") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  xlab("Time") +
  theme(legend.position = "none", axis.title = element_text(size = 15))
print(p1)







## ---- outtype
# Figure 2: Different types of anomalies in temporal data
set.seed(1234)
ts1 <- as.ts(rnorm(500, 10, 3))
ts1[300] <- 35
ts1[150] <- 30
ts1 <- as_tibble(ts1) %>% mutate(Time = 1:500)
colnames(ts1) <- c("Value", "Time")
point <- tibble(Time = c(150, 300), Value = c(30, 35))
p1 <- ggplot(ts1) +
  geom_line(aes(x = Time, y = Value), size = 0.5, alpha = 0.7) +
  geom_point(data = point, aes(x = Time, y = Value), col = "red", size = 2, alpha = 0.7) +
  ggtitle("(a) Contextual anomalies within a  given series") +
  expand_limits(y = c(-0, 40))

ts2 <- as.ts(rnorm(500, 10, 3))
ts2[250:325] <- rnorm(325:250, 40, 4)
ts2 <- as_tibble(ts2) %>% mutate(Time = 1:500)
colnames(ts2) <- c("Value", "Time")
point <- ts2[249:326, ]
p2 <- ggplot(ts2) +
  geom_line(aes(x = Time, y = Value), size = 0.5, alpha = 0.7) +
  geom_line(data = point, aes(x = Time, y = Value), col = "red", size = .5, alpha = 0.7) +
  ggtitle("(b) Anomalous sub-sequences within a given series")
# Data Generation
nobs <- 500
nts <- 20
tsframe <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe[, i] <- 10 + rnorm(nobs, 0, .5) # adding noise
}
tsframe[200:500, 15] <- 5 + 1 * (200:500)^(1 / 3) + rnorm(301, 4, 1)
tsframe[1:200, 15] <- tsframe[1:200, 15] + rnorm(200, 2, 1)
data_melt <- reshape::melt(as.matrix(tsframe))
data_melt <- dplyr::mutate(data_melt, type = ifelse(X2 %in% "Series 15", "Outlier", "Typical"))
p3 <- ggplot(data_melt) +
  geom_line(
    aes_(x = ~X1, y = ~value, group = ~X2, color = ~type),
    alpha = 0.6, size = 0.5
  ) +
  scale_colour_manual(
    name = "Type",
    values = c("Typical" = "black", "Outlier" = "red")
  ) +
  xlab("Time") +
  ylab("Value") +
  ggtitle("(c) Anomalous series within a space of a collection of series (plot contains 20 time series)") + theme(legend.position = "")
grid.arrange(p1, p2, p3, ncol = 1)







## ---- EVDchange
# Figure 3 : Extreme value distributions with respect to m
## standard normal
set.seed(100)
m <- c(1, 10, 100, 1000)
max_m <- list()
min_m <- list()
for (i in 1:4)
{
  max <- numeric(100000)
  min <- numeric(100000)
  for (j in 1:100000)
  {
    data <- rnorm(m[i])
    max[j] <- max(data)
    min[j] <- min(data)
  }
  max_m[[i]] <- max
  min_m[[i]] <- min
}
names(max_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
names(min_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
max_m <- as_tibble(max_m)
min_m <- as_tibble(min_m)
new_data1 <- tidyr::gather(max_m, EVD, value)
new_data2 <- tidyr::gather(min_m, EVD, value)
p3a <- ggplot(new_data1, aes(x = value)) +
  geom_density(aes(group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)")

p3a <- p3a + geom_density(data = new_data2, aes(x = value, group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12)
  ) +
  theme(legend.title = element_blank())

## exponential
set.seed(100)
m <- c(1, 10, 100, 1000)
max_m <- list()
for (i in 1:4)
{
  max <- numeric(100000)
  for (j in 1:100000)
  {
    max[j] <- max(rexp(m[i]))
  }
  max_m[[i]] <- max
}
names(max_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
max_m <- as_tibble(max_m)
new_data <- tidyr::gather(max_m, EVD, value)
p3b <- ggplot(new_data, aes(x = value)) +
  geom_density(aes(group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12)
  ) +
  theme(legend.title = element_blank())

ggpubr::ggarrange(p3a, p3b, common.legend = TRUE, legend = "bottom")







## ---- sensortypes
# Figure 4 : Feature space for different sensor types
# Figure 4(b)
features1 <- extract_tsfeatures(anomalous_stream_1)
pc1 <- get_pc_space(features1)
g <- as_tibble(pc1$pcnorm) %>%
  mutate(row_n = 1:ncol(anomalous_stream_1)) %>%
  mutate(type = ifelse(1:ncol(anomalous_stream_1) %in%
    350:560, "out", "normal")) %>%
  mutate(Value = features1[, 1])
g <- g[-(c(449:450, 520, 613:616)), ]
p1 <- g %>%
  ggplot(aes(x = PC1, y = PC2, label1 = row_n)) +
  geom_point(aes(colour = Value)) +
  scale_color_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(anomalous_stream_1))
  ) +
  geom_text(
    aes(label = ifelse(row_n %in% c(seq(350, 550, 25), 448, 451),
      as.character(row_n), ""
    )),
    hjust = 1, vjust = 1,
    col = "black", size = 3
  ) +
  theme(aspect.ratio = 1) +
  expand_limits(
    y = c(-8, 4),
    x = c(-4, 7)
  ) +
  ggtitle("(b) Feature space") +
  theme(legend.position = "none") +
  stat_density_2d(alpha = 0.4, bins = 4)

# Figure 4(a)
anomalous_stream_2 <- anomalous_stream_1
j <- c(290:399, 401:449, 451:590)
set.seed(123)
for (i in 1:length(j)) {
  anomalous_stream_2[, j[i]] <- (anomalous_stream_2[, sample(1:300, 1)] +
    rnorm(nrow(anomalous_stream_2), 0, 0.5))
}

anomalous_stream_2[, 1:290] <- anomalous_stream_2[, sample(1:290)] +
  rnorm(nrow(anomalous_stream_2), 0, 0.5)
anomalous_stream_2[, 200] <- anomalous_stream_2[, 400] * 4
anomalous_stream_2[, 400] <- anomalous_stream_2[, 300]
features2 <- extract_tsfeatures(anomalous_stream_2)
pc2 <- get_pc_space(features2)
g2 <- as_tibble(pc2$pcnorm) %>%
  mutate(row_n = 1:ncol(anomalous_stream_2)) %>%
  mutate(type = ifelse(1:ncol(anomalous_stream_2) %in%
    290, "out", "normal")) %>%
  mutate(Value = features2[, 1])
g2 <- g2[-(613:616), ]
p2 <- g2 %>%
  ggplot(aes(x = PC1, y = PC2, label1 = row_n)) +
  geom_point(aes(colour = Value)) +
  scale_color_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(anomalous_stream_2))
  ) +
  geom_text(
    aes(label = ifelse(row_n %in% c(200, 450), as.character(row_n), "")),
    hjust = 1, vjust = 1, col = "black", size = 3
  ) +
  theme(aspect.ratio = 1) +
  expand_limits(y = c(-5, 20), x = c(-25, 7)) +
  ggtitle("(a) Feature space") +
  theme(legend.position = "none") +
  stat_density_2d(alpha = 0.4, bins = 3)

# Figure 4(d)
fence_climb1 <- anomalous_stream_1[, -(c(449:450, 520, 613:616))]
t <- nrow(fence_climb1)
f <- ncol(fence_climb1)
g <- as_tibble(fence_climb1) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p3 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(fence_climb1))
  ) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(d) Multivariate time series plot")

# Figure 4(c)
fence_climb2 <- anomalous_stream_1
j <- c(290:399, 401:449, 451:590)
set.seed(123)
for (i in 1:length(j)) {
  fence_climb2[, j[i]] <- (fence_climb2[, sample(1:300, 1)] +
    rnorm(nrow(fence_climb2), 0, 0.5))
}
fence_climb2[, 1:290] <- fence_climb2[, sample(1:290)] +
  rnorm(nrow(fence_climb2), 0, 0.5)
fence_climb2[, 200] <- fence_climb2[, 400] * 4
fence_climb2[, 400] <- fence_climb2[, 300]
t <- nrow(fence_climb2)
f <- ncol(fence_climb2)
g <- as_tibble(fence_climb2) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p4 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(fence_climb2))
  ) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(c) Multivariate time series plot")
ggpubr::ggarrange(
  ggarrange(p2, p1, ncol = 2), ggarrange(p4, p3, ncol = 2),
  nrow = 2, heights = c(5, 3)
)







## ---- tsfeatures_generate
# Generate Figure 5
t <- nrow(anomalous_stream_1)
f <- ncol(anomalous_stream_1)
ts <- oddstream::extract_tsfeatures(anomalous_stream_1)
ts <- cbind(Cable = 1:f, ts)
ts <- as_tibble(ts)
ts1 <- gather(ts, Feature, Value, -Cable)
ts1$Feature <- factor(ts1$Feature, levels = c(
  "spikiness", "lumpiness", "vchange", "lshift", "maximum",
  "moment3", "variance", "mean", "linearity", "highlowmu",
  "BurstinessFF", "curvature", "rmeaniqmean", "minimum"
))
p1 <- ggplot(ts1, aes(x = Cable, y = Value)) +
  geom_line() +
  facet_grid(Feature ~ ., scales = "free") +
  theme(
    strip.text.y = element_text(angle = 360, size = 12),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 14)
  ) +
  xlab("Cable (Individual points of the sensor cable)") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2))
ggsave(filename = "figure/tsfeatures.png", plot = p1)







####################################################### Figure 6 - Analysis
## ---- generate_sim2
# Generate sim2 (simulate_2.rda) dataset
# Figure 6 - Multimodal typical classes but no non-stationarity.
nobs <- 1500
nts <- 250
tsframe1 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  # tsframe1[,i] <- exp(0.002*(1:nobs)) + rnorm(nobs,0,2) # adding noise
  tsframe1[, i] <- 10 + rnorm(nobs, 0, 2) # adding noise
}
tsframe2 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  #  tsframe2[,i] <- exp(0.002*(1:nobs)) + rnorm(nobs,0,2) # adding noise
  tsframe2[, i] <- 20 + rnorm(nobs, 0, 2) # adding noise
}
tsframe <- cbind(tsframe1, tsframe2)

# to make the series an anomalouse time series
ts_start <- c(35, 30, 25, 30, 35)
ts_end <- c(40, 45, 50, 45, 40)
time_start <- seq(400, 880, by = 120)
time_end <- seq(520, 1000, by = 120)
out <- c(15, 20, 30, 25, 20)
for (i in 1:length(time_start)) {
  points <- (ts_end[i] - ts_start[i]) * (time_end[i] - time_start[i])
  tsframe[ time_start[i]:(time_end[i] - 1), ts_start[i]:(ts_end[i] - 1)] <-
    matrix(rnorm(points, out[i], 4), ncol = ts_end[i] - ts_start[i])
}
save(tsframe, file = "data/simulate_2.rda")








## ---- label_sim2
# Label each point as an outlier (1) or a typical point (0)
load(file = "simulate_2.rda")
true_sim2 <- matrix(0, ncol = 500, nrow = 1500)

ts_start <- c(35, 30, 25, 30, 35)
ts_end <- c(40, 45, 50, 45, 40)
time_start <- seq(400, 880, by = 120)
time_end <- seq(520, 1000, by = 120)
out <- c(15, 20, 30, 25, 20)
for (i in 1:length(time_start)) {
  points <- (ts_end[i] - ts_start[i]) * (time_end[i] - time_start[i])
  true_sim2[ time_start[i]:(time_end[i] - 1), ts_start[i]:(ts_end[i] - 1)] <-
    matrix(1, ncol = ts_end[i] - ts_start[i])
}
save(true_sim2, file = "data/true_sim2.rda")







## ---- generate_out_sim_2
# Apply Algorithm 1 and 2 to simulate_2 dataset and generate the output: out_simulate_2.rda
# Considers the first window (W[1:150]) of the data set as the training set
# and the remaining as the test stream
load(file = "data/simulate_2.rda")
train_data <- tsframe[1:150, ]
test_stream <- tsframe[151:1500, ]
out_sim2 <- find_odd_streams(train_data, test_stream,
  plot_type = "", trials = 50, window_skip = 1, p_rate = 0.001
)
save(out_simulate2, file = "sim2bp001.rda")







## ---- sim2
# Figure 6
# Figure (a) Multivariate time series plot (top view)
load(file = "data/simulate_2.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  scale_x_continuous(breaks = seq(0, 1500, 200)) +
  xlab("Time") +
  ggtitle("(a) Multivariate time series plot (top view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))

# Figure (b) Multivariate time series plot (side view)
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
p2 <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.2) +
  ggtitle("(b) Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1500, 200))

# Figure (c) Resulting Output
load(file = "data/sim2bp001.rda")
out_sim2 <- out_simulate2
t <- nrow(out_sim2)
f <- ncol(out_sim2)
g <- as_tibble(out_sim2) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p3 <- ggplot(g, aes(x = Time + 300, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("lightgray", "black"), labels = c("normal", "outlier"), name = "Output") +
  scale_x_continuous(breaks = seq(0, 1500, 200)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 1500)) +
  ggtitle("(c) Resulting Output") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  )

# Figure- d) Performance of the algorithm 1 & 2
load(file = "data/true_sim2.rda")
true_sim2 <- true_sim2[301:1500, ]
c <- ncol(true_sim2)
r <- nrow(true_sim2)
true_positive_2 <- rep(0, r)
true_negative_2 <- rep(0, r)
false_positive_2 <- rep(0, r)
false_negative_2 <- rep(0, r)
ppv_2 <- rep(0, r)
npv_2 <- rep(0, r)
accuracy_2 <- rep(0, r)
for (i in 1:r)
{
  false_positive_2[i] <- sum(out_sim2[i, ] == 1 & true_sim2[i, ] == 0) / c
  false_negative_2[i] <- sum(out_sim2[i, ] == 0 & true_sim2[i, ] == 1) / c
  true_positive_2[i] <- sum(out_sim2[i, ] == 1 & true_sim2[i, ] == 1) / c
  true_negative_2[i] <- sum(out_sim2[i, ] == 0 & true_sim2[i, ] == 0) / c
  accuracy_2[i] <- sum(out_sim2[i, ] == true_sim2[i, ]) / c
  ppv_2[i] <- true_positive_2[i] / (true_positive_2[i] + false_positive_2[i])
  npv_2[i] <- true_negative_2[i] / (true_negative_2[i] + false_negative_2[i])
}

performance <- tibble(
  Time = (1:r) + 300, PPV = ppv_2, FN = false_negative_2, FP = false_positive_2,
  Accuracy = accuracy_2
)
df <- gather(performance, Series, Value, -Time)
p4 <- ggplot(data = df, aes(x = Time, y = Value, group = Series, color = Series, linetype = Series)) +
  geom_line(alpha = 0.8) +
  geom_point(data = df, aes(alpha = Series, shape = Series, size = Series)) +
  labs(color = "") +
  expand_limits(x = c(0, 1500), y = c(0, 1)) +
  ylab("Performance") +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 8), legend.text = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(d) Performance of the proposed framework") +
  scale_y_continuous(breaks = seq(0, 500, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1500, 200))
p4 <- p4 + scale_color_manual(values = c("Accuracy" = "#000000", "FP" = "#f4c133", "FN" = "#0072B2", "PPV" = "#e31a1c"))
p4 <- p4 + scale_linetype_manual("", values = c("Accuracy" = 1, "FP" = 3, "FN" = 4, "PPV" = 5))
p4 <- p4 + scale_shape_manual("", values = c("Accuracy" = 17, "FP" = 22, "FN" = 18, "PPV" = 21)) +
  scale_size_manual("", values = c("Accuracy" = 1, "FP" = 1, "FN" = 2.5, "PPV" = 2.5)) +
  scale_alpha_manual("", values = c("Accuracy" = 0.5, "FP" = 1, "FN" = 1, "PPV" = 1))

p <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "figure/sim2.png", plot = p, height = 6.74, width = 12) # Saving 12 x 6.74 in image


# PPV and NPV for the whole experiment
c <- ncol(true_sim2) * nrow(true_sim2)
FP <- sum(out_sim2 == 1 & true_sim2 == 0) / c
FN <- sum(out_sim2 == 0 & true_sim2 == 1) / c
TP <- sum(out_sim2 == 1 & true_sim2 == 1) / c
TN <- sum(out_sim2 == 0 & true_sim2 == 0) / c
Sn <- TP / (TP + FN)
Sp <- TN / (TN + FP)
Pp <- sum(true_sim2 == 1) / c # Prevalence : Propotion of outliers
Pn <- 1 - Pp
PPV <- Sn * Pp / (Sn * Pp + ((1 - Sp) * (1 - Pp)))
NPV <- Sp * (1 - Pp) / ((1 - Sn) * Pp + Sp * (1 - Pp))
LR <- (1 - Sn) / Sp

P <- Sn * Pp + Sp * Pn
RI <- abs(Sp - Sn) / (Sp + Sn)
OP <- P - RI

## Minimal value of Accuracy and Maximum value of FP and FN
min_acc <- performance[ which.min(performance$Accuracy), c("Time", "Accuracy")]
max_FP <- performance[ which.max(performance$FP), c("Time", "FP")]
max_FN <- performance[ which.max(performance$FN), c("Time", "FN")]







####################################################### Figure 7 - Analysis
## ---- generate_simulate_7
# (sudden non-stationarity)
# Generate simulate_7 (simulate_7.rda) dataset
# Figure 7 - `Sudden' non-stationarity
set.seed(1234)
nts <- 300
nobs <- 300
tsframe0 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe0[, i] <- 0 + rnorm(nobs, 0, 2) # adding noise
} # adding noise
nobs <- 400
nts <- 300
tsframe1 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe1[, i] <- 15 + rnorm(nobs, 0, 2) # adding noise
}
# to make the series an anomalouse time series
for (j in 150:170)
{
  tsframe1[150:175, j] <- tsframe1[150:175, j] + rnorm(26, 15, 3)
}
tsframe <- rbind(tsframe0, tsframe1)
save(tsframe, file = "data/simulate_7.rda")







## ---- label_simulate_7
# Label each point as an outlier (1) or a typical point (0)
load(file = "data/simulate_7.rda")
true_sim7 <- matrix(0, ncol = 300, nrow = 700)
for (j in 150:170)
{
  true_sim7[450:475, j] <- rep(1, 26)
}
save(true_sim7, file = "data/true_sim7.rda")







## ---- generate_out_simulate_7
# Apply Algorithm 1, 2 and 3 to simulate_7 dataset and generate the output: out_simulate_7.rda
# Considers the first window (W[1:150]) of the data set as the training set
# and the remaining as the test stream. In this analysis model is updated in an
# occurance of non stationarity ( According to Algorithm 3)
load(file = "data/simulate_7.rda")
train_data <- tsframe[1:100, ]
test_stream <- tsframe[101:700, ]
out_sim7 <- find_odd_streams(train_data, test_stream,
  plot_type = "", trials = 100,
  window_skip = 1, concept_drift = TRUE, p_rate = 0.001, cd_alpha = 0.1
)
out_simulate_7 <- out_sim7[[1]]
save(out_simulate_7, file = "data/out_sim7dp001t1.rda")
## Detection of concept drift, P value for the hypothesis test fto == ftt
cd_sim7 <- out_sim7[[2]]
save(cd_sim7, file = "data/cd_sim7dp001t1.rda")
anom_t_sim7 <- out_sim7[[3]]
save(anom_t_sim7, file = "data/anom_sim7dp001t1.rda")







## ---- sudden
# Figure 7
# (a) Multivariate time series plot (top view)
load(file = "data/simulate_7.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p10 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 700, 100)) +
  xlab("Time") +
  ggtitle("(a) Multivariate time series plot (top view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))

# (b)  Multivariate time series plot (side view)
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
ph <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
  ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 700, 100))

# (c) Resulting Output"
load(file = "data/out_sim7dp001t1.rda")
t <- nrow(out_simulate_7)
f <- ncol(out_simulate_7)
g <- as_tibble(out_simulate_7) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p11 <- ggplot(g, aes(x = Time + 200, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("lightgray", "black"), labels = c("normal", "outlier"), name = "Output") +
  scale_x_continuous(breaks = seq(0, 700, 100)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 700)) +
  ggtitle("(c) Resulting Output") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

# (d) Performance of the algorithm 1-3
load(file = "data/true_sim7.rda")
true_sim7 <- true_sim7[201:700, ]
c <- ncol(true_sim7)
r <- nrow(true_sim7)
true_positive <- rep(0, r)
true_negative <- rep(0, r)
false_positive <- rep(0, r)
false_negative <- rep(0, r)
ppv <- rep(0, r)
npv <- rep(0, r)
accuracy <- rep(0, r)
for (i in 1:r)
{
  false_positive[i] <- sum(out_simulate_7[i, ] == 1 & true_sim7[i, ] == 0) / c
  false_negative[i] <- sum(out_simulate_7[i, ] == 0 & true_sim7[i, ] == 1) / c
  true_positive[i] <- sum(out_simulate_7[i, ] == 1 & true_sim7[i, ] == 1) / c
  true_negative[i] <- sum(out_simulate_7[i, ] == 0 & true_sim7[i, ] == 0) / c
  accuracy[i] <- sum(out_simulate_7[i, ] == true_sim7[i, ]) / c
  ppv[i] <- true_positive[i] / (true_positive[i] + false_positive[i])
  npv[i] <- true_negative[i] / (true_negative[i] + false_negative[i])
}

performance <- tibble(
  Time = (1:r) + 200, PPV = ppv, FN = false_negative, FP = false_positive,
  Accuracy = accuracy
)
df <- gather(performance, Series, Value, -Time)
p4 <- ggplot(data = df, aes(x = Time, y = Value, group = Series, color = Series, linetype = Series)) +
  geom_line(alpha = 0.8) +
  geom_point(data = df, aes(alpha = Series, shape = Series, size = Series)) +
  labs(color = "") +
  expand_limits(x = c(0, 700), y = c(0, 1)) +
  ylab("Performance") +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 8), legend.text = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(d) Performance of the proposed framework") +
  scale_y_continuous(breaks = seq(0, 500, 0.2)) +
  scale_x_continuous(breaks = seq(0, 700, 100))
p4 <- p4 + scale_color_manual(values = c("Accuracy" = "#000000", "FP" = "#f4c133", "FN" = "#0072B2", "PPV" = "#e31a1c"))
p4 <- p4 + scale_linetype_manual("", values = c("Accuracy" = 1, "FP" = 3, "FN" = 4, "PPV" = 5))
p4 <- p4 + scale_shape_manual("", values = c("Accuracy" = 17, "FP" = 22, "FN" = 18, "PPV" = 21)) +
  scale_size_manual("", values = c("Accuracy" = 1, "FP" = 1, "FN" = 2.5, "PPV" = 2.5)) +
  scale_alpha_manual("", values = c("Accuracy" = 0.5, "FP" = 1, "FN" = 1, "PPV" = 1))

p <- gridExtra::grid.arrange(p10, ph, p11, p4)
ggsave(filename = "figure/sudden.png", plot = p, height = 6.74, width = 12)


# PPV and NPV for the whole experiment
c <- ncol(true_sim7) * nrow(true_sim7)
FP <- sum(out_simulate_7 == 1 & true_sim7 == 0) / c
FN <- sum(out_simulate_7 == 0 & true_sim7 == 1) / c
TP <- sum(out_simulate_7 == 1 & true_sim7 == 1) / c
TN <- sum(out_simulate_7 == 0 & true_sim7 == 0) / c
Sn <- TP / (TP + FN)
Sp <- TN / (TN + FP)
Pp <- sum(true_sim7 == 1) / c # Prevalence : Propotion of outliers
Pn <- 1 - Pp
PPV <- Sn * Pp / (Sn * Pp + ((1 - Sp) * (1 - Pp)))
NPV <- Sp * (1 - Pp) / ((1 - Sn) * Pp + Sp * (1 - Pp))
LR <- (1 - Sn) / Sp

P <- Sn * Pp + Sp * Pn
RI <- abs(Sp - Sn) / (Sp + Sn)
OP <- P - RI

## Minimal value of Accuracy and Maximum value of FP and FN
min_acc <- performance[ which.min(performance$Accuracy), c("Time", "Accuracy")]
max_FP <- performance[ which.max(performance$FP), c("Time", "FP")]
max_FN <- performance[ which.max(performance$FN), c("Time", "FN")]







####################################################### Figure 8 - Analysis
## ---- generate_simulate_10
# (gradual non-stationarity)
# Generate simulate_10 (simulate_10.rda) dataset
# Figure 8 - `gradual' non-stationarity
## dataset with concept drift
nts <- 300
nobs <- 300
tsframe0 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe0[, i] <- 0 + rnorm(nobs, 0, 2) # adding noise
} # adding noise
nobs <- 700
nts <- 300
tsframe1 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe1[, i] <- 15 + rnorm(nobs, 0, 2) # adding noise
}
for (j in 150:170)
{
  tsframe1[550:575, j] <- tsframe1[550:575, j] + rnorm(26, 10, 3)
}
tsframe <- rbind(tsframe0, tsframe1)
tsframe[325:350, ] <- 0 + rnorm(7800, 0, 2)
tsframe[425:475, ] <- 0 + rnorm(15300, 0, 2)
save(tsframe, file = "data/simulate_10.rda")







## ---- label_simulate_10
# Label each point as an outlier (1) or a typical point (0)
load(file = "data/simulate_10.rda")
true_sim10 <- matrix(0, ncol = 300, nrow = 1000)
for (j in 150:170)
{
  true_sim10[850:875, j] <- rep(1, 26)
}
save(true_sim10, file = "data/true_sim10.rda")







## ---- generate_out_simulate_10
# Apply Algorithm 1, 2 and 3 to simulate_10 dataset and generate the output: out_simulate_10.rda
# Considers the first window (W[1:150]) of the data set as the training set
# and the remaining as the test stream. In this analysis model is updated in an
# occurance of non stationarity ( According to Algorithm 3)
load(file = "data/simulate_10.rda")
# Considers the first window  of the data set as the training set and the remaining as the test stream
train_data <- tsframe[1:100, ]
test_stream <- tsframe[101:1000, ]
out_sim10 <- find_odd_streams(train_data, test_stream,
  plot_type = "", trials = 100,
  window_skip = 1, concept_drift = TRUE, p_rate = 0.001, cd_alpha = 0.1
)
out_simulate_10 <- out_sim10[[1]]
save(out_simulate_10, file = "data/out_sim10dp001t1.rda")
cd_sim10 <- out_sim10[[2]]
save(cd_sim10, file = "data/cd_sim10dp001t1.rda")
anom_t_sim10 <- out_sim10[[3]]
save(anom_t_sim10, file = "data/anom_sim10dp001t1.rda")







## ---- gradual
# Figure 8
# (a) Multivariate time series plot (top view)
load(file = "data/simulate_10.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p13 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
  ggtitle("(a) Multivariate time series plot (top view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))

# (b)  Multivariate time series plot (side view)
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
ph <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
  ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1000, 100))

# (c) Resulting Output
load(file = "data/out_sim10dp001t1.rda")
t <- nrow(out_simulate_10)
f <- ncol(out_simulate_10)
g <- as_tibble(out_simulate_10) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p14 <- ggplot(g, aes(x = Time + 200, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("lightgray", "black"), labels = c("normal", "outlier"), name = "Output") +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 1000)) +
  ggtitle("(c) Resulting Output") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

# (d) Performance of the Algorithm 1-3
load(file = "data/true_sim10.rda")
true_sim10 <- true_sim10[201:1000, ]
c <- ncol(true_sim10)
r <- nrow(true_sim10)
true_positive <- rep(0, r)
true_negative <- rep(0, r)
false_positive <- rep(0, r)
false_negative <- rep(0, r)
ppv <- rep(0, r)
npv <- rep(0, r)
accuracy <- rep(0, r)
for (i in 1:r)
{
  false_positive[i] <- sum(out_simulate_10[i, ] == 1 & true_sim10[i, ] == 0) / c
  false_negative[i] <- sum(out_simulate_10[i, ] == 0 & true_sim10[i, ] == 1) / c
  true_positive[i] <- sum(out_simulate_10[i, ] == 1 & true_sim10[i, ] == 1) / c
  true_negative[i] <- sum(out_simulate_10[i, ] == 0 & true_sim10[i, ] == 0) / c
  accuracy[i] <- sum(out_simulate_10[i, ] == true_sim10[i, ]) / c
  ppv[i] <- true_positive[i] / (true_positive[i] + false_positive[i])
  npv[i] <- true_negative[i] / (true_negative[i] + false_negative[i])
}

performance <- tibble(
  Time = (1:r) + 200, PPV = ppv, FN = false_negative, FP = false_positive,
  Accuracy = accuracy
)
df <- gather(performance, Series, Value, -Time)
p4 <- ggplot(data = df, aes(x = Time, y = Value, group = Series, color = Series, linetype = Series)) +
  geom_line(alpha = 0.8) +
  geom_point(data = df, aes(alpha = Series, shape = Series, size = Series)) +
  labs(color = "") +
  expand_limits(x = c(0, 700), y = c(0, 1)) +
  ylab("Performance") +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 8), legend.text = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(d) Performance of the proposed framework") +
  scale_y_continuous(breaks = seq(0, 500, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1000, 100))
p4 <- p4 + scale_color_manual(values = c("Accuracy" = "#000000", "FP" = "#f4c133", "FN" = "#0072B2", "PPV" = "#e31a1c"))
p4 <- p4 + scale_linetype_manual("", values = c("Accuracy" = 1, "FP" = 3, "FN" = 4, "PPV" = 5))
p4 <- p4 + scale_shape_manual("", values = c("Accuracy" = 17, "FP" = 22, "FN" = 18, "PPV" = 21)) +
  scale_size_manual("", values = c("Accuracy" = 1, "FP" = 1, "FN" = 2.5, "PPV" = 2.5)) +
  scale_alpha_manual("", values = c("Accuracy" = 0.5, "FP" = 1, "FN" = 1, "PPV" = 1))


p <- gridExtra::grid.arrange(p13, ph, p14, p4)
ggsave(filename = "figure/gradual.png", plot = p, height = 6.74, width = 12)

# PPV and NPV for the whole experiment
c <- ncol(true_sim10) * nrow(true_sim10)
FP <- sum(out_simulate_10 == 1 & true_sim10 == 0) / c
FN <- sum(out_simulate_10 == 0 & true_sim10 == 1) / c
TP <- sum(out_simulate_10 == 1 & true_sim10 == 1) / c
TN <- sum(out_simulate_10 == 0 & true_sim10 == 0) / c
Sn <- TP / (TP + FN)
Sp <- TN / (TN + FP)
Pp <- sum(true_sim10 == 1) / c # Prevalence : Propotion of outliers
Pn <- 1 - Pp
PPV <- Sn * Pp / (Sn * Pp + ((1 - Sp) * (1 - Pp)))
NPV <- Sp * (1 - Pp) / ((1 - Sn) * Pp + Sp * (1 - Pp))
LR <- (1 - Sn) / Sp

P <- Sn * Pp + Sp * Pn
RI <- abs(Sp - Sn) / (Sp + Sn)
OP <- P - RI

## Minimal value of Accuracy and Maximum value of FP and FN
min_acc <- performance[ which.min(performance$Accuracy), c("Time", "Accuracy")]
max_FP <- performance[ which.max(performance$FP), c("Time", "FP")]
max_FN <- performance[ which.max(performance$FN), c("Time", "FN")]







####################################################### Figure 9 - Analysis
## ---- generate_simulate_11
# (reoccurring non-stationarity)
# Generate simulate_11 (simulate_11.rda) dataset
# Figure 9 - `reoccurring' non-stationarity
## dataset with concept drift
nts <- 300
nobs <- 1000
tsframe <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe[, i] <- 0 + rnorm(nobs, 0, 2) # adding noise
} # adding noise
for (j in 150:170)
{
  tsframe[825:875, j] <- tsframe[825:875, j] + rnorm(51, 8, 3)
}
for (i in 300:450) {
  tsframe[i, ] <- 15 + rnorm(nts, 0, 2) # adding noise
}
save(tsframe, file = "data/simulate_11.rda")

## ---- label_simulate_11
# Label each point as an outlier (1) or a typical point (0)
load(file = "data/simulate_11.rda")
true_sim11 <- matrix(0, ncol = 300, nrow = 1000)
for (j in 150:170)
{
  true_sim11[825:875, j] <- rep(1, 51)
}
save(true_sim11, file = "data/true_sim11.rda")







## ---- generate_out_simulate_11
# Apply Algorithm 1, 2 and 3 to simulate_11 dataset and generate the output: out_simulate_10.rda
# Considers the first window (W[1:150]) of the data set as the training set
# and the remaining as the test stream. In this analysis model is updated in an
# occurance of non stationarity ( According to Algorithm 3)
load(file = "data/simulate_11.rda")
# Considers the first window  of the data set as the training set and the remaining as the test stream
train_data <- tsframe[1:100, ]
test_stream <- tsframe[101:1000, ]
out_sim11 <- find_odd_streams(train_data, test_stream,
  plot_type = "",
  trials = 100, window_skip = 1, concept_drift = TRUE, p_rate = 0.001, cd_alpha = 0.1
)
out_simulate_11 <- out_sim11[[1]]
save(out_simulate_11, file = "data/out_sim11dp001t1.rda")
cd_sim11 <- out_sim11[[2]]
save(cd_sim11, file = "data/cd_sim11dp001t1.rda")
anom_t_sim11 <- out_sim11[[3]]
save(anom_t_sim11, file = "data/anom_sim11dp001t1.rda")







## ---- reoccurring
# Figure 9

# (a)  Multivariate time series plot (top view)
load(file = "data/simulate_11.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p10 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
  ggtitle("(a)  Multivariate time series plot (top view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))

# (b)  Multivariate time series plot (side view)
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
ph <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
  ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1000, 100))

# (c) Resulting Output
load(file = "data/out_sim11dp001t1.rda")
t <- nrow(out_simulate_11)
f <- ncol(out_simulate_11)
g <- as_tibble(out_simulate_11) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p11 <- ggplot(g, aes(x = Time + 200, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("lightgray", "black"), labels = c("normal", "outlier"), name = "Output") +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 1000)) +
  ggtitle("(c) Resulting Output") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

# (d) Performance of the Algorithm 1-3
load(file = "data/true_sim11.rda")
true_sim11 <- true_sim11[201:1000, ]
c <- ncol(true_sim11)
r <- nrow(true_sim11)
true_positive <- rep(0, r)
true_negative <- rep(0, r)
false_positive <- rep(0, r)
false_negative <- rep(0, r)
ppv <- rep(0, r)
npv <- rep(0, r)
accuracy <- rep(0, r)
for (i in 1:r)
{
  false_positive[i] <- sum(out_simulate_11[i, ] == 1 & true_sim11[i, ] == 0) / c
  false_negative[i] <- sum(out_simulate_11[i, ] == 0 & true_sim11[i, ] == 1) / c
  true_positive[i] <- sum(out_simulate_11[i, ] == 1 & true_sim11[i, ] == 1) / c
  true_negative[i] <- sum(out_simulate_11[i, ] == 0 & true_sim11[i, ] == 0) / c
  accuracy[i] <- sum(out_simulate_11[i, ] == true_sim11[i, ]) / c
  ppv[i] <- true_positive[i] / (true_positive[i] + false_positive[i])
  npv[i] <- true_negative[i] / (true_negative[i] + false_negative[i])
}

performance <- tibble(
  Time = (1:r) + 200, PPV = ppv, FN = false_negative, FP = false_positive,
  Accuracy = accuracy
)
df <- gather(performance, Series, Value, -Time)
p4 <- ggplot(data = df, aes(x = Time, y = Value, group = Series, color = Series, linetype = Series)) +
  geom_line(alpha = 0.8) +
  geom_point(data = df, aes(alpha = Series, shape = Series, size = Series)) +
  labs(color = "") +
  expand_limits(x = c(0, 1000), y = c(0, 1)) +
  ylab("Performance") +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 8), legend.text = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(d) Performance of the proposed framework") +
  scale_y_continuous(breaks = seq(0, 500, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1000, 100))
p4 <- p4 + scale_color_manual(values = c("Accuracy" = "#000000", "FP" = "#f4c133", "FN" = "#0072B2", "PPV" = "#e31a1c"))
p4 <- p4 + scale_linetype_manual("", values = c("Accuracy" = 1, "FP" = 3, "FN" = 4, "PPV" = 5))
p4 <- p4 + scale_shape_manual("", values = c("Accuracy" = 17, "FP" = 22, "FN" = 18, "PPV" = 21)) +
  scale_size_manual("", values = c("Accuracy" = 1, "FP" = 1, "FN" = 2.5, "PPV" = 2.5)) +
  scale_alpha_manual("", values = c("Accuracy" = 0.5, "FP" = 1, "FN" = 1, "PPV" = 1))


p <- gridExtra::grid.arrange(p10, ph, p11, p4)
ggsave(filename = "figure/reoccurring.png", plot = p, height = 6.74, width = 12)

# PPV and NPV for the whole experiment
c <- ncol(true_sim11) * nrow(true_sim11)
FP <- sum(out_simulate_11 == 1 & true_sim11 == 0) / c
FN <- sum(out_simulate_11 == 0 & true_sim11 == 1) / c
TP <- sum(out_simulate_11 == 1 & true_sim11 == 1) / c
TN <- sum(out_simulate_11 == 0 & true_sim11 == 0) / c
Sn <- TP / (TP + FN)
Sp <- TN / (TN + FP)
Pp <- sum(true_sim11 == 1) / c # Prevalence : Propotion of outliers
Pn <- 1 - Pp
PPV <- Sn * Pp / (Sn * Pp + ((1 - Sp) * (1 - Pp)))
NPV <- Sp * (1 - Pp) / ((1 - Sn) * Pp + Sp * (1 - Pp))
LR <- (1 - Sn) / Sp

P <- Sn * Pp + Sp * Pn
RI <- abs(Sp - Sn) / (Sp + Sn)
OP <- P - RI

## Minimal value of Accuracy and Maximum value of FP and FN
min_acc <- performance[ which.min(performance$Accuracy), c("Time", "Accuracy")]
max_FP <- performance[ which.max(performance$FP), c("Time", "FP")]
max_FN <- performance[ which.max(performance$FN), c("Time", "FN")]







####################################################### Figure 10 - Analysis
## ---- generate_simulate_14
# (incremental non-stationarity)
# Generate simulate_14 (simulate_14.rda) dataset
# Figure 10 - `incremental' non-stationarity
## dataset with concept drift
set.seed(1234)
nts <- 300
nobs <- 300
tsframe0 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe0[, i] <- 0 + rnorm(nobs, 0, 2) # adding noise
} # adding noise
tsframe1 <- ts(matrix(ncol = nts, nrow = 200))
for (i in 1:200) {
  tsframe1[i, ] <- rnorm(nts, 0, 2) + i * 0.2 # adding noise
}
nobs <- 500
tsframe2 <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe2[, i] <- 40 + rnorm(nobs, 0, 2) # adding noise
} # adding noise
tsframe <- rbind(tsframe0, tsframe1, tsframe2)
for (j in 150:170)
{
  tsframe[825:875, j] <- tsframe[825:875, j] + rnorm(51, 20, 3)
}
save(tsframe, file = "data/simulate_14.rda")







## ---- label_simulate_14
# Label each point as an outlier (1) or a typical point (0)
load(file = "data/simulate_14.rda")
true_sim14 <- matrix(0, ncol = 300, nrow = 1000)
for (j in 150:170)
{
  true_sim14[825:875, j] <- rep(1, 51)
}
save(true_sim14, file = "data/true_sim14.rda")







## ---- generate_out_simulate_14
# Apply Algorithm 1, 2 and 3 to simulate_14 dataset and generate the output: out_simulate_10.rda
# Considers the first window (W[1:150]) of the data set as the training set
# and the remaining as the test stream. In this analysis model is updated in an
# occurance of non stationarity ( According to Algorithm 3)
load(file = "data/simulate_14.rda")
# Considers the first window  of the data set as the training set and the remaining as the test stream
train_data <- tsframe[1:100, ]
test_stream <- tsframe[101:1000, ]
out_sim14 <- find_odd_streams(train_data, test_stream,
  plot_type = "", trials = 100,
  window_skip = 1, concept_drift = TRUE, p_rate = 0.001, cd_alpha = 0.1
)
out_simulate_14 <- out_sim14[[1]]
save(out_simulate_14, file = "data/out_sim14dp001t1.rda")
cd_sim14 <- out_sim14[[2]]
save(cd_sim14, file = "data/cd_sim14dp001t1.rda")
anom_t_sim14 <- out_sim14[[3]]
save(anom_t_sim14, file = "data/anom_sim14dp001t1.rda")







## ---- incremental
# Figure 10
# (a)  Multivariate time series plot (top view)
load(file = "data/simulate_14.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p10 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
  ggtitle("(a)  Multivariate time series plot (top view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))

# (b)  Multivariate time series plot (side view)
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
ph <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
  ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1000, 100))

# (c) Resulting Output"
load(file = "data/out_sim14dp001t1.rda")
t <- nrow(out_simulate_14)
f <- ncol(out_simulate_14)
g <- as_tibble(out_simulate_14) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p11 <- ggplot(g, aes(x = Time + 200, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("lightgray", "black"), labels = c("normal", "outlier"), name = "Output") +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 1000)) +
  ggtitle("(c) Resulting Output") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

# (d) Performance of the Algorithm 1-3
load(file = "data/true_sim14.rda")
true_sim14 <- true_sim14[201:1000, ]
c <- ncol(true_sim14)
r <- nrow(true_sim14)
true_positive <- rep(0, r)
true_negative <- rep(0, r)
false_positive <- rep(0, r)
false_negative <- rep(0, r)
ppv <- rep(0, r)
npv <- rep(0, r)
accuracy <- rep(0, r)
for (i in 1:r)
{
  false_positive[i] <- sum(out_simulate_14[i, ] == 1 & true_sim14[i, ] == 0) / c
  false_negative[i] <- sum(out_simulate_14[i, ] == 0 & true_sim14[i, ] == 1) / c
  true_positive[i] <- sum(out_simulate_14[i, ] == 1 & true_sim14[i, ] == 1) / c
  true_negative[i] <- sum(out_simulate_14[i, ] == 0 & true_sim14[i, ] == 0) / c
  accuracy[i] <- sum(out_simulate_14[i, ] == true_sim14[i, ]) / c
  ppv[i] <- true_positive[i] / (true_positive[i] + false_positive[i])
  npv[i] <- true_negative[i] / (true_negative[i] + false_negative[i])
}

performance <- tibble(
  Time = (1:r) + 200, PPV = ppv, FN = false_negative, FP = false_positive,
  Accuracy = accuracy
)
df <- gather(performance, Series, Value, -Time)
p4 <- ggplot(data = df, aes(x = Time, y = Value, group = Series, color = Series, linetype = Series)) +
  geom_line(alpha = 0.8) +
  geom_point(data = df, aes(alpha = Series, shape = Series, size = Series)) +
  labs(color = "") +
  expand_limits(x = c(0, 1000), y = c(0, 1)) +
  ylab("Performance") +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 8), legend.text = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(d) Performance of the proposed framework") +
  scale_y_continuous(breaks = seq(0, 500, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1000, 100))
p4 <- p4 + scale_color_manual(values = c("Accuracy" = "#000000", "FP" = "#f4c133", "FN" = "#0072B2", "PPV" = "#e31a1c"))
p4 <- p4 + scale_linetype_manual("", values = c("Accuracy" = 1, "FP" = 3, "FN" = 4, "PPV" = 5))
p4 <- p4 + scale_shape_manual("", values = c("Accuracy" = 17, "FP" = 22, "FN" = 18, "PPV" = 21)) +
  scale_size_manual("", values = c("Accuracy" = 1, "FP" = 1, "FN" = 2.5, "PPV" = 2.5)) +
  scale_alpha_manual("", values = c("Accuracy" = 0.5, "FP" = 1, "FN" = 1, "PPV" = 1))


p <- gridExtra::grid.arrange(p10, ph, p11, p4)
ggsave(filename = "figure/incremental.png", plot = p, height = 6.74, width = 12)

# PPV and NPV for the whole experiment
c <- ncol(true_sim14) * nrow(true_sim14)
FP <- sum(out_simulate_14 == 1 & true_sim14 == 0) / c
FN <- sum(out_simulate_14 == 0 & true_sim14 == 1) / c
TP <- sum(out_simulate_14 == 1 & true_sim14 == 1) / c
TN <- sum(out_simulate_14 == 0 & true_sim14 == 0) / c
Sn <- TP / (TP + FN)
Sp <- TN / (TN + FP)
Pp <- sum(true_sim14 == 1) / c # Prevalence : Propotion of outliers
Pn <- 1 - Pp
PPV <- Sn * Pp / (Sn * Pp + ((1 - Sp) * (1 - Pp)))
NPV <- Sp * (1 - Pp) / ((1 - Sn) * Pp + Sp * (1 - Pp))
LR <- (1 - Sn) / Sp

P <- Sn * Pp + Sp * Pn
RI <- abs(Sp - Sn) / (Sp + Sn)
OP <- P - RI

## Minimal value of Accuracy and Maximum value of FP and FN
min_acc <- performance[ which.min(performance$Accuracy), c("Time", "Accuracy")]
max_FP <- performance[ which.max(performance$FP), c("Time", "FP")]
max_FN <- performance[ which.max(performance$FN), c("Time", "FN")]







## ---- conceptdrift
# Figure 11
# (a)Sudden concept drift in Figure 7"
load(file = "data/cd_sim7dp001t1.rda")
load(file = "data/anom_sim7dp001t1.rda")
r <- length(cd_sim7)
sudden <- tibble(time = (1:r) + 200, pval = cd_sim7, thereshold = anom_t_sim7)

p1 <- ggplot(sudden, aes(x = time, y = pval)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 700), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("(a)Sudden (Fig. 7)") +
  scale_x_continuous(breaks = seq(0, 700, 200)) +
  geom_hline(yintercept = 0.1)

p2 <- ggplot(sudden, aes(x = time, y = thereshold)) +
  geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 700)) +
  ylab("Anomalous Threshold") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("") +
  scale_x_continuous(breaks = seq(0, 700, 200)) +
  ggtitle("(e)Sudden (Fig. 7)")

# (b) Gradual concept drift in Figrue 8
load(file = "data/cd_sim10dp001t1.rda")
load(file = "data/anom_sim10dp001t1.rda")
r <- length(cd_sim10)
gradual <- tibble(time = (1:r) + 200, pval = cd_sim10, thereshold = anom_t_sim10)
p3 <- ggplot(gradual, aes(x = time, y = pval)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1000), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("(b) Gradual (Fig. 8)") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  geom_hline(yintercept = 0.1)

p4 <- ggplot(gradual, aes(x = time, y = thereshold)) +
  geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1000)) +
  ylab("Anomalous Threshold") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  ggtitle("(f) Gradual (Fig. 8)")

# (c) Reoccurring concept drift in Figrue 9
load(file = "data/cd_sim11dp001t1.rda")
load(file = "data/anom_sim11dp001t1.rda")
r <- length(cd_sim11_wonorm_padjust)
reoccurring <- tibble(time = (1:r) + 200, pval = cd_sim11_wonorm_padjust, thereshold = anom_t_sim11)

p5 <- ggplot(reoccurring, aes(x = time, y = pval)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1000), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("(c) Reoccurring (Fig. 9)") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  geom_hline(yintercept = 0.1)

p6 <- ggplot(reoccurring, aes(x = time, y = thereshold)) +
  geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1000)) +
  ylab("Anomalous Threshold") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  ggtitle("(g) Reoccurring (Fig. 9)")

# (d) Incremental concept drift in Figrue 10
load(file = "data/cd_sim14dp001t1.rda")
load(file = "data/anom_sim14dp001t1.rda")
r <- length(cd_sim14_wonorm_padjust)
incremental <- tibble(time = (1:r) + 200, pval = cd_sim14_wonorm_padjust, thereshold = anom_t_sim14)

p7 <- ggplot(incremental, aes(x = time, y = pval)) +
  geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1000), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("(d) Incremental (Fig. 10)") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  geom_hline(yintercept = 0.1)

p8 <- ggplot(incremental, aes(x = time, y = thereshold)) +
  geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1000)) +
  ylab("Anomalous Threshold") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 10), title = element_text(size = 10)
  ) +
  ggtitle("") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  ggtitle("(h) Incremental (Fig. 10)")

gridExtra::grid.arrange(p1, p3, p5, p7, p2, p4, p6, p8, ncol = 4)







## ---- application_annalysis

# Application 1
load(file = "data/real_1.rda")
# Considers the first window  of the data set as the training set and the remaining as the test stream
train_data <- tsframe[1:100, ]
test_stream <- tsframe[101:1459, ]
out_real1_wcd <- find_odd_streams(train_data, test_stream,
  plot_type = "",
  trials = 50, window_skip = 1, concept_drift = TRUE,
  p_rate = 0.001, cd_alpha = 0.1
)
out_real1 <- out_real1_wcd[[1]]
save(out_real1, file = "data/out_real1dp001t1.rda")
cd_real1 <- out_real1_wcd[[2]]
save(cd_real1, file = "data/cd_real1dp001t1.rda")
anom_threshold_real1 <- out_real1_wcd[[3]]
save(anom_threshold_real1, file = "data/anom_real1dp001t1.rda")


# Application 2
load(file = "data/real_2.rda")
# Considers the first window  of the data set as the training set and the remaining as the test stream
train_data <- tsframe[1:100, ]
test_stream <- tsframe[101:1085, ]
out_real2_wcd <- find_odd_streams(train_data, test_stream,
  plot_type = "",
  trials = 100, window_skip = 1, concept_drift = TRUE,
  p_rate = 0.001, cd_alpha = 0.1
)
out_real2 <- out_real2_wcd[[1]]
save(out_real2, file = "data/out_real2dp001t1.rda")
cd_real2 <- out_real2_wcd[[2]]
save(cd_real2, file = "data/cd_real2dp001t1.rda")
anom_threshold_real2 <- out_real2_wcd[[3]]
save(anom_threshold_real2, file = "data/anom_real2dp001t1.rda")


# Application 3
load(file = "data/real_3.rda")
# Considers the first window  of the data set as the training set and the remaining as the test stream
train_data <- tsframe[1:50, ]
test_stream <- tsframe[50:499, ]
out_real3_wcd <- find_odd_streams(train_data, test_stream,
  plot_type = "",
  trials = 100, window_skip = 1, concept_drift = TRUE,
  p_rate = 0.001, cd_alpha = 0.1
)
out_real3 <- out_real3_wcd[[1]]
save(out_real3, file = "data/out_real3dp001t1.rda")
cd_real3 <- out_real3_wcd[[2]]
save(cd_real3, file = "data/cd_real3dp001t1.rda")
anom_threshold_real3 <- out_real3_wcd[[3]]
save(anom_threshold_real3, file = "data/anom_real3dp001t1.rda")







## ---- realanalysisplots
# Figure 12

# Application 1
load(file = "data/real_1.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000", "#000000"), values = c(0, 0.1, max(tsframe))) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 640, 100)) +
  scale_x_continuous(breaks = seq(0, 1500, 250)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(a) Multivariate time series plot (Application 1)") +
  theme(
    title = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12)
  )

load(file = "data/out_real1dp001t1.rda")
t <- nrow(out_real1)
f <- ncol(out_real1)
g <- as_tibble(out_real1) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p2 <- ggplot(g, aes(x = Time + 200, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("light gray", "black"), labels = c("normal", "outlier"), name = "") +
  scale_x_continuous(breaks = seq(0, 1500, 250)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 1500)) +
  scale_y_continuous(breaks = seq(0, 640, 100)) +
  theme(legend.position = "none") +
  ggtitle("(d Resulting output ") +
  theme(
    title = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12)
  )

## Application 2
load(file = "data/real_2.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p3 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000", "#000000"), values = c(0, 0.1, max(tsframe))) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 1000, 250)) +
  scale_x_continuous(breaks = seq(0, 1085, 250)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(b) Multivariate time series plot (Application 2)") +
  theme(
    title = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12)
  )
load(file = "data/out_real2dp001t1.rda")
t <- nrow(out_real2)
f <- ncol(out_real2)
g <- as_tibble(out_real2) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p4 <- ggplot(g, aes(x = Time + 200, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("light gray", "black"), labels = c("normal", "outlier"), name = "") +
  scale_x_continuous(breaks = seq(0, 1085, 250)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 1085)) +
  theme(legend.position = "none") +
  ggtitle("(e) Resulting output") +
  theme(
    title = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12)
  )

## Application 3
load(file = "data/real_3.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")

p5 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000", "#000000"), values = c(0, 0.1, max(tsframe))) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 2500, 500)) +
  scale_x_continuous(breaks = seq(0, 500, 100)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(c) Multivariate time series plot (Application 3)") +
  theme(
    title = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12)
  )
load(file = "data/out_real3dp001t1.rda")
t <- nrow(out_real3)
f <- ncol(out_real3)
g <- as_tibble(out_real3) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p6 <- ggplot(g, aes(x = Time + 100, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("light gray", "black"), labels = c("normal", "outlier"), name = "") +
  scale_x_continuous(breaks = seq(0, 500, 100)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 500)) +
  theme(legend.position = "none") +
  ggtitle("(f) Resulting output") +
  theme(
    title = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12)
  )
realanalysis <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6)
ggsave(filename = "figure/realanalysis.png", plot = realanalysis)







## ---- conceptreal
# Figure 13

# (a) Application 1
load(file = "data/cd_real1dp001t1.rda")
load(file = "data/anom_real1dp001t1.rda")
r <- length(cd_real1)
data1 <- tibble(time = (1:r) + 200, pval = cd_real1, Threshold = anom_threshold_real1)
p1 <- ggplot(data1, aes(x = time, y = pval)) +
  geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1500), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12), title = element_text(size = 12)
  ) +
  ggtitle("(a) Application 1") +
  scale_x_continuous(breaks = seq(0, 1500, 200)) +
  geom_hline(yintercept = 0.1)

p2 <- ggplot(data1, aes(x = time, y = Threshold)) +
  geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1500)) +
  ylab("Anomalous Threshold") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1500, 200)) +
  ggtitle("(d) Application 1")


# (a) Application 2
load(file = "data/cd_real2dp001t1.rda")
load(file = "data/anom_real2dp001t1.rda")
r <- length(cd_real2)
data2 <- tibble(time = (1:r) + 200, pval = cd_real2, Threshold = anom_threshold_real2)
p3 <- ggplot(data2, aes(x = time, y = pval)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1085), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(b) Application 2") +
  scale_x_continuous(breaks = seq(0, 1085, 200)) +
  geom_hline(yintercept = 0.1)

p4 <- ggplot(data2, aes(x = time, y = Threshold)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 1085)) +
  ylab("Anomalous Threshold") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1085, 200)) +
  ggtitle("(e) Application 2")

# (c) Application 3
load(file = "data/cd_real3dp001t1.rda")
load(file = "data/anom_real3dp001t1.rda")
r <- length(cd_real3)
data3 <- tibble(time = (1:r) + 200, pval = cd_real3, Threshold = anom_threshold_real3)
p5 <- ggplot(data3, aes(x = time, y = pval)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 500), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(c) Application 3") +
  scale_x_continuous(breaks = seq(0, 500, 100)) +
  geom_hline(yintercept = 0.1)

p6 <- ggplot(data3, aes(x = time, y = Threshold)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 500)) +
  ylab("Anomalous Threshold") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(f) Application 3") +
  scale_x_continuous(breaks = seq(0, 500, 100))

gridExtra::grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3)
