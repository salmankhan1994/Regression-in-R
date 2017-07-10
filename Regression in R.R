whites <- read.csv("winequality-white (1).csv")
whites$quality_as_factor <- factor(whites$quality, levels=c(0,1,2,3,4,5,6,7,8,9,10))

whites_quality_min <- min(whites$quality)
whites_quality_max <- max(whites$quality)
whites_quality_mean <- mean(whites$quality)
whites_quality_median <- median(whites$quality)
whites_quality_iqr <- IQR(whites$quality)
whites_quality_q1 <- whites_quality_median - whites_quality_iqr
whites_quality_q3 <- whites_quality_median + whites_quality_iqr

whites_quality_mode <- names(which.max(table(whites$quality)))

whites_outliers <- sum(whites$quality < whites_quality_q1 - 1.5*whites_quality_iqr)
whites_outliers <- whites_outliers + sum(whites$quality > whites_quality_q3 + 1.5*whites_quality_iqr)

whites_quality_sd <- sd(whites$quality)

ggplot(data=whites, aes(x=quality)) +
  geom_bar(binwidth=1, color='black', fill='white') +
  coord_cartesian(xlim=c(0,10)) +
  geom_vline(xintercept = whites_quality_median, linetype='longdash', alpha=.5) +
  geom_vline(xintercept = whites_quality_q1 - 1.5*whites_quality_iqr, linetype='longdash', alpha=.5) +
  geom_vline(xintercept = whites_quality_q3 + 1.5*whites_quality_iqr, linetype='longdash', alpha=.5) +
  geom_vline(xintercept = whites_quality_mean, linetype=1, color='red', alpha=.5) +
  xlab("White Wine Quality") +
  ylab("Quantity of Samples")


