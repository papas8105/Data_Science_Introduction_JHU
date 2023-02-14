library(UsingR)

data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x,n*B,replace = TRUE),B,n)
## Means (unbiased)
resampledMeans   <- apply(resamples,1,mean)
sd(resampledMeans)
quantile(resampledMeans,c(.025,.975))
mean(resampledMeans)
mean(x) - mean(resampledMeans)
library(ggplot2)
ggplot(data = data.frame(bootstrapped_means = resampledMeans),aes(x = bootstrapped_means)) +
    geom_density() + theme_bw() + 
    geom_vline(xintercept = mean(x),col = "red") + 
    geom_vline(xintercept = mean(resampledMeans)) #observe the normality
## Medians
resampledMedians <- apply(resamples,1,median)
sd(resampledMedians)
quantile(resampledMedians,c(.025,.975))
median(resampledMedians)
## bootstrap estimation error
median(x) - median(resampledMedians)
# Use bootstrap package for unbiased estimation
library(bootstrap)
y <- bootstrap(x,1e4,median)
sd(y$thetastar)
quantile(y$thetastar,c(.025,.975))
median(x) - median(y)
# histogram plot 

g <- ggplot(data.frame(resampledMedians = resampledMedians), aes(x = resampledMedians))
g1 <- g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05) + xlab("bootstrap medians") + 
    geom_density(alpha = .2)

h <- ggplot(data.frame(unbiasedbootstrap = y$thetastar), aes(x = unbiasedbootstrap))
h1 <- h + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05) + 
    xlab("bootstrap unbiased medians")
library(patchwork)
g1 + h1

# density plot
g2 <- g + geom_density(fill = rgb(1,0,0,.2)) + geom_vline(xintercept = median(resampledMedians))
h2 <- h + geom_density(fill = rgb(0,0,1,.2)) + geom_vline(xintercept = median(y$thetastar))
g2 + h2
