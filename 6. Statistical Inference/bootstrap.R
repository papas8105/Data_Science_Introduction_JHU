library(UsingR)

data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000

resamples <- matrix(sample(x,n*B,replace = TRUE),B,n)
resampledMedians <- apply(resamples,1,median)
sd(resampledMedians)
quantile(resampledMedians,c(.025,.975))

# histogram plot

g <- ggplot(data.frame(resampledMedians = resampledMedians), aes(x = resampledMedians))
g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05) + xlab("bootstrap medians")

# density plot

g + geom_density(color = "black",fill = "red",linewidth = 2) + 
    geom_vline(xintercept = median(resampledMedians),linewidth = 2)
