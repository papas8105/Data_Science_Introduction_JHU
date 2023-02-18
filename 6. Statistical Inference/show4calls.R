#show 4 different calls to t.test
#display as 4 long array
g1 <- sleep$extra[sleep$group == 1]
g2 <- sleep$extra[sleep$group == 2]
difference <- g2 - g1
mn <- mean(difference)
n  <- 10
s  <- sd(g2 - g1)
## paired plot
library(ggplot2)
ggplot(data = sleep,aes(x = group,y = extra)) + geom_point(aes(colour = ID),cex = 4) + 
    geom_line(aes(group = ID,colour = ID)) + theme_bw() + ggtitle("paired sleep data plot") + 
    scale_y_continuous(n.breaks = 20)
## paired t 95% confidence interval
rbind(
  mn + c(-1, 1) * qt(.975,n - 1) * s / sqrt(n),
  as.vector(t.test(difference)$conf.int),
  as.vector(t.test(g2, g1, paired = TRUE)$conf.int),
  as.vector(t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int),
  ## add normal ci although sample is not big enough
  mn + c(-1,1) * qnorm(.975) * s / sqrt(n) ## notice it is narrower
  )
# pvalue for a paired test 
2 * pt(sqrt(10) * mean(difference)/sd(difference),df = 9,lower.tail = FALSE)
## we reject the null hypothesis 