## Load the data of the p-values
p <- readRDS("pvalues.RDS")
## p is a list with 2 vectors
## The first one contains 1000 p-values obtained by fitting a linear regression model
## to random normal x,y pairs so there is no true significant relationship between 
## x's and y's.
pvalues <- p$all_uncorrelated
## Let us see how many p-values fall under the 0.05 threshold
sum(pvalues < 0.05)
## as expected around 5%, let's use the bonferroni correction
sum(p.adjust(pvalues,method = "bonferroni") < 0.05)
## we see that it eliminates all false positives/false alarm rates/Type I error rate.
## Benjamini-Hochberg correction does the same.
sum(p.adjust(pvalues,method = "BH") < 0.05)
## plot all values with different adjustments
plot(x = 1:1000,y = seq(0,1,length = 1000),type = "n",xlab = "index of sorted pvalues",
     ylab = "adjusted and unadjusted p-values")
## add unadjusted p-values
points(x = 1:1000,y = sort(pvalues),col = "black",pch = 16,cex = .5)
## add 0.05 threshold 
abline(h = 0.05)
## add bonferroni adjusted p-values, all fall over the 0.05 horizontal line
points(x = 1:1000,y = sort(p.adjust(pvalues,method = "bonferroni")),col = "red",pch = 16,cex = .5)
## add BH adjusted p-values, again all fall over the 0.05 horizontal line
points(x = 1:1000,y = sort(p.adjust(pvalues,method = "BH")),col = "green",pch = 16,cex = .5)
title(main = "Bonferroni & BH correction against unadjusted pvalues")
legend("bottomright",legend = c("unadjusted","Bonferroni correction","BH correction"),pch = 16,
       col = c("black","red","green"))
## from the plot is obvious that BH is less conservative than Bonferroni
## -----------------------------------------------------------------------------
## Next example
pvalues2 <- p$half_correlated
## In this data, the first half ( 500 x/y pairs) contains x and y values
## that are random and the second half contain x and y pairs that are related, 
## so running a linear regression model on the 1000 pairs should find some significant 
## (not random) relationship. We create a vector with the classification of the 
## p-pvalues "zero" --> not correlated x,y pairs, "not zero" --> correlated
truestatus <- c(rep('zero',500),rep('not zero',500))
## confusion matrix no correction
table(pvalues2 < .05,truestatus)
## type I errors
24/500
## type II errors
0/500
## confusion matrix bonferroni correction
## we observe that all type I errors are eliminated but the Type II errors 
## were raised to 23
table(p.adjust(pvalues2,method = "bonferroni") < .05,truestatus)
## type  I errors
0/500
## type II errors 
23/500
## confusion matrix BH correction, less conservative it allowed less
## typer I errors than "bonferroni" correction
table(p.adjust(pvalues2,method = "BH") < .05,truestatus)
## type  I errors
13/500
## type II erros
0/500