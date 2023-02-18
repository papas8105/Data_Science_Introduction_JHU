## mywald function used to depict coverage of binomial trials using the p_hat 
## instead of population's p for the computation of the sd for the construction 
## of confidence intervals

mywald_norm <- function(nosim = 1000,n = 20,aggresti_coull = FALSE) {
    pvals <- seq(0.05,0.95,by = 0.05)
    ifelse(aggresti_coull == FALSE,0,2) -> x
    coverage <-sapply(pvals,function(p) {
        phats <- (rbinom(nosim,prob = p,size = n) + x) / (n + 2 * x)
        ll <- phats - qnorm(.975) * sqrt((phats * (1 - phats)) / n)
        ul <- phats + qnorm(.975) * sqrt((phats * (1 - phats)) / n)
        mean(p > ll & p < ul)
        })
    plot(x = pvals,y = coverage,type = 'l',col = 'red',lwd = 2,xlab = "p-values",ylab = "% coverage",
         main = "Coverage % for different p-values",ylim = c(0,1.05))
    points(x = pvals,y = coverage,pch = 16,cex = 2)
    abline(h = .95)
    rect(0,.95,1,1,col = rgb(1,0,0,.3))
    legend(.37,.2,legend = "over 95% coverage",col = rgb(1,0,0,.3),lwd = 2)
}

## c.i. for poisson, parameter input --> time period of measurement of occurrence of events

coverage_poisson <- function(t = 94.32,min_l = 0.005,max_l = 0.1,step = 0.01,nosim = 1000) {
    lambdas <- seq(min_l,max_l,by = step)
    coverage <- sapply(lambdas,function(lambda) {
        l <- rpois(nosim,lambda*t)/t
        ul <- l + qnorm(.975) * sqrt(l/t)
        ll <- l - qnorm(.975) * sqrt(l/t)
        mean(lambda > ll & lambda < ul )
    })
    
    plot(x = lambdas,y = coverage,type = 'l',col = 'red',lwd = 2,
         xlab = latex2exp::TeX("$\\lambda$"),ylab = "% coverage",
         main = latex2exp::TeX('Wald coverage % with $\\hat{\\lambda}=\\frac{\\lambda}{t}$'),ylim = c(0,1.05))
    abline(h = .95)
    rect(min_l,.95,max_l,1,col = rgb(0,0,0.4,.2))
    legend("bottomleft",legend = "over 95% coverage",col = rgb(0,0,0.4),lwd =2)
}