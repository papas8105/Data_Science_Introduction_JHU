dim(InsectSprays)
names(InsectSprays)
library(RColorBrewer)
boxplot(count ~ spray,data = InsectSprays,col = brewer.pal(6,"RdPu"))

## teststat generic function

teststat <- function(groups = c("A","B"),permute = FALSE) {
    if (isTRUE(permute)) {
        count  <- InsectSprays$count[InsectSprays$spray %in% groups]
        label  <- c(rep(groups[1],length(which(InsectSprays$spray == groups[1]))),
                    rep(groups[2],length(which(InsectSprays$spray == groups[2]))))
        perms  <- sample(label)
        return(mean(count[perms == groups[2]]) - mean(count[perms == groups[1]]))
    }
    mean(
     InsectSprays$count[InsectSprays$spray == groups[2]] - InsectSprays$count[InsectSprays$spray == groups[1]]
        )
}
## Permutation tests check whether labels play a significant role or not
## Permutation Test For Groups B & C

mu_diff_BC <- teststat(c("B","C"))

perms_BC <- sapply(1:1e4,function(i) teststat(groups = c("B","C"),permute = TRUE))

# p-value computation
## H_0:  mu_diff_BC  = mean(perms_BC)
## H_a:  mu_diff_BC != mean(perms_BC)

p <- 2 * min(mean(mu_diff_BC > mean(perms_BC)),mean(mu_diff_BC < mean(perms_BC))) # reject, favor H_a
print(p)
# plot + quantile of p-value
dx <- density(perms_BC)
s <- sd(perms_BC)
plot(dx,main = "bootstrapped density")
points(x = quantile(perms_BC,prob = p),y = 0,cex = 1.5,col = "black",pch = 16)
polygon(x = c(quantile(perms_BC,probs = 0,sd = s),dx$x[dx$x <= quantile(perms_BC,probs = 0.025,sd = s)]),
        y = c(dx$y[dx$x <= quantile(perms_BC,probs = 0.025,sd = s)],0),col = "red",border = "red")
polygon(x = c(dx$x[dx$x >= quantile(perms_BC,probs = 0.975,sd = s)],quantile(perms_BC,probs = 1,sd =s)),
        y = c(0,dx$y[dx$x >= quantile(perms_BC,probs = 0.975,sd = s)]),col = "red",border = "red")
legend("topright",legend = c("p-value"),col = "black",pch = 16)

## Permutation Test For Groups D & E

mu_diff_DE <- teststat(c("D","E"))

perms_DE <- sapply(1:1e4,function(i) teststat(groups = c("D","E"),permute = TRUE))

# p-value computation
## H_0: mu_diff_DE = mean(perms_DE)
## H_a: mu_diff_DE != mean(perms_DE)
## fail to reject

p <- 2 * min(mean(perms_DE < mu_diff_DE),mean(perms_DE > mu_diff_DE))
print(p)
# plot + quantile of p-value
dx <- density(perms_DE)
s  <- sd(perms_DE)
plot(dx,main = "bootstrapped density")
points(x = quantile(perms_DE,probs = p),y = 0,cex = 1.5,col = "black",pch = 16)
polygon(x = c(quantile(perms_DE,probs = 0,sd = s),dx$x[dx$x <= quantile(perms_DE,probs = 0.025,sd = s)]),
        y = c(dx$y[dx$x <= quantile(perms_DE,probs = 0.025,sd = s)],0),border = "red",col = "red")
polygon(x = c(quantile(perms_DE,probs = 0.975,sd = s),dx$x[dx$x >= quantile(perms_DE,probs = 0.975,sd = s)]),
        y = c(0,dx$y[dx$x >= quantile(perms_DE,probs = 0.975,sd = s)]),border = "red",col = "red")
legend("topright",legend = c("p-value"),col = "black",pch = 16)

# p-value computation
## H_0: mu_diff_CE  = mean(perms_CE)
## H_a: mu_diff_CE != mean(perms_CE)

mu_diff_CE <- teststat(c("C","E"))
perms_CE <- sapply(1:1e4,function(i) teststat(groups = c("C","E"),permute = TRUE))

p <- 2 * min(mean(perms_CE < mu_diff_CE),mean(perms_CE > mu_diff_CE))
print(p) # fail to reject but too close to alpha level
dx <- density(perms_CE)
plot(dx,main = "bootstrapped density")
points(x = quantile(perms_CE,probs = p),y = 0,cex = 1.5,col = "black",pch = 16)
polygon(x = c(quantile(perms_CE,probs = 0,sd = s),dx$x[dx$x <= quantile(perms_CE,probs = 0.025,sd = s)]),
        y = c(dx$y[dx$x <= quantile(perms_CE,probs = 0.025,sd = s)],0),border = "red",col = "red")
polygon(x = c(dx$x[dx$x >= quantile(perms_CE,probs = 0.975,sd = s)],quantile(perms_BC,probs = 1,sd =s)),
        y = c(0,dx$y[dx$x >= quantile(perms_CE,probs = 0.975,sd = s)]),col = "red",border = "red")
legend("topright",legend = c("p-value"),col = "black",pch = 16)