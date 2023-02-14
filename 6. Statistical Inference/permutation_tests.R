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

## Permutation Test For Groups B & C

mu_diff_BC <- teststat(c("B","C"))

perms_BC <- sapply(1:1e4,function(i) teststat(groups = c("C","B"),permute = TRUE))

# p-value of mean_perm_diff_mean = mu_diff_BC (given that it is true)

mean(perms_BC < mu_diff_BC)

# plot + quantile of p-value
dx <- density(perms_BC)
plot(dx,main = "bootstrapped density")
points(x = quantile(perms_BC,prob = 1 - 0),y = 0,cex = 1.5,col = "black",pch = 16)
polygon(c(dx$x[dx$x >= qnorm(.95,sd = sd(perms_BC))],quantile(perms_BC,probs = 1,sd = sd(perms_BC))), 
        c(dx$y[dx$x >= qnorm(.95,sd = sd(perms_BC))],quantile(perms_BC,probs = 0,sd = sd(perms_BC))),
        col = rgb(1, 0, 0, alpha = 0.5), border = "red", main = "",ylim = c(0,02))
legend("topright",legend = c("p-value"),col = "black",pch = 16)

## Permutation Test For Groups D & E

mu_diff_DE <- teststat(c("D","E"))

perms_DE <- sapply(1:1e4,function(i) teststat(groups = c("D","E"),permute = TRUE))

# p-value of mean_perm_diff_mean = mu_diff_DE (given that it is true)

p <- mean(perms_DE < mu_diff_DE)
print(p)
# plot + quantile of p-value
dx <- density(perms_DE)
plot(dx,main = "bootstrapped density")
points(x = quantile(perms_DE,prob = p),y = 0,cex = 1.5,col = "black",pch = 16)
polygon(c(dx$x[dx$x <= qnorm(.05,sd = sd(perms_DE))],quantile(perms_DE,probs = p,sd = sd(perms_BC))), 
        c(dx$y[dx$x <= qnorm(.05,sd = sd(perms_DE))],quantile(perms_DE,probs = 0,sd = sd(perms_BC))),
        col = rgb(1, 0, 0, alpha = 0.5), border = "red", main = "",ylim = c(0,02))
legend("topright",legend = c("p-value"),col = "black",pch = 16)