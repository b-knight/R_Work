library(distances)
obs <- matrix(c(8, 9, 3, 4, 
                7, 4, 4, 5, 
                5, 3, 4, 4, 
                2, 3, 3, 4), nrow=4, ncol=4)

ob_1_2 <- ((8-7)^2+(9-4)^2+(3-4)^2+(4-5)^2)^(1/2)
ob_1_2 

distances(obs)


obs_means <- colMeans(obs)
obs_means
rowSums(obs)

distances(obs)

ob_1_2 <- ((8-7)^2+(9-4)^2+(3-4)^2+(4-5)^2)^(1/2)
ob_1_2 


(S <-  var(obs))
mahalanobis(c(0, 0, 0, 0), obs_means, S)


mahalanobis(c(0, 0), 1, S)

# x <- matrix(rnorm(100*3), ncol = 3)
# stopifnot(mahalanobis(x, 0, diag(ncol(x))) == rowSums(x*x))
# ##- Here, D^2 = usual squared Euclidean distances
# 
# Sx <- cov(x)
# D2 <- mahalanobis(x, colMeans(x), Sx)
# plot(density(D2, bw = 0.5),
#      main="Squared Mahalanobis distances, n=100, p=3") ; rug(D2)
# qqplot(qchisq(ppoints(100), df = 3), D2,
#        main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
#                            " vs. quantiles of" * ~ chi[3]^2))
# abline(0, 1, col = 'gray')
# # }

super_secret_shopper_algorithm <- lalonde


library(MatchIt)
matchit_results <- matchit(treat ~ age + educ + hispan + married,
                   data = super_secret_shopper_algorithm, 
                   method = "exact", 
                   distance = "mahalanobis")
summary(matchit_results)

