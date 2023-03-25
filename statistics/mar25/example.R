# --- Libraries
library(tidyverse)
library(markovchain)
library(psych)
library(ISLR)
library(MASS)
# ---

# --- Example 1
data <- Default
data <- data %>%
    select(default, balance) %>%
    mutate(default = recode(default, "No" = 0, "Yes" = 1))

head(data)

logistic_model <- glm(default ~ balance, data = data, family = "binomial")

(ggplot(data = data, aes(x = balance, y = default)) +
    geom_point(aes(color = as.factor(default)), shape = 1) +
    stat_function(fun = function(x) {
        predict(logistic_model,
            newdata = data.frame(balance = x),
            type = "response"
        )
    }) +
    theme_bw() +
    labs(
        title = "Logistic Regression",
        y = "Default Probability"
    ) +
    theme(legend.position = "none"))
# ---

# --- Example 2
weight <- c(54.2, 65, 78, 77.3, 88.3)
age <- c(22, 32, 27, 24, 21)
height <- c(1.62, 1.69, 1.75, 1.7, 1.78)

df <- data.frame(weight, age, height)

mu <- colMeans(df)

sigma <- cov(df)

inverse <- solve(sigma)

r <- cor(df)

# mahalanobis is a segmentation method. Using an average as a value
# alongside a p_value for deviation, we can effectively determine if the
# values are grouped.
df$distance <- sqrt(mahalanobis(df, mu, sigma))

df$p <- pchisq(df$distance, df = 3, lower.tail = FALSE)
# ---

# --- Example 2
file <- "datos_est.csv"
df <- read.csv(file)

r <- cor(df)

# PCA is meaningless if there is no correlation between the variables
# With a small enough correlation determinant, we can conclude that there is
# a high correlation within the dataset.
det(r)
# [1] 9.009434e-06

pca <- princomp(df, cor = TRUE)

summary(pca)
# Importance of components:
#                           Comp.1    Comp.2    Comp.3     Comp.4
# Standard deviation     1.9992490 1.6227619 0.9643947 0.44818373
# Proportion of Variance 0.4996246 0.3291695 0.1162571 0.02510858
# Cumulative Proportion  0.4996246 0.8287941 0.9450512 0.97015980
#                            Comp.5     Comp.6      Comp.7      Comp.8
# Standard deviation     0.35832730 0.22955645 0.202940803 0.128226392
# Proportion of Variance 0.01604981 0.00658702 0.005148121 0.002055251
# Cumulative Proportion  0.98620961 0.99279663 0.997944749 1.000000000
eigen_values <- pca$sdev^2
eigen_vectors <- pca$loadings

# Graphically, we can see that the only viable PCAs to pick, are those greater
# than one on the Y axis.
plot(1:8, eigen_values,
    type = "b", xlab = "Principal Components",
    ylab = "Variances", main = "Sedimentation"
)

# Correlation between components and variables
r_pca <- sweep(pca$loadings[1:8, 1:8], 2, pca$sdev, "*")

par(mfrow = c(1, 2))
eqscplot(r_pca[, 1:2], xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
abline(h = 0, v = 0)
text(r_pca[, 1:2], labels = colnames(df), cex = 0.8)
symbols(0, 0, circles = 1., inches = FALSE, add = TRUE)
biplot(pca, cex = 0.75)

pca_varimax <- principal(df, nfactors = 2, rotate = "varimax")

biplot(pca_varimax, labels = rownames(df), cex = 0.75, main = "")

# ---

# --- Example 4
p <- matrix(c(0, .5, .5, .5, 0, .5, .5, .5, 0), nrow = 3, byrow = TRUE)

mc <- new("markovchain", transitionMatrix = p, states = c("a", "b", "c"),
          name = "Chain 1")

summary(mc)
par(mfrow = c(1, 1))
plot(mc)

x_0 <- c(.5, .2, .3)

# probability distribution for 6 steps
x_6 <- x_0 * (mc)^6
#              a       b         c
# [1,] 0.3359375 0.33125 0.3328125

statisctical_distribution <- steadyStates(mc)
# ---
