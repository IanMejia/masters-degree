# --- Libraries
library(fdth)
# ---

# --- Example 1
# a
pnorm(210, 200, 10) # 84.13%
# b
qnorm(1 - 0.1, 200, 10) # 212.81
# ---

# --- Example 2
weigths <- c(
    2.64, 2.83, 3.11, 2.19, 2.48, 2.60, 2.45, 2.51, 2.39, 2.50, 2.14,
    2.18, 2.67, 2.31, 1.63, 2.51, 2.62, 3.02, 2.51, 2.23, 2.52, 2.51,
    2.62, 2.44
)

# a
hist(weigths)

# b
1 - pnorm(2, 2.5, 1) # 69.14%

# c
qnorm(1 - .2, 2.5, 1) # 3.34

weights_ran <- rnorm(1000, 2.5, 1)
hist(weights_ran)
# ---

# --- Example 3
x <- seq(-10, 10, len = 1000)
fx <- dnorm(x, 0, 1)
fcx <- pnorm(x, 0, 1)
plot(x, fx, type = "l")
# ---

# --- Example 4
data <- c(
    .060, 1.827, .372, .610, .521, 1.189, .537, .898, .319, .603, .614,
    .374, .411, .406, .533, .788, .449, .348, .413, .662, .273, .262,
    1.925, .767, 1.117, 2.464, .448, .550, .385, .307, .571, .971, .622,
    .674, 1.499
)

t.test(data, alternative = "two.sided", mu = 0.0, conf.level = .95)
# ---

# --- Example 5
resistance <- c(
    348.3, 378.9, 329.6, 379.3, 348.8, 367.7, 358.4, 378.2, 377.9,
    341.8
)

x <- sum(as.numeric(resistance > 350))
prop.test(x, length(resistance),
    alternative = "two.sided", p = .5,
    conf.level = .95, correct = TRUE
)

prop.test(x, length(resistance),
    alternative = "two.sided", p = .5,
    conf.level = .99, correct = TRUE
)
# ---

# --- Example 6
smokers <- c(
    10.0, 8.4, 12.8, 25.0, 11.8, 9.8, 12.5, 15.4, 23.5, 9.4, 25.1,
    19.5, 25.5, 9.8, 7.5, 11.8, 12.2, 15.0
)

non_smokers <- c(
    30.0, 30.1, 15.0, 24.1, 30.5, 17.8, 16.8, 14.8, 13.4, 28.5,
    17.5, 14.4, 12.5, 20.4
)

t.test(smokers, non_smokers)
t.test(non_smokers, smokers)
# ---

# --- Example 7
group_a <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
group_b <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)

t.test(group_a, group_b, paired = TRUE)
# ---
