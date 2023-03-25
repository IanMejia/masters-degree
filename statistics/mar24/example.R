# --- Libraries
library(fdth)
library(lmtest)
library(ggplot2)
# ---

# --- Example 1
smokers <- c(rep(1, 77), rep(0, 54), rep(1, 123), rep(0, 171))

illness <- c(rep(1, 77), rep(1, 54), rep(0, 123), rep(0, 171))

relation_data <- data.frame(smokers, illness)

table(relation_data)
#       illness
# smokes cases control
#    no     54     171
#    yes    77     123

model <- glm(illness ~ smokers, family = "binomial")

par(mfrow = c(2, 2))
plot(model)

summary(model)
# Deviance Residuals:
#     Min       1Q   Median       3Q      Max
# -0.9860  -0.9860  -0.7409   1.3817   1.6894
# Coefficients:
# (Intercept)  -1.1527     0.1561  -7.384 1.53e-13 ***
# smokes        0.6843     0.2133   3.209  0.00133 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
#     Null deviance: 525.03  on 424  degrees of freedom
# Residual deviance: 514.57  on 423  degrees of freedom
# AIC: 518.57
# Number of Fisher Scoring iterations: 4

# ---

# --- Example 2
dose <- c(1, 2, 3, 4, 5)
subjects <- c(20, 30, 30, 25, 20)
intoxicated <- c(2, 6, 9, 10, 12)

freq <- intoxicated / subjects
failure <- subjects - intoxicated

relation_data <- data.frame(
    dose, subjects, intoxicated, freq, failure,
    rep(0, 5)
)

relation_data <- setNames(relation_data, c(
    "dose", "subjects", "intox",
    "freq", "failure", "sex"
))

model_1 <- glm(relation_data$freq ~ relation_data$dose,
    weights = relation_data$subjects, family = "binomial"
)

#     weights = relation_data$subjects)
# Coefficients:
#        (Intercept)  relation_data$dose
#            -2.6814              0.6013
# Degrees of Freedom: 4 Total (i.e. Null);  3 Residual
# Null Deviance:	    14.92
# Residual Deviance: 0.2037 	AIC: 20.93

# success-failure matrix
md <- cbind(relation_data$intox, relation_data$failure)
#      [,1] [,2]
# [1,]    2   18
# [2,]    6   24
# [3,]    9   21
# [4,]   10   15
# [5,]   12    8

model_2 <- glm(md ~ relation_data$dose, family = "binomial")
# Coefficients:
#        (Intercept)  relation_data$dose
#            -2.6814              0.6013
# Degrees of Freedom: 4 Total (i.e. Null);  3 Residual
# Null Deviance:	    14.92
# Residual Deviance: 0.2037 	AIC: 20.93

# success-failure vector
sf_vector <- c(
    rep(1, sum(relation_data$subjects)),
    rep(0, sum(relation_data$failure))
)

dose_2 <- c(
    rep(relation_data$dose, relation_data$subjects),
    rep(relation_data$dose, relation_data$failure)
)

model_3 <- glm(sf_vector ~ dose_2, family = "binomial")
# Coefficients:
# (Intercept)       dose_2
#     -0.1322       0.1801
# Degrees of Freedom: 210 Total (i.e. Null);  209 Residual
# Null Deviance:	    285.3
# Residual Deviance: 282.5 	AIC: 286.5

intoxicated_2 <- c(4, 10, 16, 20, 19)

freq_2 <- intoxicated_2 / subjects
failure_2 <- subjects - intoxicated_2

relation_data_2 <- data.frame(
    dose, subjects, intoxicated, freq_2, failure_2,
    rep(1, 5)
)

relation_data_2 <- setNames(relation_data_2, c(
    "dose", "subjects", "intox",
    "freq", "failure", "sex"
))

data_frame <- rbind(relation_data, relation_data_2)

model_4 <- glm(data_frame$freq ~ data_frame$dose + data_frame$sex, weights = data_frame$subjects, family = "binomial")

summary(model_4)

# Deviance Residuals:
#      Min        1Q    Median        3Q       Max
# -0.63047  -0.49580   0.04172   0.55702   1.20603
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)
# (Intercept)      -3.3385     0.4770  -7.000 2.57e-12 ***
# data_frame$dose   0.7972     0.1255   6.352 2.12e-10 ***
# data_frame$sex    1.2467     0.3023   4.123 3.73e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
#     Null deviance: 69.0618  on 9  degrees of freedom
# Residual deviance:  3.8027  on 7  degrees of freedom
# AIC: 42.403
# Number of Fisher Scoring iterations: 4

par(mfrow = c(2, 2))
plot(model_4)
# ---
