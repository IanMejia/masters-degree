# --- Libraries
library(fdth)
library(lmtest)
library(ggplot2)
# ---

# --- Example 1 (Absorbance)
x <- c(0.5, 0.6, 0.7, 0.8, 1, 1.3, 1.7, 2.5, 3.3)
y <- c(0.15, 0.18, 0.23, 0.24, 0.3, 0.39, 0.52, 0.73, 0.88)

plot(x, y)

model <- lm(y ~ x)

summary(model)

# correlation coefficient
(r <- cor(x, y))

# determination coefficient
r_2 <- r^2

model_data <- data.frame(x, y, model$fitted.values, model$residuals)

error <- model$residuals

par(mfrow = c(1, 2))
qqnorm(error)
qqline(error)
hist(error)

# normality test
shapiro.test(error)

# variance similarity test
bptest(model, studentize = FALSE)

# error independency
dwtest(model, alternative = "two.sided")
# ---

# --- Example 2
pressure <- c(50, 60, 70, 90, 100)
vols <- c(64.7, 51.3, 40.5, 25.9, 7.8)

plot(pressure, vols)

pressure_star <- log(pressure)

model <- lm(pressure_star ~ vols)

error <- model$residuals

par(mfrow = c(1, 2))
qqnorm(error)
qqline(error)
hist(error)

r <- cor(vols, pressure)
# ---

# --- Example 3
cholesterol <- c(
    350, 190, 263, 320, 280, 198, 232, 320, 303, 220, 405, 190,
    230, 227, 440, 318, 212, 340, 195, 223
)
age <- c(
    80, 30, 42, 50, 45, 35, 18, 32, 49, 35, 50, 20, 40, 30, 30, 23, 35, 18, 22,
    41
)
fats <- c(
    35, 40, 15, 20, 35, 50, 70, 40, 45, 35, 50, 15, 20, 35, 80, 40,
    40, 80, 15, 34
)

patient_data <- data.frame(cholesterol, age, fats)
plot(patient_data)

# model
model <- lm(cholesterol ~ age + fats)
# Coefficients:
# (Intercept)          age         fats
#      89.264        2.496        2.344

summary(model)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -95.823 -41.071  -4.804  39.584  88.339
# Coefficients:
# (Intercept)  89.2643    49.6902   1.796  0.09022 .
# age           2.4960     0.9175   2.720  0.01454 *
# fats          2.3440     0.6913   3.391  0.00348 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 56.59 on 17 degrees of freedom
# Multiple R-squared:  0.4734,	Adjusted R-squared:  0.4114
# F-statistic: 7.641 on 2 and 17 DF,  p-value: 0.004293

error <- model$residuals

par(mfrow = c(1, 2))
qqnorm(error)
qqline(error)
hist(error)

# normality test
shapiro.test(error)

# variance similarity test
bptest(model, studentize = FALSE)

# error independency
dwtest(model, alternative = "two.sided")
# ---

# --- Example 4
conc <- c(1, 1.5, 2, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7, 8, 9, 10, 11, 12, 13, 14, 15)
resis <- c(
    6.3, 11.1, 20, 24, 26.1, 30, 33.8, 34, 38.1, 39.9, 42, 46.1, 53.1,
    52, 52.5, 48, 42.8, 27.8, 21.9
)
wood_data <- data.frame(concentration = conc, resistance = resis)

(ggplot(wood_data, aes(x = concentration, y = resistance))
 + geom_point())

resistance_linear_model <- lm(resistance ~ concentration, data = wood_data)
resistance_pol_model <- lm(resistance ~ concentration + I(concentration^2),
                           data = wood_data)

error_lm <- resistance_linear_model$residuals

par(mfrow = c(1, 2))
qqnorm(error_lm)
qqline(error_lm)
hist(error_lm)

error_pm <- resistance_pol_model$residuals
qqnorm(error_pm)
qqline(error_pm)
hist(error_pm)

(ggplot(wood_data, aes(x = concentration, y = resistance))
 + geom_point()
 + geom_smooth(method = "lm", formula = y ~ x)
 + geom_smooth(method = "lm", formula = y ~ x + I(x^2))
)

anova(resistance_linear_model, resistance_pol_model)

par(mfrow = c(2, 2))
plot(resistance_pol_model)
# ---
