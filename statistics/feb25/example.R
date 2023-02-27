library(fdth)

# --- Stores
perm  <- function(a, b) {
    return(factorial(a) / (factorial(b) * factorial(a - b)))
}

tienda <- function(x, partition = 3) {
    f <- (choose(16, partition - x)
          * choose(4, x)
          / choose(20, partition))
    return(f)
}

x <- seq(0, 3)
fr <- tienda(x)
fc <- cumsum(fr)
df <- data.frame(x, fr, fc)

plot(x, fr, type = "l")
# ---

# --- Water wells
dbinom(3, 10, 0.3)
1 - pbinom(4, 10, 0.3)

x <- seq(0, 25)
fx <- dbinom(x, 25, 0.3)
fcx <- cumsum(fx)
df <- data.frame(x, fx, fcx)

plot(x, fx, type = "h")
# ---

# --- Particles
x <- seq(0, 10)
fx <- dpois(x, 4)
fcx <- ppois(x, 4)
df <- data.frame(x, fx, fcx)
plot(x, fx, type = "h")
# ---

# --- Normal probabilities
x <- seq(-10, 10, length = 1000)
fx <- dnorm(x, 8.2, 0.4)
fcx <- pnorm(x, 8.2, 0.4)
df <- data.frame(x, fx, fcx)
plot(x, fx, type = "l")
# ---
