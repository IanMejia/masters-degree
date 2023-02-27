# --- imports
library(fdth) # Frequency Distribution Tables, Histograms and Polygons
# ---

# --- Datasets
ages <- c(2, 1, 1, 3, 4, 1, 2)
weight <- c(10.2, 5.3, 5.6, 16.2, 22.1, 5.8, 10.1)

datos <- data.frame(ages, weight) # data matrix

fuma <- c(1, 1, 1, 5, 7, 8, 9, 1, 10, 15, 13, 28, 21, 25, 20, 20, 21, 23, 23,
          11, 11, 15, 9, 11, 10, 15, 13, 13, 17, 17, 18, 18, 19, 19, 3, 1, 1,
          5, 5, 7, 6, 6, 8, 10, 10, 11, 15, 10, 21, 18)

# documentation for `fdt` can be called with `>?fdt`
table <- fdt(fuma, breaks = c("Sturges")) # equation 2 in notes

r <- max(fuma) - min(fuma)

m <- 1 + 3.3 * log10(length(fuma)) # eqation 1 in notes

round(m) # rounding up the expected data

a <- r / m

# ---

# --- graphs
# histogram
hist(fuma, breaks = "Sturges")

# freq polygon
plot(table, type = "fp") # frequency polygon [tendencies]
plot(table, type = "cfp") # polygon of relative freqs
plot(table, type = "cdh") # cummulative density histogram
plot(table, type = "fh") # freqs histogram
plot(table, type = "rfh") # relative freqs histogram

# pie charts
sum(table$table$rf)
# ---

# --- Statistics
summary(fuma)

boxplot(fuma)

quantile(fuma, seq(0, 1, 0.25))
# ---
