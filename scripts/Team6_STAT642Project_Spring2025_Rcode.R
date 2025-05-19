## STAT 642, Spring 2025, Project. Team 6: Sai Sushma Mutyala, Stephanie King, Gustavo Romero, Kendra Wilson

## SAS code also submitted. 
## What is in this R code that is not in SAS: Profile plot, Hsu's procedure, check of log transform
## Included in this R code that is also in SAS (as a double check): ANOVA, Shapiro-Wilk test, Brown-Forsythe test, Normal probability plot, check of Box Cox transform

## LOAD PACKAGES
library(dplyr)
library(readxl)
library(tidyr)
library(car)  
library(MASS)
library(ggplot2)

## LOAD DATA
dataOriginal <- read.table(text = "
Time  Conc  Pretreatment       RS
4     0.5   Acid       0.60
4     0.5   Acid       0.60
4     0.5   Acid       0.70
4     1.0   Acid       0.70
4     1.0   Acid       0.70
4     1.0   Acid       0.80
4     1.5   Acid       0.80
4     1.5   Acid       0.80
4     1.5   Acid       0.90
4     0.5   Alkali     0.60
4     0.5   Alkali     0.60
4     0.5   Alkali     0.60
4     1.0   Alkali     0.60
4     1.0   Alkali     0.70
4     1.0   Alkali     0.70
4     1.5   Alkali     0.70
4     1.5   Alkali     0.70
4     1.5   Alkali     0.70
4     0.5   Enzyme     3.30
4     0.5   Enzyme     3.20
4     0.5   Enzyme     3.30
4     1.0   Enzyme     4.20
4     1.0   Enzyme     4.30
4     1.0   Enzyme     4.20
4     1.5   Enzyme     3.30
4     1.5   Enzyme     3.20
4     1.5   Enzyme     3.30
8     0.5   Acid       0.70
8     0.5   Acid       0.80
8     0.5   Acid       0.80
8     1.0   Acid       0.80
8     1.0   Acid       0.80
8     1.0   Acid       1.00
8     1.5   Acid       0.90
8     1.5   Acid       1.00
8     1.5   Acid       0.80
8     0.5   Alkali     0.70
8     0.5   Alkali     0.60
8     0.5   Alkali     0.70
8     1.0   Alkali     0.70
8     1.0   Alkali     0.80
8     1.0   Alkali     0.70
8     1.5   Alkali     0.90
8     1.5   Alkali     0.90
8     1.5   Alkali     0.80
8     0.5   Enzyme     4.40
8     0.5   Enzyme     4.40
8     0.5   Enzyme     4.30
8     1.0   Enzyme     5.20
8     1.0   Enzyme     5.30
8     1.0   Enzyme     5.20
8     1.5   Enzyme     4.30
8     1.5   Enzyme     4.20
8     1.5   Enzyme     4.30
12    0.5   Acid       3.20
12    0.5   Acid       3.20
12    0.5   Acid       3.30
12    1.0   Acid       3.40
12    1.0   Acid       3.40
12    1.0   Acid       3.40
12    1.5   Acid       3.50
12    1.5   Acid       3.50
12    1.5   Acid       3.40
12    0.5   Alkali     3.20
12    0.5   Alkali     3.20
12    0.5   Alkali     3.19
12    1.0   Alkali     3.20
12    1.0   Alkali     3.20
12    1.0   Alkali     3.20
12    1.5   Alkali     3.20
12    1.5   Alkali     3.30
12    1.5   Alkali     3.20
12    0.5   Enzyme     5.30
12    0.5   Enzyme     5.30
12    0.5   Enzyme     5.20
12    1.0   Enzyme     6.20
12    1.0   Enzyme     6.30
12    1.0   Enzyme     6.20
12    1.5   Enzyme     4.30
12    1.5   Enzyme     4.20
12    1.5   Enzyme     4.30
", header=TRUE)

data <- dataOriginal
# Change to factors to prepare for analysis
data$Time <- as.factor(data$Time)
data$Conc <- as.factor(data$Conc)
data$Pretreatment <- as.factor(data$Pretreatment)
data$Time <- gsub(" hours", "", data$Time)
data$Time <- factor(data$Time, levels = c(4, 8, 12))

# ANOVA: Run Type III ANOVA (done in SAS as well, R code used as a check)
model <- lm(RS ~ Time * Conc * Pretreatment, data = data)
anova_result <- Anova(model, Pretreatment = "III")
# Normality and Brown–Forsythe test (done in SAS as well, R code used as a check)
shapiro.test(model$residuals)
leveneTest(RS ~ interaction(Time, Conc, Pretreatment), data = data, center = "median")
anova_result
summary(model)

# QQ plot (done in SAS as well, R code used as a check)
qqnorm(model$residuals,
       main = "Normal Q-Q Plot of Residuals",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19,        # Use filled circles
       cex = 1.2)       # Increase point size
qqline(model$residuals, col = "steelblue", lwd = 2)
grid()                # Add a subtle grid

## PROFILE PLOT, separated by Pretreatment type
ggplot(data, aes(x = Time, y = RS, color = Conc, group = Conc)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  facet_wrap(~Pretreatment) +
  labs(title = "Profile Plot of Reduced Sugars (RS)", x = "Time", y = "Mean RS") +
  theme_minimal()

## PROFILE PLOT, all three factors
ggplot(data, aes(x = Time, y = RS, color = Pretreatment, group = interaction(Pretreatment, Conc))) +
  geom_line(aes(linetype = Conc), size = 1) +
  geom_point(aes(shape = Conc), size = 2) +
  labs(title = "Profile Plot: Reducing Sugar Content Over Time",
       x = "Time (hours)",
       y = "Reducing Sugars (RS)") +
  theme_minimal() +
  scale_color_manual(values = c("Acid" = "#FFB000", "Alkali" = "#DC267F", "Enzyme" = "#648FFF")) +
  guides(color = guide_legend(override.aes = list(shape = NA)), shape = guide_legend(title = "Conc")) + # Remove the shape legend
  theme(
    legend.position = "bottom", 
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )


## HSU's PROCEDURE
mcb_combination <- function(model, response_var = "RS", factor_vars = c("Time", "Conc", "Pretreatment"), best = "largest", alpha = 0.05) {
  if ("aov" %in% class(model) | "lm" %in% class(model)) {
    y <- model$model[[response_var]]
    trt <- interaction(model$model[, factor_vars], drop = TRUE)  # COMBINE the factors
    dfMSE <- df.residual(model)
    MSE <- deviance(model) / dfMSE
  }
  
  data_comb <- subset(data.frame(y, trt), !is.na(y))
  means <- tapply(data_comb[, 1], data_comb[, 2], mean)
  ni <- tapply(data_comb[, 1], data_comb[, 2], length)
  N <- sum(ni)
  ntr <- length(ni)
  
  # Load required package
  library(mvtnorm)
  
  # Compute standard errors
  se_diff <- sqrt(MSE * (1/ni + 1/ni))
  
  # Determine the best treatment
  if (best == "largest") {
    best_trt <- names(which.max(means))
  } else {
    best_trt <- names(which.min(means))
  }
  
  # Compute the test statistics
  diff_best <- means[best_trt] - means
  se_best <- sqrt(MSE * (1/ni[best_trt] + 1/ni))
  t_stats <- diff_best / se_best
  
  # Calculate critical value
  crit_val <- qmvt(1 - alpha, tail = "lower.tail", df = dfMSE, corr = diag(ntr))$quantile
  
  # Determine significance
  significant <- t_stats > crit_val
  
  # Output results
  results <- data.frame(
    Treatment = names(means),
    Mean = as.numeric(means),
    Best_Diff = as.numeric(diff_best),
    t_stat = as.numeric(t_stats),
    Significant = significant,
    Best = names(means) == best_trt  # Add a TRUE/FALSE column
  )
  
  results <- results[order(-results$Mean), ]
  
  list(
    Best_Treatment = best_trt,
    Results = results,
    Critical_Value = crit_val
  )
}

mcb_combination(model, response_var = "RS", factor_vars = c("Time", "Conc", "Pretreatment"), best = "largest", alpha = 0.05)


## ATTEMPTS TO IMPROVE NORMALITY 
# Box cox
bc <- boxcox(RS ~ Time * Conc * Pretreatment, data = data)
lambda_opt <- bc$x[which.max(bc$y)]
lambda_opt # lambda approx = 1

# Power transform
pt <- powerTransform(RS ~ Time * Conc * Pretreatment, data = data)
summary(pt)
pt$lambda # lambda approx = 1

# Log transform
data$Quality_log <- log(data$RS)
model_log <- lm(Quality_log ~ Time * Conc * Pretreatment, data = data)
anova_result_bc <- Anova(model_log, Pretreatment = "III")
shapiro.test(model_log$residuals) # further deviation from normality

