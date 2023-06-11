# asthma

# Load required libraries
library(dplyr)
library(ggplot2)
library(car)
library(pROC)

# Read the dataset (replace 'asthma_data.csv' with your actual data file)
data <- read.csv("asthma_data.csv")

# Define the outcome variable and risk factors
outcome_var <- "death"
risk_factors <- c("age", "sex", "cpd", "lifestyle")

# Assumption checks
for (factor in risk_factors) {
  formula <- as.formula(paste(outcome_var, "~", factor))
  model <- glm(formula, data = data, family = binomial)
  
  # Check assumptions
  car::crPlots(model, which = 1)
  car::crPlots(model, which = 3)
  roc_obj <- roc(data[[outcome_var]], fitted(model))
  plot(roc_obj, print.thres = "best", print.auc = TRUE)
}

# Fit logistic regression models and store results
model_results <- list()
for (factor in risk_factors) {
  formula <- as.formula(paste(outcome_var, "~", factor))
  model <- glm(formula, data = data, family = binomial)
  model_results[[factor]] <- model
}

# Evaluate goodness of fit for each model
goodness_of_fit <- lapply(model_results, function(model) {
  list(
    deviance = model$deviance,
    df_resid = model$df.residual,
    p_value = pchisq(model$deviance, model$df.residual, lower.tail = FALSE),
    AIC = AIC(model)
  )
})

# Print goodness of fit results
for (factor in risk_factors) {
  cat("Factor:", factor, "\n")
  cat("Deviance:", goodness_of_fit[[factor]]$deviance, "\n")
  cat("DF Residual:", goodness_of_fit[[factor]]$df_resid, "\n")
  cat("P-value:", goodness_of_fit[[factor]]$p_value, "\n")
  cat("AIC:", goodness_of_fit[[factor]]$AIC, "\n\n")
}

# Plot logistic regression curves for each risk factor
plot_curves <- function(model, factor) {
  xv <- seq(min(data[, factor]), max(data[, factor]), length.out = 100)
  yv <- predict(model, newdata = data.frame(factor = xv), type = "response")
  
  ggplot(data, aes_string(x = factor, y = outcome_var)) +
    geom_point(shape = 21, fill = "yellow") +
    geom_line(aes(x = xv, y = yv), color = "blue") +
    labs(title = paste("Logistic Regression Curve for", factor)) +
    xlab(factor) +
    ylab("Probability of Death") +
    theme_minimal()
}

# Plot logistic regression curves for each risk factor
for (factor in risk_factors) {
  plot_curves(model_results[[factor]], factor)
}
