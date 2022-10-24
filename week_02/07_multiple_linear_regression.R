
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 07_multiple_linear_regressions.R
### --- description: Linear and Multiple Linear Regression
### --- with lm() in R.ss
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

### ----------------------------------------------------------------------------

### --- Setup
# - install.packages("ppcor")
library(tidyverse)
data_dir <- paste0(getwd(), "/_data/")

### --- Dataset
# - Kaggle: https://www.kaggle.com/datasets/aungpyaeap/fish-market
# - place it in your _data/ directory
fish_data <- read.csv(paste0(data_dir, "Fish.csv"), 
                      header = TRUE,
                      check.names = FALSE,
                      stringsAsFactors = FALSE)

### --- Multiple Linear Regression
# - Target: predict weight from all continuous predictors

# - model_frame
str(fish_data)
model_frame <- fish_data %>% 
  select(-Species)

### --- Multiple Linear Regression
# - R: formula interface, outcome ~ predictors:
linear_model <- lm(data = model_frame,
                   Weight ~ Length1 + Length2 + Length3 + Height + Width)
linear_model <- lm(data = model_frame,
                   Weight ~ .)
summary(linear_model)

### --- Visualize model predictions
predictions <- linear_model$fitted.values
prediction_frame <- data.frame(observation = model_frame$Weight, 
                               prediction = predictions)
ggplot(data = prediction_frame,
       aes(x = prediction, y = observation)) +
  geom_abline(intercept = 0, 
              slope = 1, 
              color = "black", 
              size = .25) +
  geom_point() +
  xlim(c(-500, 2000)) + ylim(c(-500, 2000)) + 
  ggtitle("Model Predictions\nNote: identity line") +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(plot.title = element_text(hjust = .5))

# - inspect model object: linear_model
summary(linear_model)

### --- linear_model
linear_model$coefficients
linear_model$residuals
sse <- sum(linear_model$residuals^2)
print(sse)
linear_model$fitted.values

### --- Coefficient of Determination (R2) unpacked
total_sse <- sum((model_frame$Weight - mean(model_frame$Weight))^2)
model_sse <- sum((linear_model$fitted.values - mean(model_frame$Weight))^2)
residual_sse <- sum(linear_model$residuals^2)
total_sse == model_sse + residual_sse
model_summary <- summary(linear_model)
r2 <- model_summary$r.squared
print(r2)
print(model_sse/total_sse)

### --- The distribution of residuals
model_residuals <- data.frame(residuals = linear_model$residuals)
ggplot(data = model_residuals,
       aes(x = residuals)) + 
  geom_histogram(binwidth = 10) + 
  ggtitle("Model Residuals") +
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))

# - qqplot
ggplot(model_residuals, aes(sample = residuals)) + 
  stat_qq() + 
  stat_qq_line() + 
  ggtitle("Q-Q Plot of Model Residuals") +
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))

### --- Heteroskedacity
# - Predicted vs. residuals {ggplot2}
predictions <- linear_model$fitted.values
residuals <- linear_model$residuals
predicted_res_frame <- data.frame(predicted = predictions,
                                  residual = residuals)
ggplot(data = predicted_res_frame,
       aes(x = predicted, y = residual)) +
  geom_point(size = 1.5) +
  geom_smooth(method = 'lm',
              size = .25,
              alpha = .25, 
              color = "red", 
              se = FALSE) + 
  ggtitle("Predicted vs Residual") + 
  xlab("Predicted") + ylab("Residual") + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))

### --- Influential Cases
inf_frame <- influence.measures(linear_model)
inf_frame <- as.data.frame(inf_frame$infmat)

# - Cook's distance
# - Cook and Weisberg (1982) 
# - consider values greater than 1 to be problematic
w_cookD <- which(inf_frame$cook.d >1)
print(w_cookD)

# - Leverage: hat values (inf_frame$hat values)
# - Average Leverage = (k+1)/n
# - k - num. of predictors
# - n - num. observations
# - Also termed: hat values, range: 0 - 1
# - Various criteria (twice the average leverage, three times the average leverage...)
k <- 5 # - number of predictors
n <- dim(model_frame)[1] # - number of observations
# - Various criteria, we will use > twice the leverage:
w_leverage <- which(inf_frame$hat > 2*((k + 1)/n))
print(w_leverage)

## Influence plot
inf_plot_frame <- data.frame(residual = linear_model$residuals,
                             leverage = inf_frame$hat,
                             cookD = inf_frame$cook.d)
ggplot(inf_plot_frame,
       aes(y = residual,
           x = leverage)) +
  geom_point(size = inf_plot_frame$cookD*100, 
             shape = 1, 
             color = "blue") +
  ggtitle("Influence Plot\nSize of the blob corresponds to Cook's distance") +
  theme(plot.title = element_text(size=8, face="bold")) +
  ylab("Standardized Residual") + xlab("Leverage") + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))

### --- Multicolinearity
predictors <- model_frame %>% 
  select(-Weight)
cor(predictors)

# - install.packages(car)
library(car)
car::vif(linear_model)
# - NOTE: 
# The lower bound of VIF is 1; 
# - there is no upper bound;
# - VIF > 2 indicates high variance inflation
sqrt(car::vif(linear_model))
# - important: 
# - sqrt(vif) shows how much larger a standard error 
# - of a regression coefficient is due to multicolinearity, 
# - compared to a hypothetical situation where 
# - there were no multicolinearity.

### --- Categorical predictors in Linear Regression
### --- Dummy Coding/One-Hot Encoding
model_frame <- fish_data
glimpse(model_frame)

# - prepare categorical predictor: factor()
table(model_frame$Species)
model_frame$Species <- as.factor(model_frame$Species)
class(model_frame$Species)
levels(model_frame$Species)
is.ordered(model_frame$Species)

# - Multiple Linear Regression
linear_model <- lm(data = model_frame,
                   Weight ~ .)
summary(linear_model)

# - change baseline for Species
model_frame$Species <- relevel(model_frame$Species, 
                               ref = "Perch")
linear_model <- lm(data = model_frame,
                   Weight ~ .)
summary(linear_model)

### --- Visualize model predictions
predictions <- linear_model$fitted.values
prediction_frame <- data.frame(observation = model_frame$Weight, 
                               prediction = predictions)
ggplot(data = prediction_frame,
       aes(x = prediction, y = observation)) +
  geom_abline(intercept = 0, 
              slope = 1, 
              color = "black", 
              size = .25) +
  geom_point() +
  xlim(c(-500, 2000)) + ylim(c(-500, 2000)) + 
  ggtitle("Model Predictions\nNote: identity line") +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(plot.title = element_text(hjust = .5))


