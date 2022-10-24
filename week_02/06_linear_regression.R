
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 06_linear_regressions.R
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

### --- Simple Linear Regression
# - Target: predict weight from Height

# - model_frame
model_frame <- fish_data %>% 
  select(Height, Weight)
linear_model <- lm(data = model_frame,
                   Weight ~ Height)
model_frame$predicted <- linear_model$fitted.values
model_frame$residuals <- linear_model$residuals

# - visualize relationship
ggplot(data = model_frame,
       aes(x = Height, y = Weight)) +
  geom_smooth(method = lm, se = FALSE, color = "red", size = .25) +
  geom_segment(aes(x = Height, 
                   y = predicted, 
                   xend = Height, 
                   yend = predicted + residuals),
               color = "black", size = .2, linetype = "dashed") +
  geom_point(aes(x = Height, y = Weight), color = "black", size = 1) +
  geom_point(aes(x = Height, y = Weight), color = "white", size = .5) +
  geom_point(aes(x = Height, y = predicted), color = "red", size = 1) +
  ggtitle("Weight ~ Height") +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(plot.title = element_text(hjust = .5))

# - correlation and R2
r <- cor.test(model_frame$Height,
              model_frame$Weight)
print(r$estimate)
print(r$estimate^2)

# - inspect model object: linear_model
summary(linear_model)

### --- linear_model
linear_model$coefficients
linear_model$residuals
sse <- sum(linear_model$residuals^2)
print(sse)
linear_model$fitted.values

### --- Predicting new data
new_fish_height = rnorm(100,
                        mean = mean(model_frame$Height),
                        sd = sd(model_frame$Height)
                        )
predictions <- data.frame(Height = new_fish_height, 
                          Weight = predict(linear_model,
                                           newdata = data.frame(
                                             Height = new_fish_height))
                          )
# - plot model predictions
ggplot(data = predictions, 
       aes(x = Height,
           y = Weight)) + 
  geom_smooth(method = lm, se = F, size = .25, color = "red") +
  geom_point(color = "black", size = 2) + 
  geom_point(color = "white", size = 1.5) +
  ggtitle("Predictions") + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))

### --- Coefficient of Determination (R2) unpacked
total_sse <- sum((model_frame$Weight - mean(model_frame$Weight))^2)
model_sse <- sum((linear_model$fitted.values - mean(model_frame$Weight))^2)
residual_sse <- sum(linear_model$residuals^2)
total_sse == model_sse + residual_sse
r2 <- cor(model_frame$Height, model_frame$Weight)^2
print(r2)
print(model_sse/total_sse)
# - also, do not forget:
x <- scale(model_frame$Height, 
           center = TRUE,
           scale = TRUE)
y <- scale(model_frame$Weight, 
           center = TRUE,
           scale = TRUE)
# - correlation is the covariance of scaled variables (z-scores)
cov(x, y)^2

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
k <- 1 # - number of predictors
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

