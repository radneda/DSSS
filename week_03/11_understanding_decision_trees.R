### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 3. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 11_understanding_decision_trees.R
### --- description: a function to decide a split in a Decision Tree model
### --- with glmnet() and cv.glmnet()
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

### ----------------------------------------------------------------------------

### --- setup
library(rpart)
library(rpart.plot)

### --- Titanic dataset
data(Titanic)
data_set <- TitanicSurvival
data_set <- data_set[complete.cases(data_set), ]

### --- decision tree model w. rpart
# - complexity parameter = 0
dec_tree <- rpart(survived ~ .,
                  data = data_set)
rpart.plot(dec_tree)

# - predictions
predictions <- predict(dec_tree, 
                       newdata = data_set, 
                       type = "prob")
predictions <- ifelse(predictions[, 1] <= predictions[, 2],
                     "yes",
                     "no")

# - elementary ROC analysis
acc <- sum(predictions == data_set$survived)/length(data_set$survived)
print(acc)
hit <- sum(predictions == "yes" & data_set$survived == "yes")/sum(data_set$survived == "yes")
print(hit)
fa <- sum(predictions == "yes" & data_set$survived == "no")/sum(data_set$survived == "no")
print(fa)

### --- How is a split decided?

### --- First: a function to compute Gini Impurity
# - for classification trees
# - Gini Impurity
# - nodes data.frame to test with:
nodes <- data.frame(class_counts = c(142, 232))

gini_binary <- function(nodes) {
  
  # - probabilities
  p <- nodes$class_counts/sum(nodes$class_counts)

  # - Gini Impurity
  gini <- 1 - sum(p^2)
  
  return(gini)
  
}

gini_binary(nodes)

### --- Second: a function to decide a split
# - for both numerical and categorical predictors
attribute_split <- function(params) {
  
  # - input
  # - the data:
  data <- params[[1]]
  # - the attribute (feature, predictor) that we wish to test
  attribute <- params[[2]]
  # - the outcome variable 
  target <- params[[3]]

  # - type of attribute
  att <- which(colnames(data) == attribute)
  atype <- class(data[, att])
  
  # - switch categorical and continuous
  if (atype == "factor") {
    
    # - CATEGORICAL
    
    # - cardinality
    acard <- length(unique(data[, att]))
  
    # - possible splits
    design <- combn(unique(data[, att]), 2)
    design <- matrix(as.character(design),
                     ncol = acard)

    # - If cardinality == 2
    if (acard == 2) {
      
      # - left
      wd <- which(data[, att] == design[, 1])
      d <- data[wd, target]
      n1 <- length(d)
      td <- as.numeric(table(d))
      td <- data.frame(class_counts = td)
      gini_split_1 <- gini_binary(td)
      # - right
      wd <- which(data[, att] == design[, 2])
      d <- data[wd, target]
      n2 <- length(d)
      td <- as.numeric(table(d))
      td <- data.frame(class_counts = td)
      gini_split_2 <- gini_binary(td)
      # - Gini
      n <- n1 + n2
      g <- n1/n*gini_split_1 + n2/n*gini_split_2

      # - output
      return(list(splits = design,
                  gini = g))
      
    # - If cardinality > 2  
    } else {
      
      # - gini for all possible splits
      g <- apply(design, 2, function(x) {
        # - left
        wd <- which(data[, att] %in% x)
        d <- data[wd, target]
        n1 <- length(d)
        td <- as.numeric(table(d))
        td <- data.frame(class_counts = td)
        gini_split_1 <- gini_binary(td)
        # - right
        wd <- which(!(data[, att] %in% x))
        d <- data[wd, target]
        n2 <- length(d)
        td <- as.numeric(table(d))
        td <- data.frame(class_counts = td)
        gini_split_2 <- gini_binary(td)
        # - Gini
        n <- n1 + n2
        return(n1/n*gini_split_1 + n2/n*gini_split_2)
      })
      
      # - output
      return(list(splits = design,
                  gini = g))
      
    }
    
  } else {
    
    # - CONTINUOUS
    # - note: if CONTINUOUS
    # - we measure MSE
    
    # - lower and upper bounds
    lower_b <- min(data[, att]) + 1
    upper_b <- max(data[, att]) - 1
    
    # - objective
    min_gini <- function(split) {
      
      # - right
      wd <- which(data[, att] > split)
      d <- data[wd, target]
      n1 <- length(d)
      td <- as.numeric(table(d))
      td <- data.frame(class_counts = td)
      gini_split_1 <- gini_binary(td)
      # - left
      wd <- which(data[, att] <= split)
      d <- data[wd, target]
      n2 <- length(d)
      td <- as.numeric(table(d))
      td <- data.frame(class_counts = td)
      gini_split_2 <- gini_binary(td)
      
      n <- n1 + n2
      return(n1/n*gini_split_1 + n2/n*gini_split_2)
      
    }
    
    # - optimization: grid-search
    c <- seq(lower_b, upper_b, by = .1)
    ginies <- sapply(c, min_gini)
    
    # - output
    return(list(split = c[which.min(ginies)[1]],
                gini = ginies[which.min(ginies)]))
  
  }
  
}

### --- Test

# - fist split:
params <- list(data_set, "sex", "survived")
attribute_split(params)
params <- list(data_set, "passengerClass", "survived")
attribute_split(params)
params <- list(data_set, "age", "survived")
attribute_split(params)

# - split at sex == "male"
params <- list(data_set[data_set$sex=="male", ], "age", "survived")
attribute_split(params)

# - split at sex == "female"
params <- list(data_set[data_set$sex=="female", ], "passengerClass", "survived")
attribute_split(params)

