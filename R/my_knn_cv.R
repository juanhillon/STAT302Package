#' k-Nearest Neighbors Cross-Validation function
#'
#' This function predicts an output class using the input data's covariates.
#'
#' @param train Input data frame, training data.
#' @param cl Vector of true class value of training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list containing a vector of the predicted clas \code{class} and
#'   a numeric with the cross-validation misclassification error, \code{cv_err}.
#'
#'
#' @importFrom class knn
#' @importFrom stats na.omit
#' @importFrom dplyr filter
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  #randomly assigns observations to folds
  train$fold <- sample(rep(1:k_cv, length = nrow(train)))
  fold <- train$fold
  #creates column to store prediction for all observations
  class <- rep(NA, nrow(train))
  #creates vector to store error calculations
  misclass_rate <- rep(NA, k_cv)
  #uses for loop to repeat steps for each fold
  for (i in 1:k_cv) {
    #creates train and test data
    test_data <- train %>% filter(fold == i)
    train_data <- train %>% filter(fold != i)
    #creates vector of classes for both data sets
    cl_train <- train_data$species
    cl_test <- test_data$species
    #removes class and fold columns from datasets
    train_data <- train_data[ ,3:ncol(train_data)-1]
    test_data <- test_data[ ,3:ncol(test_data)-1]
    #stores predictions in dataset
    class[train$fold == i] <- knn(train_data,
                                  test_data,
                                  cl_train,
                                  k = k_nn,
                                  prob = TRUE)
    #creates vector to store misclassifications
    error <- rep(NA, nrow(test_data))
    #labels misclassifications
    for (j in 1:nrow(test_data)) {
      error[train$fold == j] =
        (as.numeric(train$species[train$fold == j])) != class[train$fold == j]
    }
    #calculates misclassification rate
    misclass_rate[i] <- sum(na.omit(as.numeric(error[train$fold == i])))
    misclass_rate[i] <- misclass_rate[i] / length(cl_test)
  }
  #calculates mean misclassification rate
  cv_err <- mean(misclass_rate)
  #creates list of objects to return
  my_list <- list()
  my_list$cv_err <- cv_err
  my_list$class <- class
  #returns list
  return(my_list)
}
