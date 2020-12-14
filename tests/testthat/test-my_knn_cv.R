#sample data
#removes observations with na values
train <- na.omit(my_penguins)
#removes unnecessary columns from dataset
train$island <- NULL
train$sex <- NULL
train$year <- NULL
cl <- train$species

test_that("output is a list", {
  expect_is(my_knn_cv(train, cl, 1, 5), "list")
})

test_that("each component of output is of correct type", {
  expect_is(my_knn_cv(train, cl, 1, 5)$cv_err, "numeric")
  expect_is(my_knn_cv(train, cl, 1, 5)$class, "factor")
})

test_that("incorrect inputs throw errors", {
  expect_error(my_knn_cv("train", cl, 1, 5))
  expect_error(my_knn_cv(train, cl, "string", 5))
  expect_error(my_knn_cv(train, cl, 1, "string"))
})
