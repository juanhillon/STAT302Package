library(tidyr)
data("my_penguins")
my_data <- drop_na(my_penguins)
test_that("output is numeric", {
  expect_is(my_rf_cv(2), "numeric")
})
