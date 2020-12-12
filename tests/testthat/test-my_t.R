test_that("non numeric input for x and mu throws error", {
  expect_error(my_t.test("string", "alternative", mu = 1 ))
  expect_error(my_t.test(x, "alternative", mu = "string"))
})

test_that("incorrect input for alternative throws error", {
  expect_error(my_t.test(x, "string", mu = 1))
  expect_error(my_t.test(x, 100, mu = 1))
})

test_that("output is always a list", {
  expect_is(my_t.test(1:10, "less", mu = 5), "list")
  expect_is(my_t.test(1:10, "greater", mu = 5), "list")
  expect_is(my_t.test(1:10, "two.sided", mu = 5), "list")
})


