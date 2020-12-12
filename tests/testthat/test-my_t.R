test_that("non numeric input for x and mu throws error", {
  expect_error(my_t.test("string", "alternative", mu = 1 ))
  expect_error(my_t.test(x, "alternative", mu = "string"))
})

test_that("incorrect input for alternative throws error", {
  expect_error(my_t.test(x, "string", mu = 1))
  expect_error(my_t.test(x, 100, mu = 1))
})

test_that("output is a list", {
  expect_is(my_t.test(my_gapminder$lifeExp, "less", mu = 60), "list")
})

