test_that("string input gives error", {
  expect_error(my_lm("lifeExp ~ gdpPercap", my_gapminder))
  expect_error(my_lm(lifeExp ~ gdpPercap, "my_gapminder"))
})

test_that("output is of correct type",{
  expect_type(my_lm(lifeExp ~ gdpPercap, my_gapminder), "character")
})
