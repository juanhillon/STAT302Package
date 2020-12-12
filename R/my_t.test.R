#' T test function
#'
#' This function takes an input dataset, and performs a null hypothesis t-test.
#'
#' @param x Numeric vector of data.
#' @param alternative Character string specifying the alternative hypothesis,
#'   only accepts "two.sided", "less", or "greater".
#' @param mu Number indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return List containing the numeric test statistic \code{test_stat}, numeric
#'   degrees of freedom \code{df}, string value of parameter \code{alternative},
#'   and numeric p-value \code{p_val}.
#'
#' @examples
#' my_t.test(1:10, "two.sided", mu = 1)
#' my_t.test(1:10, "greater", mu = 0.1)
#' my_t.test(1:10, "less", mu = 10)
#'
#' @importFrom stats sd
#' @importFrom stats pt
#'
#' @export
my_t.test <- function(x, alternative, mu){
  #calculates standard error
  std_error <- sd(x) / sqrt(length(x))
  #calculates test statistic
  test_stat <- (mean(x) - mu) / std_error
  #calculates degrees of freedom
  df <- length(x) - 1
  #find p_val depending on alternative, if no valid alternative, returns error
  if(alternative == "less"){
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if(alternative == "greater"){
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if (alternative == "two.sided"){
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE) +
      pt(-1 *abs(test_stat), df, lower.tail = TRUE)
  } else {
    stop("alternative can only be two.sided, less, or greater")
  }
  #returns list of variables
  return(list(test_stat, df, alternative, p_val))
}
