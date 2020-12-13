#' Linear model function
#'
#' This function fits data to a linear model.
#'
#' @param formula Formula class object, describes model to be fitted to.
#' @param data Input data frame.
#' @keywords inference
#'
#' @return Table with rows for each coefficient, and columns \code{Estimate},
#'   \code{Std. Error}, \code{t value}, and \code{Pr(>|t|)}
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap, my_gapminder)
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#'
#' @importFrom kableExtra kable_styling
#' @importFrom stats pt
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom kableExtra kable
#'
#' @export
my_lm <- function(formula, data){
  #extracts model frame
  model_frame <- model.frame(formula, data)
  #extracts model matrix x
  x <- model.matrix(formula, data = data)
  #extracts model response y
  y <- model.response(model_frame)
  #finds coefficients using given formula
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  #calculates degrees of freedom
  df <- nrow(data) - ncol(model_frame)
  #calculates sigma^2
  sigma_2 <- sum(((y - x %*% beta) ^ 2) / df)
  #sets negative values in matrix to zero
  my_solve <- sigma_2 * solve((t(x) %*% x))
  for (i in 1:nrow(my_solve)) {
    for(j in 1:ncol(my_solve)){
      if(my_solve[i, j ] < 0){
        my_solve[i,j] = 0
      }
    }
  }
  #calculates std error
  std_error <- diag(sqrt(my_solve))
  #calculates t values
  t_val <- beta / std_error
  #Calculates Pr(>|t|)
  Pr_less_than_abs_t <- 2 * pt(abs(t_val), df, lower.tail = FALSE)
  #creates data frame for table
  output <- data.frame("Estimate" = c(beta),
                       "Std. Error" = c(std_error),
                       "t value" = c(t_val),
                       "Pr(>|t|)" = c(Pr_less_than_abs_t)
  )
  #properly labels rownames
  rownames(output) <- rownames(beta)
  #creates table
  return(kable_styling(kable(output)))
}
