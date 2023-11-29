#' Title myci
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

myci <- function(x) {
  n <- length(x)
  alpha <- 0.05
  t_value <- qt(1 - alpha / 2, df = n - 1)
  margin_of_error <- t_value * sd(x) / sqrt(n)
  mean_estimate <- mean(x)

  lower_bound <- mean_estimate - margin_of_error
  upper_bound <- mean_estimate + margin_of_error

  ci <- c(lower_bound, upper_bound)
  return(ci)
}
set.seed(23)
x <- rnorm(30, mean = 10, sd = 12)

result <- myci(x)
print(result)


