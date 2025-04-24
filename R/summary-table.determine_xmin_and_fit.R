##' @rdname summary_mle_table
##' @export
summary_mle_table.determine_xmin_and_fit <- function(res,
                                            dig = 2){
  # stopifnot("intervals_density" %in% class(int_dens))

  res_mle <- res$mle_fit

  # Just does one row here, for multiple fits could make the row a function. Or
  # put all results into one tibble.
  cat("| Exponent $$b$$| Confidence interval | $$x_{min}$$ | $$x_{max}$$ | \n",
      "| :-------| ----------:| -----:| -----:| \n",
      "| ", f(res_mle$b_mle, dig) , "| (",
      f(res_mle$b_conf[1], dig), ",",
      f(res_mle$b_conf[2], dig), ") | ",
      f(res_mle$x_min, dig), " | ",
      f(res_mle$x_max, dig), "| \n")
}
