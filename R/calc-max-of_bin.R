##' Calculate the maximum value of a bin given a value (that can represent the
##'  minimum or midpoint)
##'
##' Also depends on binbreak.
##'
##' @param vec vector of values corresponding to bins
##' @param bin_widths numeric of length 1 for all bins having same widths, or
##'   vector of two different allowed bin widths TODO think if need more, or
##'   generalise. Or tell people to do manually if not simply two options.
##' @param vec_represents_bin_min
##' @return tibble of bin_min, bin_max
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
calc_max_of_bin <- function(vec,
                            bin_widths = 1,
                            vec_represents_bin_min = TRUE
                            ){
  if(length(bin_widths) == 1){
    if(vec_represents_bin_min){
      # All bins are width bin_width and values represent minima
      bin_min <- vec
      bin_max <- bin_min + bin_widths
    } else {
      stop("TODO not implemented yet")
    }
  } else {
    if(!identical(bin_widths,
                 c(0.01, 1))){
      stop("TODO not yet implemented for other widths, tailoring for Med data")
    }
    if(vec_represents_bin_min){
      bin_width_vec <- rep(NA,
                           length(vec))

      bin_width_vec[vec %% 1 == 0] <- 1
      bin_width_vec[vec %% 1 != 0] <- 0.01

      bin_min <- vec
      bin_max <- bin_min + bin_width_vec
    } else {
      stop("TODO not implemented yet either")
    }
  }

  ret <- tibble::tibble(bin_min = bin_min,
                        bin_max = bin_max)
  ret
}
