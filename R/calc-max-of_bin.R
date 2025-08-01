##' Calculate the maximum value of each bin minimum or midpoint
##'
##' Likely deprecated, see [calc_bin_breaks()]. TODO decide if want to keep
##' this, keep the 0.01/1 code somewhere in case want to incorporate it, though
##' splitting up like I'm doing for Med data seems best. Actually, might need it
##' for that, just need to change name maybe.
##'
##' Given a vector of values representing either the midpoint or the minimum of
##' each bin, calculate the correponding maximum value of each bin. The defined
##' bin widths can be specified (currently either all identical or some are 0.01
##' and some 1).
##'
##' @param vec vector of values corresponding to bins
##' @param bin_widths numeric of length 1 to specify all bins having same widths, or
##'   vector of two different allowed bin widths TODO think if need more, or
##'   generalise. Or tell people to do manually if not simply two options.
##' @param vec_represents_bin_mid logical, if `TRUE` then `vec` represents the
##'   midpoints of bins, if `FALSE` then represents the minima of bins.
##' @return tibble of bin_min, bin_max
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24)) %>% a()
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24), bin_widths = c(0.01, 1))  %>% a()
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24), vec_represents_bin_mid = FALSE)  %>% a()
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24), vec_represents_bin_mid = FALSE, bin_widths = c(0.01, 1))  %>% a()
##'
##' }
calc_max_of_bin <- function(vec,
                            bin_widths = 1,
                            vec_represents_bin_mid = TRUE
                            ){
  if(length(bin_widths) == 1){
    # All bins are width bin_width
    if(vec_represents_bin_mid){
      # Values represent bin midpoints
      bin_min <- vec - bin_widths/2
      bin_max <- bin_min + bin_widths
    } else {
      # Values represent bin minima
      bin_min <- vec
      bin_max <- bin_min + bin_widths
    }
  } else {
    if(!identical(bin_widths,
                  c(0.01, 1))){
      stop("TODO not yet implemented for other widths, tailoring for Med data")
    }
    if(vec_represents_bin_mid){
      # Values represent bin midpoints
      bin_width_vec <- rep(NA,
                           length(vec))

      bin_width_vec[vec %% 1 == 0] <- 1
      bin_width_vec[vec %% 1 != 0] <- 0.01

      bin_min <- vec - bin_width_vec/2
      bin_max <- bin_min + bin_width_vec
    } else {
      # Values represent bin minima
      bin_width_vec <- rep(NA,
                           length(vec))

      bin_width_vec[vec %% 1 == 0] <- 1
      bin_width_vec[vec %% 1 != 0] <- 0.01

      bin_min <- vec
      bin_max <- bin_min + bin_width_vec
    }
  }

  ret <- tibble::tibble(bin_min = bin_min,
                        bin_max = bin_max)
  ret
}
