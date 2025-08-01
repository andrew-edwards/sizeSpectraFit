##' Given a tibble that contains a column of measurements, append minimum and
##' maximum for each bin
##'
##' Given a tibble of measurements identified by `measurement`, create new
##' columns of `<measurement>_bin_min` and `<measurement>_bin_max`
##' representing minimum and maximum of each bin. Options specify whether the
##' original `measurement` represents the midpoint of minimum of each bin. Bin
##' width can also be specified.
##'
##' @param tib tibble values including a column identified by `measurement`
##' @param measurement character description of the column of interest, to then
##'   use to generate names for the new columns
##' @param bin_width numeric value of assumed bin width. If multiple bin widths
##'   are needed then split the data up and apply the function each time, then
##'   combine together. If multiple bin widths might have been used for one type
##'   of data but we can't filter those easily, then see [calc_max_of_bin()]
##'   TODO check.
##' @param represents character describing what the `measurement` column
##'   is assumed to be: either the `min` (minimum), `midpoint`, or `max` (maximum) of the bins.
##' @return tibble of the input with appended columns `<measurement>_bin_min`
##'   and `<measurement>_bin_max`.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # TODO
##' # calc_bin_breaks(mediterranean_data, represents = "max") # etc.
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24)) %>% a()
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24), bin_widths = c(0.01, 1))  %>% a()
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24), vec_represents_bin_mid = FALSE)  %>% a()
##' calc_max_of_bin(c(1:5, 7, 9.83, 11.24), vec_represents_bin_mid = FALSE, bin_widths = c(0.01, 1))  %>% a()
##'
##' }
calc_bin_breaks <- function(tib,
                            measurement = "length",
                            bin_width = 1,
                            represents = "midpoint"
                            ){
  stopifnot("`represents` must be min, midpoint, max" =
              represents %in% c("min", "midpoint", "max"))

  # Do each represents separately (rather than get clever and do in
  # one go).

  measures_vec <- dplyr::pull(tib,
                              measurement)


  if(represents == "min"){
    bin_min_values <- measures_vec
    bin_max_values <- measures_vec + bin_width
  }

  if(represents == "midpoint"){
    bin_min_values <- measures_vec - bin_width/2
    bin_max_values <- measures_vec + bin_width/2
  }

  if(represents == "max"){
    bin_min_values <- measures_vec - bin_width
    bin_max_values <- measures_vec
  }

  new_bin_min_name <- paste0(measurement,
                             "_bin_min")
  new_bin_max_name <- paste0(measurement,
                             "_bin_max")

  result <- cbind(tib,
                  new_bin_min = bin_min_values,
                  new_bin_max = bin_max_values) %>%
    as_tibble() %>%
    dplyr::rename_with(~ new_bin_min_name,
                       new_bin_min) %>%
    dplyr::rename_with(~ new_bin_max_name,
                       new_bin_max)

  result
}
