##' Simulated vector of 1000 simulated values from a power-law distribution
##'
##' As used for Figures 1 and 2 of MEE paper, used as a default set of
##' values. Paramaters are `b = -2`, `xmin = 1`, xmax = 1000`, and the seed was
##' set to 42.
##'
##' @format numeric
##' @examples
##' \dontrun{
##' head(sim_vec)
##' }
##' @author Andrew Edwards
##' @source Generated from `data-raw/sim_vec.R`.
"sim_vec"

##' Results of the fits as shown in Table B.1 (to use as the example code to
##' plot Figure B.20 of Quevedo et al., 2026)
##'
##' @format tibble
##' @examples
##' \dontrun{
##' quevedo_table_b1
##' knitr::kable(quevedo_table_b1,
##'              digits = 2)       # As shown in Table B.1 (2 decimal places)
##' plot_multiple_exponents(quevedo_table_b1,         # Plot Figure B.20
##'                         shade_first = TRUE)
##' }
##' @author Andrew Edwards
##' @source Generated from `data-raw/mediterranean-results.R`.
"quevedo_table_b1"

