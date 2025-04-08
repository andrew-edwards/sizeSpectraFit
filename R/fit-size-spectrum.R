##' Fit a size spectrum to data using maximum likelihood
##'
##' The function automatically uses the appropriate method depending on the data...
##' For tibble, need `strata` if the data are different
##'
##' @param dat One of:
##' * `numeric` vector of values (such as individual body masses or lengths), which uses
##'   the MLE method (via the function [fit_size_spectrum.numeric()];
##' * `data.frame` that can be either
##'   * individual measurements for a strata (such as a year), for which column
##'   names are given by `strata` (user has to specify TODO or make it year and
##'   check other columns ) and `x` for the measurements. More column
##'   names can be in the data.frame but will not be used. In this case the MLE
##'   method `fit_size_spectrum.numeric()` will be applied separately to each
##'   year. TODO - need the functions OR
##'   * count data for MLEbin method. At a minimum this has to include the columns:
##'     * `bin_min`
##'     * `bin_max`
##'     * `bin_count`
##'
##'
##' TODO
##' @param x_min minimum value of data to fit the PLB distribution to. If `NULL`
##'   (the default) then it is set to the minimum value of the data (if `dat` is
##'   `numeric`), else to the minimum bin break of the lowest bin or ??TODO any
##'   other options? If not NULL then the fitting is restricted to value above
##'   `x_min`; for the MLEbin method this is the first full bin equal to or above `x_min`
##'   (i.e. first bin with `bin_min >= x_min`). Similarly for `x_max` (fitting
##'   is restricted to including the largest bin for which `bin_max <= x_max`).
##' TODO check the MLEbinfunction when finished. TODO Mention species-specific
##' @param x_max maximum value of data to fit the PLB distribution to. If `NULL`
##'   (the default) then it is set to the maximum value of the data (if `dat` is
##'   `numeric`), else to the maximum bin break of the highest bin or ??TODOany
##'   other options? If not NULL then the fitting is restricted to value below
##'   `x_max`; for the MLEbin method this includes the largest bin for which
##'   `bin_max <= x_max`. TODO  Mention species-specific...
##'
##' @return
##' * If `dat` is numeric then returns a list object of class
##'   `size_spectrum_numeric` (such that we can plot it
##'   with [plot.size_spectrum_numeric()], with objects.... (adapt below), also
##'   mention that data output can change with MLEbin method due to x_min and
##'   x_max; though could still keep them I guess. Prob best to keep.
##' :
##'   * TODOintervals: one-row tibble with columns:
##'     * median: median of the data
##'     * eti_lower: lower end of the ETI
##'     * eti_upper: upper end of the ETI
##'     * hdi_lower: lower end of the HDI
##'     * hdi_upper: upper end of the HDI
##'     * width_eti: width of the ETI
##'     * width_hdi: width of the HDI
##'     * width_diff: difference in widths, how much smaller (more certain) the
##'   HDI is than the ETI
##'     * i_eti_lower: index for which `eti_lower` is between
##'   `dens$x[i_eti_lower]` and `dens$x[i_eti_lower + 1]`
##'     * y_eti_lower: linearly interpolated value based on `i_eti_lower`
##'   corresponding to the density at `eti_lower`
##'     * i_eti_upper, y_eti_upper: similar to `...lower` but for `upper`
##'     * i_hdi_lower: index for which `dens$x[i_hdi_lower] = hdi_lower`. The
##'   theoretical true value of the lower bound of HDI will lie between
##'   `dens$x[i_hdi_lower - 1]` and `dens$x[i_hdi_lower]`, but the high `n` used
##'   should make this range small enough
##'     * y_hdi_lower: the density at `dens$y[i_hdi_lower]` corresponding to `hdi_lower`
##'     * i_hdi_upper: index for which `dens$x[i_hdi_upper] = hdi_upper`. The
##'   theoretical true value of the upper bound of HDI will lie between
##'   `dens$x[i_hdi_upper]` and `dens$x[i_hdi_upper + 1]` (note the asymmetry to
##'   `i_hdi_lower`), but the high `n` used should make this range small enough
##'     * y_hdi_upper: the density at `dens$y[i_hdi_upper]` corresponding to `hdi_upper`
##'     * hdi_height: the height of the pdf returned from `HDInterval::hdi()`,
##'   corresponding to either `y_hdi_lower` or `y_hdi_upper` (depending on which
##'   is the first `dens$x` value to push the integrated sum of the sorted
##'   cumulative `dens$y` values over `credibility`; see
##'   `HDInterval::hdi.density()`. Is `NA` if `density = FALSE`.
##'     * warning: logical, if `TRUE` then a warning was produced during the
##'   `HDInterval::hdi()` calculation. If no warning printed then this warning
##'   was "The HDI is discontinuous but allowSplit = FALSE; the result is a
##'   valid CrI but not HDI.", else the new warning "New type of warning in
##'   create_intervals()." is printed and needs investigating. See
##'   `plot.intervals_density()` with `show_discontinuity = TRUE` to plot the
##'   discontinuities in the HDI.
##'     * allow_hdi_zero: logical of `allow_hdi_zero` used
##'
##' * If `dat` is a data frame then return a list object of class
##'   `intervals_density_list` with:
##'   * element `[[i]]` corresponding to column `i` of the `dat_mcmc`. Each
##'    `[[i]]` element is itself a list of the form described above (since the
##'    intervals are calculated for each column in turn), plus also the
##'    `$name` element which is the name of column `i` of `dat_mcmc`.
##'   * intervals_all_years tibble of all the intervals, with the first column,
##'    `quantity`, corresponding to each column of `dat_mcmc`, such that row `i`
##'    corresponds to column `i` of `dat_mcmc`. `quantity` is numeric if no
##'    column names of `dat_mcmc` contain non-digits (e.g. represents years).
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' fit_size_spectrum(sim_vec)
##'
##' # See the vignettes for further details and refinements.
##' # Create intervals from the vector MCMC samples for hake recruitment in 2021:
##' res_vec <- create_intervals(rec_2021)
##' res_vec
##' plot(res_vec)    # Plot the default density plot showing the HDI
##'
##' # Create intervals from the data frame of MCMC samples for hake recruitment,
##' #  with each column representing a year:
##' res_df <- create_intervals(dplyr::select(hake_recruitment_mcmc, -"Virgin"))
##' res_df
##' plot(res_df)     # Plot the time series of calculated intervals
##' }
fit_size_spectrum <- function(dat,
                             ...){
  UseMethod("fit_size_spectrum")
}
