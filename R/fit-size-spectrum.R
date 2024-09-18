##' Fit a size spectrum to data using maximum likelihood
##'
##' The function automatically uses the method...
##'
##' TODO Works for a numeric vector (e.g. MCMC samples of an estimated population size) or
##' a data frame (such as MCMC samples of an estimated population size in 10
##' different years -- years would be the named columns, each row would be an
##' MCMC sample). Calculate the ETI, HDI (using appropriate functions from
##' HDInterval), their respective widths, kernel density, and other useful
##' information. See our manuscript, Appendix, and vignettes for
##' further background and uses.
##'
##' @param dat One of:
##' * `numeric` vector of values (such as individual body masses), which uses
##'   the MLE method (via the function [fit_size_spectrum.numeric()];
##' @param x_min minimum value of data to fit the PLB distribution to. If `NULL`
##'   (the default) then it is set to the minimum value of the data (if `dat` is
##'   `numeric`), else to the minimum bin break of the lowest bin or ??any
##'   other options? Mention species-specific
##' @param x_max maximum value of data to fit the PLB distribution to. If `NULL`
##'   (the default) then it is set to the maximum value of the data (if `dat` is
##'   `numeric`), else to the maximum bin break of the highest bin or ??any
##'   other options? Mention species-specific...
##' @return
##' * If `dat` is numeric then returns a list object of class
##'   `size_spectrum_numeric` (such that we can plot it
##'   with [plot.size_spectrum_numeric()], with objects:
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
