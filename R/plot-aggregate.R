##' Do an aggregated plot of several PLB fits, each to a separate (but related)
##' group of individuals.
##'
##' Given a list of MLE results (TODO check how many different types), combine
##' the data and show an aggregated distribtuion, as well as the individual fits.
##'
##' @param res_list list of results, with each component a list object of a
##'   given type TODO first doing for `size_spectrum_numeric`, might be somewhat
##'   automatic to generalise.
##' @param col_vec vector of colours to assign for each group
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' # See aggregating-size-spectra.Rmd TODO copy something to here maybe
##' }
##'
plot_aggregate <- function(res_list,
                           col_vec = c("orange", "lightblue", "green",
                                       "magenta"),
                           xlim_global = NULL,
                           ylim_global = NULL){

  if(length(res_list) > length(col_vec)){
    stop("Need to add more colours to col_vec to have one for each results component in res_list.")
  }

  if(!("list" %in% class(res_list[[1]]))){
    stop("res_list need to be a list of lists of MLE results.")
  }

  S <- length(res_list)

  # This is for size_spectrum_numeric:
  x_global <- numeric()
  for(s in 1:S){
    x_global <- c(x_global,
                  res_list[[s]]$x)
  }

  x_global <- sort(x_global,
                   decreasing = TRUE)

  if(is.null(xlim_global)){
    xlim_global <- range(x_global)
  }

  if(is.null(ylim_global)){
    ylim_global <- c(1, length(x_global))
  }

  # Extract required values
  b_vec <- numeric()
  n_vec <- numeric()
  xmin_vec <- numeric()
  xmax_vec <- numeric()
  for(s in 1:S){
    b_vec[s] <- res_list[[s]]$b_mle
    n_vec[s] <- length(res_list[[s]]$x)
    xmin_vec[s] <- res_list[[s]]$x_min
    xmax_vec[s] <- res_list[[s]]$x_max   # TODO change to x_max_vec etc. Maybe,
                                        # thought I was trying to be consistent
  }

  # Plot first one to automatically set up axes etc.
  plot(res_list[[1]],
       xlim = xlim_global,
       ylim = ylim_global,
       col = col_vec[1],
       fit_col = col_vec[1],
       legend_text_a = NA,
       legend_text_a_n = NA)

  # Full data
  points(x_global,
         1:length(x_global))

  # x values at which to calculate PLB's and PLB_agg; may have to do each one
  # manually here
  # Doing evenly on a log scale since range is quite large for aggregated, and
  # x-axis is always logged
  x_plb_agg <- 10^seq(log10(xlim_global[1]),
                      log10(xlim_global[2]),
                      length = 1000)     # x values to plot PLB

  #  Need to insert value close to xmax to make log-log curve go down further;
  #   since log(1 - pPLB(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
  x_plb_agg_length <- length(x_plb_agg)

  x_plb_agg <- c(x_plb_agg[-x_plb_agg_length],
                 0.9999999999 * x_plb_agg[x_plb_agg_length],
                 x_plb_agg[x_plb_agg_length])

  # Add aggregated distribution first so that the right-most distribution shows up okay as
  # it overlays the aggregated one (and is thinner line).

  y_plb_agg = (1 - pPLB_agg(x = x_plb_agg,
                            b_vec = b_vec,
                            n_vec = n_vec,
                            xmin = xmin_vec,
                            xmax = xmax_vec)) * sum(n_vec)
  lines(x_plb_agg,
        y_plb_agg,
        col = "yellow",
        lwd = 4)

  # Now do remaining groups, just add them manually here
  for(s in 2:S){
    points(sort(res_list[[s]]$x,
                decreasing = TRUE),
           1:length(res_list[[s]]$x),
           col = col_vec[s])

    x_plb <- 10^seq(log10(xmin_vec[s]),
                    log10(xmax_vec[s]),
                    length = 1000)     # x values to plot PLB

    #  Need to insert value close to xmax to make log-log curve go down further;
    #   since log(1 - pPLB(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
    x_plb_length <- length(x_plb)

    x_plb <- c(x_plb[-x_plb_length],
               0.9999999999 * x_plb[x_plb_length],
               x_plb[x_plb_length])

    lines(x_plb,
          (1 - pPLB(x = x_plb,
                    b = b_vec[s],
                    xmin = xmin_vec[s],
                    xmax = xmax_vec[s])) * n_vec[s],
          col = col_vec[s])
  }

}



# Based on what I had for sizeSpectra, but shoudl end up being more succinct.
# Copying MLE.plot_agg but then inputting a list of data, one vector for each
# sample, so can show the data on the plot like Juliana has.
MLE.plot_agg_list <- function(x_list,
                              b_vec,
                              n_vec,
                              xmin_vec,
                              xmax_vec,
                              confVals = NULL,
                              panel = FALSE,
                              log.xy = "xy",
                              mgpVals = c(1.6, 0.5, 0),
                              inset = c(0, -0.04),
                              xlim_global = NA,
                              ylim_global = NA,
                              col_vec = c("lightblue", "orange", "green", "magenta"),
                              ...
                              ){


  # HERE
  # Now plot each individual sample, plus the individually fitted PLB (based on the range of the data)
    for(s in 1:S){
      x.PLB = seq(xmin_vec[s],
                  xmax_vec[s],
                  length=1000)     # x values to plot PLB.  These are s-specific, as
                                   # just covering the correct range.

      #  Need to insert value close to xmax to make log-log curve go down further;
      #   since log(1 - pPLB(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
      x.PLB.length = length(x.PLB)
      x.PLB = c(x.PLB[-x.PLB.length],
                0.9999999999 * x.PLB[x.PLB.length],
                x.PLB[x.PLB.length])         # TODO make function for this as keep
                                             # repeating it

      y.PLB = (1 - pPLB(x = x.PLB,
                        b = b_vec[s],
                        xmin = min(x.PLB),
                        xmax = max(x.PLB))) * n_vec[s]

      lines(x.PLB,
            y.PLB,
            col = col_vec[s],
            lwd = 3)

#      points(sort(x[[s]],
#                  decreasing = TRUE),
#             1:length(x[[s]]),
#             col = col_vec[s])

  }
}
