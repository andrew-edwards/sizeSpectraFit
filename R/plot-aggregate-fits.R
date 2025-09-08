##' Plot the aggregate fits for several strata
##'
##' Uses the results from [plot_aggregate_mlebin()] to plot aggregated size
##' spectra for several strata on a single plot. Has options for normalising and
##' restricting x range to give justifiable comparisons.
##'
##' @param agg_list list of aggregated fits, with each component corresponding
##'   to the list object resulting from [plot_aggregate_mlebin()] for a
##'   particular strata.
##'col_vec vector of colours to assign for each group
##' @param strata_names
##' @param log_axes which axes to log (gets passed to `plot(..., log =
##'   log_axes)`, either `"xy"` or `"x"` are of interest (although `"y"`
##'   and `""` automatically work).
##' @param restrict logical, whether or not to restrict the aggregated fits to
##'   only being above a common x value
##' @param normalise logical, whether or not to normalise each aggregated fit by
##'   it's maximum, so that all fits are on the same scale
##' @param col_strata
##' @param xlim, ylim vectors for axes limits; if NULL (the default) it gets
##'   calculated automatically, but users likely want to then manually adjust
##'   them, especially `ylim` to be able to see all fits asymptoting at their
##'   xmin values, but the
##'   default axis will go way too low (it is hard to automate this, so just
##'   refine `ylim` to celarly show the aymptotes).
##' @param ... additional arguments passed onto [plot()]
##' @return list with two objects, `x_plb_agg` and `y_plb_agg`, which are the
##'   fitted x and y values for the aggregated size spectrum (which does not
##'   have a simple exponent). To then use for plotting multiple strata in [plot_aggregate_fits()].
##' @export
##' @author Andrew Edwards
##' @examples
##' # See aggregating-size-spectra.Rmd TODO copy something to here maybe
##' }
##'
plot_aggregate_fits <- function(agg_list,
                                strata_names,
                                log_axes = "xy",
                                restrict = TRUE,
                                normalise = TRUE,
                                col_strata = c("blue",
                                               "darkgreen",
                                               "red"),
                                xlim = NULL,
                                ylim = NULL, #
                                ...){    # passed onto plot(...)

   # TODO check works for agg_list of just length 1

  num_strata <- length(strata_names)

  if(!("list" %in% class(agg_list))){
    stop("agg_list need to be a list.")
  }

  if(length(agg_list) > length(strata_names)){
    stop("Need agg_list and strata_names to be the same length, one name corresponding to each component of agg_list.")
  }

  if(length(col_strata) != num_strata){
    stop(paste("Need col_strata to have", num_strata,
               "colours, one for each strata."))
  }


  if(restrict){
    # x start for restricting all strata to start at the same value (max of the
    #  x_mins) when plotting
    x_restrict_start <- lapply(agg_list,
                               function(agg_list_one_strata){
                                 min(agg_list_one_strata$x_plb_agg)
                               }) %>%
      unlist() %>%
      max()
  } else {
    # just set to the min of the xmins, so nothing being restricted by the rest
    #  of the code will work fine; no need for more if statements.
    x_restrict_start <- lapply(agg_list,
                               function(agg_list_one_strata){
                                 min(agg_list_one_strata$x_plb_agg)
                               }) %>%
      unlist() %>%
      min()
  }

  if(is.null(xlim)){
    x_max_max <- lapply(agg_list,
                        function(agg_list_one_strata){
                          max(agg_list_one_strata$x_plb_agg)
                        }) %>%
      unlist() %>%
      max()

    xlim = c(x_restrict_start,
             x_max_max)
  }


  # Now determine the new x and y for each agg_list component (each
  # strata). Restricted regarding starting, and then normalised.

  agg_fit_x <- list()    # restricted
  agg_fit_y <- list()
  agg_fit_y_norm <- list()

  for(i in 1:length(agg_list)){
    agg_fit_x[[i]] <-
      agg_list[[i]][["x_plb_agg"]][agg_list[[i]][["x_plb_agg"]] >=
                                    x_restrict_start]
    agg_fit_y[[i]] <-
      agg_list[[i]][["y_plb_agg"]][agg_list[[i]][["x_plb_agg"]] >=
                                    x_restrict_start]

    ifelse(normalise,
           agg_fit_y_norm[[i]] <- agg_fit_y[[i]] / max(agg_fit_y[[i]]),
           agg_fit_y_norm[[i]] <- agg_fit_y[[i]])  # so not normalised, but rest
                                        # of code will work
  }

  if(is.null(ylim)){
    ymin <- lapply(agg_fit_y_norm,
                   min) %>%
      unlist() %>%
      min()

    ymax <- lapply(agg_fit_y_norm,
                   max) %>%
      unlist() %>%
      max()

    ylim = c(ymin,
             ymax)
  }

  plot(agg_fit_x[[1]],
       agg_fit_y_norm[[1]],
       log = log_axes,                  # TODO generalise options, prob use ...
       type = "l",
       xlim = xlim,
       ylim = ylim,
       xlab = expression(paste("Body mass, ",
                               italic(x), "(g)")),
       ylab = ifelse(normalise,
                     expression(paste("Proportion of ", counts >= x),
                                sep=""),
                     expression(paste("Total ", counts >= x),
                         sep="")),
       col = col_strata[1],
       axes = FALSE,
       lwd = 2,
       ...)

  box()

  add_ticks(log = log_axes)

  if(length(agg_list) > 1){
    for(j in 2:length(agg_list)){
      lines(agg_fit_x[[j]],
            agg_fit_y_norm[[j]],
        col = col_strata[j],
        lwd = 2)
    }
  }


legend(x = "topright",
       legend = strata_names,
       col = col_strata,
       lwd = 2)
       # pch = pch_strata,
       # pt.cex = pch_cex,
       # inset = legend_inset,
       # bty = legend_bty)

}
