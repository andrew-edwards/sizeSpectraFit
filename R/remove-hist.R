##' Remove histogram information from a list of objects of class
##'  `determine_xmin_and_fit_mlebins`, and also specific strata if desired.
##'
##' @param agg_list List of objects of class `determine_xmin_and_fit_mlebins`,
##'   from running `determine_xmin_and_fit_mlebins()` and then putting results
##'   into one list (TODO see vignette).
##' @param remove_strata vector of names of any strata to remove, each must be one of `names(agg_list)`.
##'
##' @return the input list but with the histogram information removed as well as
##'   any results (elements) of name in `remove_strata`.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
remove_hist <- function(agg_list,
                        remove_strata = NULL){
  return_agg <- list()

  for(i in 1:length(agg_list)){
    if("determine_xmin_and_fit_mlebins" %in% class(agg_list[[i]])){
      return_agg[[i]] <- agg_list[[i]]$mlebins
      # might need mlebin also if we create determine_xmin_and_fit_mlebin, or below TODO
    } else {
      return_agg[[i]] <- agg_list[[i]]    # presumably already the mlebins or mlebin results
    }
    names(return_agg)[i] <- names(agg_list)[i]
  }

  if(!is.null(remove_strata)){
    for(j in 1:length(remove_strata)){
      if(!(remove_strata[j] %in% names(agg_list))){
        stop("Each element of remove_strata must be in names(agg_list)")
      }

      return_agg <- within(return_agg,
                           rm(list = remove_strata[j]))  # can probably
                                        # remove loop, but it works fine
    }
  }

  return(return_agg)
}
