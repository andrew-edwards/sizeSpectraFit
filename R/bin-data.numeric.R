##' @rdname bin_data
##' @param ... arguments to pass onto `bin_data.data.frame()`
##' @export
bin_data.numeric <- function(dat = NULL,
                             ...){
  if(!is.null(dat)){
    if(!is.vector(dat))stop("dat not a vector in bin_data")
    if(anyNA(dat)) stop("dat cannot contain NA's in bin_data")
    if(min(dat) <= 0)stop("dat needs to be >0 in bin_data")
  }

  # Then just make a data.frame of counts for each measurement, and then use
  #  `bin_data.data.frame()`

  counts_df <- as.data.frame(table(dat))   # This does order them numerically
  names(counts_df) <- c("x", "counts")
  counts_df$x <- as.numeric(levels(counts_df$x))

  yy <- bin_data.data.frame(counts_df,
                           ...)
  return(yy)
}
