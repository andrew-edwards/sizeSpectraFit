##' @rdname bin_data
##' @export
bin_data.data.frame <- function(dat,
                                bin_width = NULL,
                                bin_breaks = NULL,
                                start_integer = TRUE){
  if(dim(dat)[2] != 2){
    stop("dat needs two cols in bin_data.data.frame()")
  }
  if(min(dat$x) < 0){
    stop("x values in dat need to be >= 0 in bin_data.data.frame")
  }
  if(min(dat$counts) < 0){
    stop("numbers in dat need to be >= 0 in bin_data.data.frame")
  }
  if(is.null(bin_width) & is.null(bin_breaks)){
    stop("need one of bin_width or bin_breaks in bin_data.data.frame")
  }
  if(!is.null(bin_width) & !is.null(bin_breaks)){
    stop("need only one of bin_width or bin_breaks in bin_data.data.frame")
  }
  if(start_integer != "TRUE" & start_integer != "FALSE"){
    stop("start_integer must be TRUE or FALSE in bin_data.data.frame")
  }

  x <- dat$x # measured values corresponding to counts
  min_x <- min(x)  # min(dplyr::pull(dat ,1))
  max_x <- max(x)  # max(dplyr::pull(dat ,1))

  if(!is.null(bin_breaks)){
    if(min_x < min(bin_breaks) | max_x > max(bin_breaks)){
      stop("bin_breaks do not span data in bin_data.data.frame")
    }
  } else {          # create bin_breaks based on bin_width
    if(bin_width == "2k"){
      if(start_integer){
        bin_breaks <- 2^( floor(log2(min_x)) : ceiling(log2(max_x)) )
      } else {
        stop("start_integer needs to be TRUE when
                   bin_width = 2k")
      }
    } else {    # If not "2k"
      if(!is.numeric(bin_width)){
      stop("bin_width must be 2k or a number (in quotes is okay)
                         in bin_data.data.frame().")
      }
      # start_integer says whether to start from an integer value or
      #  start from min(x),
      z <- floor(min_x) * start_integer + min_x * !start_integer
      bin_breaks <- seq(z,
                        by = bin_width,
                        length = ceiling((max_x - z)/bin_width) + 1)
    }
  }

  bin_for_each_x <- dat   # data.frame with one row for each x value

  bin_for_each_x$bin_mid <- cut(x,
                                breaks = bin_breaks,
                                right = FALSE,
                                include.lowest = TRUE,
                                labels = bin_breaks[-length(bin_breaks)] + 0.5*diff(bin_breaks))
  bin_for_each_x$bin_min <- cut(x,
                                breaks = bin_breaks,
                                right = FALSE,
                                include.lowest = TRUE,
                                labels = bin_breaks[-length(bin_breaks)])
  bin_for_each_x$bin_max <- cut(x,
                                breaks = bin_breaks,
                                right = FALSE,
                                include.lowest = TRUE,
                                labels = bin_breaks[-1])

  bin_for_each_x$bin_mid <- as.numeric(as.character(bin_for_each_x$bin_mid))
  bin_for_each_x$bin_min <- as.numeric(as.character(bin_for_each_x$bin_min))
  bin_for_each_x$bin_max <- as.numeric(as.character(bin_for_each_x$bin_max))

  # Calculate total counts in each bin class:
  bin_vals <- dplyr::summarise(dplyr::group_by(bin_for_each_x,
                                               bin_mid),
                               bin_min = unique(bin_min),
                               bin_max = unique(bin_max),
                               bin_width = bin_max - bin_min,
                               bin_count = sum(counts),         # was length(x) for sizeSpectra::binData()
                               bin_count_norm = bin_count / bin_width,
                               bin_sum = sum(x * counts),       # only appropriate for body masses
                               bin_sum_norm = bin_sum / bin_width )

  # Indices for minima of bins that have zero counts and so do not
  #  appear in bin_vals yet:
  empty_bin_min_ind <- !(signif(bin_breaks[-length(bin_breaks)], digits = 8) %in%
                         signif(bin_vals$bin_min, digits = 8))
                       # signif() to avoid not-real differences due to rounding/storing
  empty_bin_min <- bin_breaks[empty_bin_min_ind]
  empties <- length(empty_bin_min)
  empty_bin_max <- bin_breaks[-1][empty_bin_min_ind]
  empty_bin_width <- empty_bin_max - empty_bin_min
  empty_bin_mid <- empty_bin_min + empty_bin_width/2

  empty_vals <- as.data.frame(cbind(empty_bin_mid,
                                    empty_bin_min,
                                    empty_bin_max,
                                    empty_bin_width,
                                    matrix(0,
                                           nrow=empties,
                                           ncol=4)))
  names(empty_vals) <- names(bin_vals)
  bin_vals <- rbind(bin_vals, empty_vals)         # still a local df

  bin_vals <- bin_vals[order(bin_vals$bin_mid), ]   # order by bin_mid

  bin_vals <- dplyr::mutate(bin_vals,
                            log10_bin_mid = log10(bin_mid),
                            log10_bin_count = log10(bin_count),
                            log10_bin_sum = log10(bin_sum),
                            log10_bin_count_norm = log10(bin_count_norm),
                            # Had thought that last line is needed to avoid
                            # warnings (e.g. simulate-data2.R) and whole
                            # column being NA's. Maybe don't actually use it
                            # in results, but have put it in, may need to
                            # test it more.
                            log10_bin_sum_norm = log10(bin_sum_norm))
  bin_vals[is.infinite(bin_vals$log10_bin_count),
           "log10_bin_count"] <- NA
  # lm can't cope with -Inf, which appear if 0 counts in a bin
  bin_vals[is.infinite(bin_vals$log10_bin_count_norm),
           "log10_bin_count_norm"] <- NA
  bin_vals[is.infinite(bin_vals$log10_bin_sum),
           "log10_bin_sum"] <- NA
  bin_vals[is.infinite(bin_vals$log10_bin_sum_norm),
           "log10_bin_sum_norm"] <- NA
  if(dim(bin_for_each_x)[1] < 10^6) {       # only save bin_for_each_x if not too big
    y <- list(bin_for_each_x = bin_for_each_x,
              bin_vals = bin_vals)
  } else
  {
    y <- list(bin_vals = bin_vals)
  }
  return(y)
}
