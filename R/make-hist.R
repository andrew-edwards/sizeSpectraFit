##' Convert a vector of body sizes into a histogram list object and create 0 counts for missing bins.
##'
##' TODO See R/sizeSpectraHake/make_hist.R if want to do a tibble; then prob
##' have to make generic functions, so this would become make_hist.numeric.
##'
##' Can then use `plot()` which calls `plot.histogram()`. Without the 0 counts for missing bins
##' `plot.histogram()` does not plot counts because bins appear to have unequal widths.
##'
##' @param x numeric vector of values.
##' @param bin_width numeric bin width for the data set.
##' @param bin_start numeric value for the first bin to start at; if `NULL` then
##'   set to the next `bin_width` value below `min(x)`. If `bin_width = 1` then this will
##'   be the integer below `min(x)` (i.e. `min(x) %/% 1` TODO add to tests). If,
##'   say, `min(x) = 10.6` and `bin_width = 0.5` then `bin_start = 10.5`. TODO
##'   lots of tests.
##' @return a histogram list object with components (see `?hist`):
##'  - `breaks`
##'  - `mids`
##'  - `counts`
##'  - `xname`  TODO make this the default `"Body length (x), mm"`,
##'  - `equidist` TRUE since have equal bin widths
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODO counts_per_bin_example
##' make_hist(counts_per_bin_example)
##' TODO for test do
##' x <- 1:49
##' and make up other ones
##' }
make_hist <- function(x,
                      bin_width = 1,
                      bin_start = NULL){
  if(is.null(bin_start)){
    bin_start <- min(x) - min(x) %% bin_width
  }

  breaks <- seq(from = bin_start,
                to = max(x) + bin_width,
                by = bin_width)
  # Given seq generates values up to the sequence value below `to`, this should
  #  ensure final bin has max(x) in it. But if max(x) is exactly a bin break,
  #  need to remove the top 0 count. Doing below.
  h <- hist(x,
            breaks = breaks,
            plot = FALSE)

  n <- length(h$counts)

  if(h$counts[n] == 0){
    h$breaks <- h$breaks[-(n+1)]
    h$counts <- h$counts[-n]
    h$density <- h$density[-n]
    h$mids <- h$mids[-n]
  }

  h
}
