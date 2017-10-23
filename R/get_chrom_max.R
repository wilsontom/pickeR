#' Find Chromatographic Peak Apex
#'
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @return a numeric value for the \code{rt} at the apex with the maximum intensity
#'
#' @export

get_chrom_max <- function(rt, int)
{
  peaks <- get_peaks(rt, int)

  if (nrow(peaks) == 1) {
    return(peaks[, 'rt'])
  }
  if (nrow(peaks) > 1) {
    chrom_max <- which(peaks[, 'int'] == max(peaks[, 'int']))
    return(peaks[chrom_max, 'rt'])
  }
}
