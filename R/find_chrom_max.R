#' Find Chromatographic Peak Apex
#'
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @return a numeric value for the \code{rt} at the apex with the maximum intensity
#'
#' @export

find_chrom_max <- function(rt,int)
  {

  peak_info <- find_peaks(rt,int)

  max_peak <- peak_info[which(peak_info[,'int'] == max(peak_info[,'int'])),]

  return(max_peak[,'rt'])
}


