#' Peak Detection
#'
#' Detect peals using \code{pracma::findPeaks}
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @return a \code{peak_info} \code{data.frame}. See also \code{\link{get_peak_info}}
#'
#' @export

get_peaks <- function(rt, int)
{
  noise <- estimate_noise(int)

  int_sm <- smooth_peak(rt, int, ford = 4)

  peak_indicies <- pracma::findpeaks(int_sm, threshold = noise)

  peak_limits <-
    data.frame(matrix(nrow = nrow(peak_indicies), ncol = 3))
  names(peak_limits) <- c("left", "apex", "right")
  peak_limits[, "left"] <- peak_indicies[, 3]
  peak_limits[, "apex"] <- peak_indicies[, 2]
  peak_limits[, "right"] <- peak_indicies[, 4]

  peak_info <- get_peak_info(rt, int_sm, peak_limits)

  return(peak_info)

}
