#' Peak Detection
#'
#'
#'
#'
#'
#'


peak_detection <- function(rt, int)
  {

  # rt int length checks

  noise <- estimate_noise(int)

  peak_indicies <- pracma::findpeaks(int, threshold = noise)

  peak_limits <- data.frame(matrix(nrow = nrow(peak_indicies), ncol = 3))
  names(peak_limits) <- c("left", "apex", "right")
  peak_limits[,"left"] <- peak_indicies[,3]
  peak_limits[,"apex"] <- peak_indicies[,2]
  peak_limits[,"right"] <- peak_indicies[,4]


  peak_info <- get_peak_info(rt = rt, int = int, peak_limits)


  return(peak_info)

}










