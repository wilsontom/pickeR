#' Plot the chromatogram.
#'
#' If a \code{peak_info} \code{data.frame} is supplied, then the apex's of all detected peaks are indicated
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @param peak_info an optional  \code{data.frame} of peak info (see \code{\link{get_peak_info}}) (default is\code{NULL})
#' @return a \code{ggplot} object
#'
#' @export
#' @importFrom ggplot2 geom_point labs
#' @importFrom graphics plot

plot_chrom <- function(rt, int, peak_info)
{
  peak_frame <- data.frame(rt, int)

  chrom_plot <-
    ggplot(data = peak_frame, aes(x = rt, y = int)) + geom_line(size = 0.45) +
    theme_classic() + labs(x = 'Rt (mins)', y = 'Intensity', title = '') +
    theme(
      axis.text.y = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(size = 10, face = "bold")
    )


  if (!is.null(peak_info)) {
    chrom_plot <-
      chrom_plot + geom_point(data = peak_info, aes(x = rt, y = int), colour = "red")

  }
  return(chrom_plot)
}
