#' Plot chromatogram with peak area
#'
#' Plot the chromatogram with the peak area of all detected peaks highlighted
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @param peak_info a \code{data.frame} of peak info (see \code{\link{get_peaks}})
#' @return a \code{ggplot} object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @importFrom ggplot2 ggplot aes aes_string geom_line theme_classic xlab ylab theme element_text geom_polygon guides

plot_area <- function(rt, int, peak_info)
{
  peak_frame <- data.frame(rt, int)

  chrom_plot <-
    ggplot(data = peak_frame, aes(x = rt, y = int)) + geom_line(size = 0.45) +
    theme_classic() + xlab("Rt (mins)") + ylab("Intensity") +
    theme(
      axis.text.y = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(size = 10, face = "bold")
    )

  # peak ind check etc....

  poly_ind <- NULL
  for (i in seq_along(1:nrow(peak_info))) {
    poly_ind[[i]] <-
      data.frame(rt = rt[peak_info[i, "left"]:peak_info[i, "right"]],
                 int = int[peak_info[i, "left"]:peak_info[i, "right"]],
                 cls = rep(i))
  }

  poly_df <- do.call("rbind", poly_ind)

  poly_df[, "cls"] <- factor(poly_df[, "cls"])

  area_plot <-
    chrom_plot + geom_polygon(data = poly_df, aes_string(x = 'rt', y = 'int', fill = 'cls')) +
    geom_line(size = 0.5) + guides(fill = "none")

  return(area_plot)
}
