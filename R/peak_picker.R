#' Peak Picker
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @param target_rt a numeric value of a target retention time for targeted peak picking. Retention time must be in the format
#' M.SS (ie, 5.48)
#' @param rt_window a numeric value (in seconds) for retention time window. Any peaks detected within the specified \code{rt_window} of the
#' \code{target_rt} and combined into a single chromatographic feature. This is mainly of use for when poor quality peaks exhibit splitting,
#' or intense shoulder peaks
#' @return a \code{peak_info} \code{data.frame}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @examples
#'     \dontrun{
#'
#' load(system.file("extdata/chrom_b.rda", package = "pickeR"))
#' peaks_b <- pick_peaks(chrom_b$rt, chrom_b$int, target_rt = NULL)
#' print(peaks_b)
#'
#'   peak_id   rt rt_left rt_right        int        area peak_width left apex right
#' 1       1 4.82    4.64     5.00 1816325.62 162465.7170       21.4  234  243   252
#' 2       2 5.10    5.08     5.12   46423.14    800.2521        2.4  256  257   258
#' 3       3 5.24    5.16     5.34  608038.31  36007.8424       10.7  260  264   269
#' 4       4 5.43    5.39     5.45   37284.08    624.4537        3.6  272  274   275
#' 5       5 5.61    5.55     5.63   26859.48    722.4089        4.7  280  283   284
#' 6       6 5.65    5.63     5.69   20272.62    577.2687        3.6  284  285   287
#' 7       7 7.02    7.00     7.08   18515.96    640.6789        4.7  353  354   357
#' 8       8 7.32    7.22     7.36   16779.31    895.9999        8.3  364  369   371
#'
#' peaks_b <- peak_picker(chrom_b$rt, chrom_b$int, target_rt = 4.9, rt_window = 20)
#' print(peaks_b)
#'
#'   peak_id   rt rt_left rt_right     int     area peak_width left apex right
#' 1       1 4.82    4.64     5.12 1816326 162374.6       21.4  234  243   258
#'
#' }


peak_picker <- function(rt, int, target_rt, rt_window)
{
  all_peaks <- get_peaks(rt = rt, int = int)

  if (is.null(target_rt)) {
    return(all_peaks)
  }

  if (!is.null(target_rt)) {
    if (!is.numeric(target_rt)) {
      stop(deparse(substitute(target_rt, " must be numeric")), call. = FALSE)
    }
  }

  rt_range <-
    round(c(target_rt - (rt_window / 60), target_rt + (rt_window / 60)), digits = 2)

  idx <-
    which(all_peaks[, "rt"] > rt_range[1] &
            all_peaks[, "rt"] < rt_range[2])

  if (length(idx) >= 2) {
    new_info <-
      merge_peaks(
        rt = rt,
        int = int,
        idx = idx,
        peak_info = all_peaks
      )
  } else{
    new_info <- all_peaks
  }

  target_idx <-
    which(new_info[, "rt"] > rt_range[1] &
            new_info[, "rt"] < rt_range[2])

  peak_info <- new_info[target_idx,]

  return(peak_info)

}
