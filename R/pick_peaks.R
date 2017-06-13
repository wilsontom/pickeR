#' Pick peaks
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

pick_peaks <- function(rt,int, target_rt, rt_window = 20)
  {
  all_peaks <- peak_detection(rt = rt,int = int)

  if(is.null(target_rt)){
    return(all_peaks)
  }

  if(!is.null(target_rt)){
    if(!is.numeric(target_rt)){stop(deparse(substitute(target_rt, " must be numeric")),call. = FALSE)}
  }

  rt_range <- round(c(target_rt - (rt_window / 60), target_rt + (rt_window / 60)), digits = 2)

  idx <- which(all_peaks[,"rt"] > rt_range[1] & all_peaks[,"rt"] < rt_range[2])

  if(length(idx) >= 2){
    new_info <- merge_peaks(rt = rt, int = int, idx = idx, peak_info = all_peaks)
  }else{
    new_info <- all_peaks
  }

  target_idx <- which(new_info[,"rt"] > rt_range[1] & new_info[,"rt"] < rt_range[2])

  peak_info <- new_info[target_idx,]

  return(peak_info)

}
