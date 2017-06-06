#' Merge peaks
#'
#' Merge multipe detected peaks toegther into a single chromatographic feature
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity#' @param idx
#' @param idx a numeric vector of peak id's to merge.
#' @param peak_info a \code{data.frame} of peak info (see \code{\link{pick_peaks}})
#' @return an updated \code{peak_info} \code{data.frame}
#'
#' @keywords internal
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}

merge_peaks <- function(rt,int, idx, peak_info)
  {

  # find which is the apex max of idx

  midx <- match(idx, peak_info[,"peak_id"])

  apex_peak <- midx[which(peak_info[midx,"int"] == max(peak_info[midx,"int"]))]

  merge_peak <-  midx[-which(peak_info[midx,"int"] == max(peak_info[midx,"int"]))]

  peak_info[apex_peak, "rt_left"] <- min(peak_info[midx, "rt_left"])
  peak_info[apex_peak, "left"] <- min(peak_info[midx, "left"])

  peak_info[apex_peak, "rt_right"] <- max(peak_info[midx, "rt_right"])
  peak_info[apex_peak, "right"] <- max(peak_info[midx, "right"])

  peak_info <- peak_info[-merge_peak,]

  apex_peak_ind <- match(apex_peak, peak_info[,"peak_id"])
  peak_info[apex_peak_ind,"area"] <- inegrate_peak(rt,int,c(peak_info[apex_peak_ind,"left"]:peak_info[apex_peak_ind, "right"]))

  return(peak_info)
  }
