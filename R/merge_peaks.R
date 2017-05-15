#' Merge peaks
#'
#'
#'

merge_peaks <- function(rt,int, idx, peak_info)
  {

  # find which is the apex max of idx

  midx <- match(idx, peak_info$peak_id)

  apex_peak <- midx[which(peak_info[midx,"int"] == max(peak_info[midx,"int"]))]

  merge_peak <-  midx[-which(peak_info[midx,"int"] == max(peak_info[midx,"int"]))]

  peak_info[apex_peak, "rt_left"] <- min(peak_info[midx, "rt_left"])
  peak_info[apex_peak, "left"] <- min(peak_info[midx, "left"])

  peak_info[apex_peak, "rt_right"] <- max(peak_info[midx, "rt_right"])
  peak_info[apex_peak, "right"] <- max(peak_info[midx, "right"])

  peak_info <- peak_info[-merge_peak,]
  peak_info[apex_peak,"area"] <- inegrate_peak(rt,int,c(peak_info[apex_peak,"left"]:peak_info[apex_peak, "right"]))


  return(peak_info)
  }
