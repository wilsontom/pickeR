

# CHANGE name
get_peak_info <- function(rt,int, peak_limits)
{

  # checks for rt int len and peak_limits return

  peak_info <- data.frame(matrix(nrow = nrow(peak_limits), ncol = 7))
  names(peak_info) <- c("peak_id", "rt", "rt_left", "rt_right", "int", "area", "peak_width")

  for(i in seq_along(1:nrow(peak_limits))){
    peak_info[i, "peak_id"] <- i
    peak_info[i,"rt"] <- rt[peak_limits[i,"apex"]]
    peak_info[i,"int"] <- int[peak_limits[i,"apex"]]
    peak_info[i,"peak_width"] <- (rt[peak_limits[i,"right"]] * 60) - (rt[peak_limits[i,"left"]] * 60)

    peak_info[i,"rt_left"] <- rt[peak_limits[i,"left"]]
    peak_info[i,"rt_right"] <- rt[peak_limits[i,"right"]]

    peak_info[i, "area"] <- inegrate_peak(rt,int, c(peak_limits[i,"left"]:peak_limits[i,"right"]))


  }

  peak_info[,"rt"] <- round(peak_info[,"rt"], digits = 2)
  peak_info[,"rt_left"] <- round(peak_info[,"rt_left"], digits = 2)
  peak_info[,"rt_right"] <- round(peak_info[,"rt_right"], digits = 2)
  peak_info[,"peak_width"] <- round(peak_info[,"peak_width"], digits = 1)

  peak_info <- data.frame(peak_info, peak_limits)

  return(peak_info)

}
