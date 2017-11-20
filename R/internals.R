#' Local Minima
#'
#' Find all local minima's in a time series vector
#'
#' @param x a numeric vector
#' @return a vector of local minima indicies
#'
#' @keywords internal

local_min <- function(x)
{
  y <- diff(c(.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}


#' Local Maxima
#'
#' Find all local maxima's in a time series vector
#'
#' @param x a numeric vector
#' @return a vector of local maxima indicies
#'
#' @keywords internal

local_max <- function(x)
{
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}


#' Estimate Noise
#'
#' Make an approximate estimation of baseline noise by taking the mean intensity of all local minima's after removing regions
#' where a chromatographic peak may be present (local maxima's)
#'
#' @param x a numeric vector of intensity
#' @return a numeric value
#'
#' @keywords internal

estimate_noise <- function(x)
{

  lmax <- local_max(x)
  lmin <- local_min(x[-lmax])

  #return(as.numeric(median(x[lmin])))
  return(as.numeric(mean(x[lmin])))
}

#' Integrate Peak
#'
#'
#' @keywords internal




inegrate_peak <- function(rt,int,range)
{

  # stick some checking in here

  peak_area <- pracma::polyarea(int[range],rt[range])

  return(as.numeric(peak_area))
}

#' Merge peaks
#'
#' Merge multipe detected peaks toegther into a single chromatographic feature
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity#' @param idx
#' @param idx a numeric vector of peak id's to merge.
#' @param peak_info a \code{data.frame} of peak info (see \code{\link{get_peak_info}})
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


#' Get peak information
#'
#' Create a \code{peak_info} \code{data.frame}
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @param peak_limits a \code{data.frame} indicating the left, right and apex indices for all detected peaks
#' @return a \code{peak_info} \code{data.frame}
#'
#' @keywords internal
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}

get_peak_info <- function(rt, int, peak_limits)
{
  # checks for rt int len and peak_limits return

  peak_info <-
    data.frame(matrix(nrow = nrow(peak_limits), ncol = 7))
  names(peak_info) <-
    c("peak_id",
      "rt",
      "rt_left",
      "rt_right",
      "int",
      "area",
      "peak_width")

  for (i in seq_along(1:nrow(peak_limits))) {
    peak_info[i, "peak_id"] <- i
    peak_info[i, "rt"] <- rt[peak_limits[i, "apex"]]
    peak_info[i, "int"] <- int[peak_limits[i, "apex"]]
    peak_info[i, "peak_width"] <-
      (rt[peak_limits[i, "right"]] * 60) - (rt[peak_limits[i, "left"]] * 60)

    peak_info[i, "rt_left"] <- rt[peak_limits[i, "left"]]
    peak_info[i, "rt_right"] <- rt[peak_limits[i, "right"]]

    peak_info[i, "area"] <-
      inegrate_peak(rt, int, c(peak_limits[i, "left"]:peak_limits[i, "right"]))
  }

  peak_info[, "rt"] <- round(peak_info[, "rt"], digits = 2)
  peak_info[, "rt_left"] <- round(peak_info[, "rt_left"], digits = 2)
  peak_info[, "rt_right"] <-
    round(peak_info[, "rt_right"], digits = 2)
  peak_info[, "peak_width"] <-
    round(peak_info[, "peak_width"], digits = 1)

  peak_info <-
    data.frame(peak_info,
               left = peak_limits[, "left"],
               apex = peak_limits[, "apex"],
               right = peak_limits[, "right"])
  return(peak_info)

}



