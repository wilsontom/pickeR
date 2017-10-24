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



#' Smooth Peak
#' @keywords internal
smooth_peak <- function(rt,int, ford)
{

  f <- seq(from = 3, to = 51, by = 2)

  filtered_int <- NULL
  for(i in seq_along(f)){
    filtered_int[[i]] <- pracma::savgol(int, f[i], forder = ford)
  }

  noise <- estimate_noise(int)

  min_int <- which(unlist(purrr::map(filtered_int, ~{abs(min(.)) > noise})) == FALSE)

  selected_f <- f[max(min_int)]

  smoothed_int <- pracma::savgol(int, fl = selected_f, forder = ford, dorder = 0)

  smoothed_int[smoothed_int < 0] <- 0

    return(smoothed_int)

}


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





