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






