% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_picker.R
\name{peak_picker}
\alias{peak_picker}
\title{Peak Picker}
\usage{
peak_picker(rt, int, target_rt, rt_window)
}
\arguments{
\item{rt}{a numeric vector of retention time}

\item{int}{a numeric vector of intensity}

\item{target_rt}{a numeric value of a target retention time for targeted peak picking. Retention time must be in the format
M.SS (ie, 5.48)}

\item{rt_window}{a numeric value (in seconds) for retention time window. Any peaks detected within the specified \code{rt_window} of the
\code{target_rt} and combined into a single chromatographic feature. This is mainly of use for when poor quality peaks exhibit splitting,
or intense shoulder peaks}
}
\value{
a \code{peak_info} \code{data.frame}
}
\description{
Peak Picker
}
\examples{
    \dontrun{

load(system.file("extdata/chrom_b.rda", package = "pickeR"))
peaks_b <- pick_peaks(chrom_b$rt, chrom_b$int, target_rt = NULL)
print(peaks_b)

  peak_id   rt rt_left rt_right        int        area peak_width left apex right
1       1 4.82    4.64     5.00 1816325.62 162465.7170       21.4  234  243   252
2       2 5.10    5.08     5.12   46423.14    800.2521        2.4  256  257   258
3       3 5.24    5.16     5.34  608038.31  36007.8424       10.7  260  264   269
4       4 5.43    5.39     5.45   37284.08    624.4537        3.6  272  274   275
5       5 5.61    5.55     5.63   26859.48    722.4089        4.7  280  283   284
6       6 5.65    5.63     5.69   20272.62    577.2687        3.6  284  285   287
7       7 7.02    7.00     7.08   18515.96    640.6789        4.7  353  354   357
8       8 7.32    7.22     7.36   16779.31    895.9999        8.3  364  369   371

peaks_b <- peak_picker(chrom_b$rt, chrom_b$int, target_rt = 4.9, rt_window = 20)
print(peaks_b)

  peak_id   rt rt_left rt_right     int     area peak_width left apex right
1       1 4.82    4.64     5.12 1816326 162374.6       21.4  234  243   258

}
}
\author{
Tom Wilson \email{tpw2@aber.ac.uk}
}
