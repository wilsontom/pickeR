context('pickeR-internals')

test_that('internals', {

  rt <- chrom_181[[2]]$rt
  int <- chrom_181[[2]]$int

  expect_true(is.numeric(pickeR:::estimate_noise(int)))

  expect_true(is.vector(pickeR:::local_max(int)))
  expect_true(is.vector(pickeR:::local_min(int)))

  peak_info <- get_peaks(rt,int)
  peak_merged <- pickeR:::merge_peaks(rt,int, idx = c(1:5), peak_info)

  expect_true(is.data.frame(peak_merged))
  expect_true(nrow(peak_merged) < nrow(peak_info))
  expect_error(pickeR:::merge_peaks(rt,int, idx = c(15:20), peak_info))

  }
)



