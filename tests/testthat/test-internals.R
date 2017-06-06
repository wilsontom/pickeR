context("pickeR-internals")

test_that("internals", {

  load(system.file("extdata/chrom_a.rda", package = "pickeR"))

  expect_true(is.data.frame(pickeR:::peak_detection(chrom_a[,"rt"], chrom_a[,"int"])))

  expect_true(is.numeric(pickeR:::estimate_noise(chrom_a[,"int"])))

  expect_true(is.vector(pickeR:::local_max(chrom_a[,"int"])))
  expect_true(is.vector(pickeR:::local_min(chrom_a[,"int"])))

  peak_info <- pickeR:::peak_detection(chrom_a[,"rt"], chrom_a[,"int"])
  peak_merged <- pickeR:::merge_peaks(chrom_a[,"rt"], chrom_a[,"int"], idx = c(1:5), peak_info)

  expect_true(is.data.frame(peak_merged))
  expect_true(nrow(peak_merged) < nrow(peak_info))
  expect_error(pickeR:::merge_peaks(chrom_a[,"rt"], chrom_a[,"int"], idx = c(15:20), peak_info))

  }
)
