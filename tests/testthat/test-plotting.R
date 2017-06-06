context("pickeR-plotting")

test_that("plots", {

  load(system.file("extdata/chrom_a.rda", package = "pickeR"))

  expect_true(is.null(chrom_plot(chrom_a[,"rt"], chrom_a[,"int"])))
  expect_error(chrom_plot(chrom_a[,"rt"], chrom_a[,"int"], c(1:nrow(chrom_a))))

  peak_info <- pickeR:::peak_detection(chrom_a[,"rt"], chrom_a[,"int"])

  expect_false(is.null(chrom_plot(chrom_a[,"rt"], chrom_a[,"int"], peak_info = peak_info)))
  expect_true(is.list(chrom_plot(chrom_a[,"rt"], chrom_a[,"int"], peak_info = peak_info)))

  expect_false(is.null(area_plot(chrom_a[,"rt"], chrom_a[,"int"], peak_info = peak_info)))
  expect_error(area_plot(chrom_a[,"rt"], chrom_a[,"int"]))

  }
)
