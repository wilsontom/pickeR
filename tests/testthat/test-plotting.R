context('peak_picker')

test_that('package', {


  rt <- chrom_181[[2]]$rt
  int <- chrom_181[[2]]$int

  expect_error(chrom_plot(rt,int, c(1:nrow(chrom_a))))
  expect_true(is.data.frame(get_peaks(rt,int)))
  peak_info <- get_peaks(rt,int)

  expect_true(!is.null(plot_chrom(rt,int, peak_info)))

  expect_true(!is.null(plot_area(rt,int,peak_info)))
  expect_error(plot_area(rt,int))

  expect_true(is.numeric(get_chrom_max(rt,int)))
  expect_true(length(get_chrom_max(rt,int)) == 1)

  expect_true(is.data.frame(peak_picker(rt,int, target_rt = 4.19, rt_window = 25)))
  expect_true(nrow(peak_picker(rt,int, target_rt = 4.19, rt_window = 25)) == 1)

  expect_true(is.data.frame(peak_picker(rt,int, target_rt = get_chrom_max(rt,int), rt_window = 25)))
  expect_true(nrow(peak_picker(rt,int, target_rt = get_chrom_max(rt,int), rt_window = 25)) == 1)

  }
)
