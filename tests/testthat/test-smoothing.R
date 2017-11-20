context('pickeR-smoothing')

test_that('smoothing', {

rt <- chrom_181[[2]]$rt
int <- chrom_181[[2]]$int

expect_true(is.numeric(smooth_peak(rt,int, ford = 2)))
expect_true(is.vector(smooth_peak(rt,int, ford = 2)))

intsm <- pickeR::smooth_peak(rt, int, ford = 2)

expect_true(all(int != intsm))

expect_true(length(int) == length(intsm))
})
