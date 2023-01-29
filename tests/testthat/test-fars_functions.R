test_that("filename is created as expected", {
    expect_equal(make_filename(2013), "accident_2013.csv.bz2")
    expect_equal(make_filename("2013"), "accident_2013.csv.bz2")
    expect_equal(make_filename(c(2013, 2014)), c("accident_2013.csv.bz2", "accident_2014.csv.bz2"))
    expect_warning(make_filename("teststring"), "NAs introduced by coercion")
    expect_error(make_filename(list("1" = c(1, 2))))
})
