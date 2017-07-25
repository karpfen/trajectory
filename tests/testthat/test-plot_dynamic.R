test_that ("test_popup", {
    pu <- popup ("x", c ("y"), c (0))
    testthat::expect_equal (pu, "<b>x</b></br><b>y: </b>0.00")
})
