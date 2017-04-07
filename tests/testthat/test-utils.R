test_that ("readCredentials", {
               cred <- readCredentials ("../testCreds.csv")
               testthat::expect_type (cred, "list")
})
