test_that ("read_credentials", {
               cred <- read_credentials ("../test_creds.csv")
               testthat::expect_type (cred, "list")
})
