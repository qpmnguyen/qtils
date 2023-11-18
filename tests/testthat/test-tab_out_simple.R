data("rand_adlb")




test_that("basic_test_lm_ancova", {
    proc_df <- rand_adlb |> 
        dplyr::rename_all(tolower) |> 
        dplyr::filter(paramcd == "ALT") |> 
        dplyr::filter(avisit == "WEEK 5 DAY 36")
    testthat::expect_no_error(obj <- qtab_lm_anova(df = proc_df, var = "aval", arm = "trt01a", 
                                                           covariates = NULL))
})
