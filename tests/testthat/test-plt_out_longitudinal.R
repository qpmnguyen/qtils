data("rand_adlb")


test_that("basic_test_qplot_longitudinal", {
    df <- rand_adlb |> dplyr::filter(PARAMCD == "ALT") |> 
        dplyr::mutate(AVISIT = factor(AVISIT))
    
    testthat::expect_no_error(qplot_obs_long(df = df, ptype = "mean", var = AVAL, 
                   tvar = AVISIT, id = USUBJID, col_by = TRT01A))
    
    testthat::expect_no_error(qplot_obs_long(df = df, ptype = "mean", var = AVAL, 
                                             tvar = AVISIT, id = USUBJID, col_by = TRT01A, 
                                             lt_by = STRATA1))
    
    testthat::expect_no_error(qplot_obs_long(df = df, ptype = "mean", var = AVAL, 
                                             tvar = AVISIT, id = USUBJID))
    
    testthat::expect_error(qplot_obs_long(df = df, ptype = "mean", var = AVAL, 
                                          tvar = AVISIT, id = USUBJID, col_by = NULL, 
                                          lt_by = STRATA1))
})