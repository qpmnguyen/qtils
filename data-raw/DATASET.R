## code to prepare `DATASET` dataset goes here
requireNamespace('random.cdisc.data')
rand_adsl <- random.cdisc.data::radsl(N = 400, seed = 2105, study_duration = 1)
rand_adlb <- random.cdisc.data::radlb(adsl = adsl, seed = 2105)

usethis::use_data(rand_adsl, overwrite = TRUE)
usethis::use_data(rand_adlb, overwrite = TRUE)
