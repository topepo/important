test_that("forging predictors - formula", {
  co2_ptype <-
    structure(
      list(`(Intercept)` = numeric(0), Plant.L = numeric(0),
           Plant.Q = numeric(0), Plant.C = numeric(0), `Plant^4` = numeric(0),
           `Plant^5` = numeric(0), `Plant^6` = numeric(0), `Plant^7` = numeric(0),
           `Plant^8` = numeric(0), `Plant^9` = numeric(0), `Plant^10` = numeric(0),
           `Plant^11` = numeric(0), TypeMississippi = numeric(0),
           Treatmentchilled = numeric(0), conc = numeric(0)),
      row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"))

  predictors <- important:::forge_predictors(CO2, reg_f_fit)
  expect_equal(predictors[0,], co2_ptype)
  expect_equal(nrow(predictors), nrow(CO2))

  ###

  co2_1d_ptype <-
    structure(
      list(`(Intercept)` = numeric(0), conc = numeric(0)),
      row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"))

  predictors <- important:::forge_predictors(CO2, reg_1d_fit)
  expect_equal(predictors[0,], co2_1d_ptype)
  expect_equal(nrow(predictors), nrow(CO2))
})

test_that("forging predictors - recipe", {
  co2_ptype <-
    structure(
      list(
        Plant = structure(integer(0),
                          levels = c("Qn1", "Qn2", "Qn3", "Qc1", "Qc3", "Qc2",
                                     "Mn3", "Mn2", "Mn1", "Mc2", "Mc3", "Mc1"),
                          class = c("ordered", "factor")),
        conc = numeric(0),
        Type_Mississippi = numeric(0), Treatment_chilled = numeric(0)),
      row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"))

  predictors <- important:::forge_predictors(CO2, reg_r_fit)
  expect_equal(predictors[0,], co2_ptype)
  expect_equal(nrow(predictors), nrow(CO2))
})

test_that("forging predictors - selectors", {
  co2_ptype <-
    structure(
      list(
        Plant = structure(integer(0),
                          levels = c("Qn1", "Qn2", "Qn3", "Qc1", "Qc3", "Qc2",
                                     "Mn3", "Mn2", "Mn1", "Mc2", "Mc3", "Mc1"),
                          class = c("ordered", "factor")),
        Type = structure(integer(0), levels = c("Quebec", "Mississippi"),
                         class = "factor"),
        Treatment = structure(integer(0), levels = c("nonchilled","chilled"),
                              class = "factor"),
        conc = numeric(0)),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  predictors <- important:::forge_predictors(CO2, reg_v_fit)
  expect_equal(predictors[0,], co2_ptype)
  expect_equal(nrow(predictors), nrow(CO2))
})

# ------------------------------------------------------------------------------

test_that("extracting derived data - formula", {
  skip_if_not_installed("modeldata")
  ad_ptype <-
    tibble::tibble(
    `(Intercept)` = numeric(0),
    tau = numeric(0),
    GenotypeE2E3 = numeric(0),
    GenotypeE2E4 = numeric(0),
    GenotypeE3E3 = numeric(0),
    GenotypeE3E4 = numeric(0),
    GenotypeE4E4 = numeric(0),
    male = numeric(0),
    Class = structure(integer(0), levels = c("Impaired", "Control"), class = "factor")
  )

  dat <- important:::extract_data_derived(cls_f_fit, data = ad_data_small)
  expect_equal(dat[0,], ad_ptype)
  expect_equal(nrow(dat), nrow(ad_data_small))

  ###

  dat <- important:::extract_data_derived(cls_f_fit, data = ad_data_small, type = "predictors")
  expect_equal(dat[0,], ad_ptype %>% dplyr::select(-Class))
  expect_equal(nrow(dat), nrow(ad_data_small))

  ###

  dat <- important:::extract_data_derived(cls_f_fit, data = ad_data_small, type = "outcomes")
  expect_equal(dat[0,], ad_ptype %>% dplyr::select(Class))
  expect_equal(nrow(dat), nrow(ad_data_small))

  ###

  expect_snapshot(
    important:::extract_data_derived(cls_f_fit, data = ad_data_small,
                                     type = "geno"),
    error = TRUE)

  expect_snapshot(
    important:::extract_data_derived(cls_f_fit, data = ad_data_small[,1]),
    error = TRUE)

})

test_that("extracting derived data - recipe", {
  skip_if_not_installed("modeldata")
  ad_ptype <-
    tibble::tibble(
      tau = numeric(0),
      male = numeric(0),
      Genotype_E2E3 = numeric(0),
      Genotype_E2E4 = numeric(0),
      Genotype_E3E3 = numeric(0),
      Genotype_E3E4 = numeric(0),
      Genotype_E4E4 = numeric(0),
      Class = structure(integer(0), levels = c("Impaired", "Control"), class = "factor")
    )

  dat <- important:::extract_data_derived(cls_r_fit, data = ad_data_small)
  expect_equal(dat[0,], ad_ptype)
  expect_equal(nrow(dat), nrow(ad_data_small))

  ###

  dat <- important:::extract_data_derived(cls_r_fit, data = ad_data_small, type = "predictors")
  expect_equal(dat[0,], ad_ptype %>% dplyr::select(-Class))
  expect_equal(nrow(dat), nrow(ad_data_small))

  ###

  dat <- important:::extract_data_derived(cls_r_fit, data = ad_data_small, type = "outcomes")
  expect_equal(dat[0,], ad_ptype %>% dplyr::select(Class))
  expect_equal(nrow(dat), nrow(ad_data_small))
})

test_that("extracting derived data - selectors", {
  skip_if_not_installed("modeldata")
  ad_ptype <-
    tibble::tibble(
      tau = numeric(0),
      Genotype = structure(integer(0),
                           levels = c("E2E2", "E2E3", "E2E4",
                                      "E3E3", "E3E4", "E4E4"),
                           class = "factor"),
      male = numeric(0),
      Class = structure(integer(0), levels = c("Impaired", "Control"), class = "factor")
    )

  dat <- important:::extract_data_derived(cls_v_fit, data = ad_data_small)
  expect_equal(dat[0,], ad_ptype)
  expect_equal(nrow(dat), nrow(ad_data_small))

  ###

  dat <- important:::extract_data_derived(cls_v_fit, data = ad_data_small, type = "predictors")
  expect_equal(dat[0,], ad_ptype %>% dplyr::select(-Class))
  expect_equal(nrow(dat), nrow(ad_data_small))

  ###

  dat <- important:::extract_data_derived(cls_v_fit, data = ad_data_small, type = "outcomes")
  expect_equal(dat[0,], ad_ptype %>% dplyr::select(Class))
  expect_equal(nrow(dat), nrow(ad_data_small))
})

# ------------------------------------------------------------------------------

test_that("extracting original data - formula", {
  skip_if_not_installed("modeldata")

  dat <- important:::extract_data_original(cls_f_fit, data = ad_data_small)
  expect_equal(dat, ad_data_small %>% dplyr::select(tau, Genotype, male, Class))

  ### TODO this is an issue

  reg_trans_fit <-
    workflow(sqrt(uptake) ~ ., linear_reg()) %>%
    fit(data = CO2)

  dat <- important:::extract_data_original(reg_trans_fit, data = CO2)
  # names are c("Plant", "Type", "Treatment", "conc", "uptake")
  # should we get uptake or sqrt(uptake)?

  ###

  expect_snapshot(
    important:::extract_data_original(cls_f_fit, data = ad_data_small[,1]),
    error = TRUE)
})

test_that("extracting original data - recipe", {
  skip_if_not_installed("modeldata")

  dat <- important:::extract_data_original(cls_r_fit, data = ad_data_small)
  expect_equal(dat, ad_data_small %>% dplyr::select(tau, Genotype, male, Class))
})

test_that("extracting original data - selectors", {
  skip_if_not_installed("modeldata")

  dat <- important:::extract_data_original(cls_v_fit, data = ad_data_small)
  expect_equal(dat, ad_data_small %>% dplyr::select(tau, Genotype, male, Class))
})


