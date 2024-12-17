# extracting derived data - formula

    Code
      important:::extract_data_derived(cls_f_fit, data = ad_data_small, type = "geno")
    Condition
      Error in `important:::extract_data_derived()`:
      ! `type` must be one of "predictors" or "outcomes", not "geno".

---

    Code
      important:::extract_data_derived(cls_f_fit, data = ad_data_small[, 1])
    Condition
      Error in `hardhat::forge()`:
      ! The required columns "tau", "p_tau", "VEGF", "MMP10", "Genotype", and "male" are missing.

# extracting original data - formula

    Code
      important:::extract_data_original(cls_f_fit, data = ad_data_small[, 1])
    Condition
      Error in `data[, col_names]`:
      ! Can't subset columns that don't exist.
      x Columns `tau`, `p_tau`, `VEGF`, `MMP10`, `Genotype`, etc. don't exist.

