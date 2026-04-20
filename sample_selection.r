# =========================================================
# SURV 745 PracTools Project
# sample_selection.R
# =========================================================

# -----------------------------
# 0. Packages
# -----------------------------
req_pkgs <- c("readxl", "dplyr", "stringr", "tidyr", "sampling", "purrr", "tibble")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(sampling)
library(purrr)
library(tibble)

set.seed(-77)

# -----------------------------
# 1. User settings
# -----------------------------
group_name <- "CV"
data_excel <- "Detroit MI.xlsx"
fips_file  <- "Census Tract FIPS Code Detroit MI.csv"
n_psu <- 40

# Response-rate targets
resp_targets <- tibble(
  age_group = c("18_34", "35_49", "50_64", "65p"),
  respondent_n = c(250, 250, 250, 250),
  rr = c(0.35, 0.45, 0.50, 0.65)
) %>%
  mutate(selected_n_required = respondent_n / rr)

# -----------------------------
# 2. Helper functions
# -----------------------------
make_age_domains <- function(df) {
  df %>%
    mutate(
      age18_34 =
        Male18to19Yrs + Male20Yrs + Male21Yrs + Male22to24Yrs + Male25to29Yrs + Male30to34Yrs +
        Female18to19Yrs + Female20Yrs + Female21Yrs + Female22to24Yrs + Female25to29Yrs + Female30to34Yrs,
      age35_49 =
        Male35to39Yrs + Male40to44Yrs + Male45to49Yrs +
        Female35to39Yrs + Female40to44Yrs + Female45to49Yrs,
      age50_64 =
        Male50to54Yrs + Male55to59Yrs + Male60to61Yrs + Male62to64Yrs +
        Female50to54Yrs + Female55to59Yrs + Female60to61Yrs + Female62to64Yrs,
      age65p =
        Male65to66Yrs + Male67to69Yrs + Male70to74Yrs + Male75to79Yrs + Male80to84Yrs + MaleGE85Yrs +
        Female65to66Yrs + Female67to69Yrs + Female70to74Yrs + Female75to79Yrs + Female80to84Yrs + FemaleGE85Yrs,
      age18p = age18_34 + age35_49 + age50_64 + age65p
    )
}

select_one_bg_within_tract <- function(df_tract_bg) {
  if (nrow(df_tract_bg) == 1) {
    df_tract_bg %>%
      mutate(
        pik2 = 1,
        s2 = 1
      )
  } else {
    pik2 <- inclusionprobabilities(df_tract_bg$MOS, 1)
    s2 <- UPsystematic(pik2)

    df_tract_bg %>%
      mutate(
        pik2 = pik2,
        s2 = as.numeric(s2)
      )
  }
}

allocate_remainder <- function(df, count_var, float_var, pop_var, target_total) {
  current_total <- sum(df[[count_var]], na.rm = TRUE)
  remainder <- round(target_total - current_total)

  if (remainder > 0) {
    df <- df %>%
      mutate(
        frac_part = .data[[float_var]] - floor(.data[[float_var]]),
        room_left = .data[[pop_var]] - .data[[count_var]]
      ) %>%
      arrange(desc(frac_part), desc(room_left))

    idx <- which(df$room_left > 0)

    if (length(idx) > 0) {
      add_n <- min(remainder, length(idx))
      df[[count_var]][idx[1:add_n]] <- df[[count_var]][idx[1:add_n]] + 1
    }

    df <- df %>%
      arrange(Tract, BlockGroup) %>%
      select(-frac_part, -room_left)
  }

  df
}

cv_fun <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}

# -----------------------------
# 3. Read raw data
# -----------------------------
tract_raw <- read_excel(data_excel, sheet = "Tract")
bg_raw    <- read_excel(data_excel, sheet = "BlockGroup")
fips_raw  <- read.csv(fips_file, stringsAsFactors = FALSE)

tract_raw <- tract_raw %>%
  mutate(
    Tract = str_pad(as.character(Tract), width = 11, side = "left", pad = "0")
  )

bg_raw <- bg_raw %>%
  mutate(
    BlockGroup = str_pad(as.character(BlockGroup), width = 12, side = "left", pad = "0"),
    Tract = substr(BlockGroup, 1, 11),
    BG_in_tract = substr(BlockGroup, 12, 12)
  )

fips_raw <- fips_raw %>%
  mutate(
    GEOID = str_pad(as.character(GEOID), width = 11, side = "left", pad = "0")
  )

# -----------------------------
# 4. Build tract and block-group frames
# -----------------------------
tract <- make_age_domains(tract_raw)
bg    <- make_age_domains(bg_raw)

bg_counts <- bg %>%
  group_by(Tract) %>%
  summarise(
    n_bg = n(),
    tract_totperson_from_bg = sum(TotPerson, na.rm = TRUE),
    tract_age18p_from_bg = sum(age18p, na.rm = TRUE),
    .groups = "drop"
  )

tract <- tract %>%
  left_join(bg_counts, by = "Tract") %>%
  mutate(
    n_bg = ifelse(is.na(n_bg), 0, n_bg),
    tract_totperson_from_bg = ifelse(is.na(tract_totperson_from_bg), 0, tract_totperson_from_bg),
    tract_age18p_from_bg = ifelse(is.na(tract_age18p_from_bg), 0, tract_age18p_from_bg),
    zero_pop_flag = TotPerson <= 0 | age18p <= 0,
    lt2_bg_flag = n_bg < 2,
    small_age18p_flag = age18p < 30
  )

tract_frame <- tract %>%
  filter(!zero_pop_flag, n_bg >= 1)

bg_frame <- bg %>%
  semi_join(tract_frame, by = "Tract") %>%
  filter(age18p > 0, TotPerson > 0)

# -----------------------------
# 5. Population summaries and equal-rate check
# -----------------------------
pop_totals <- tract_frame %>%
  summarise(
    N18_34 = sum(age18_34, na.rm = TRUE),
    N35_49 = sum(age35_49, na.rm = TRUE),
    N50_64 = sum(age50_64, na.rm = TRUE),
    N65p   = sum(age65p, na.rm = TRUE)
  )

same_rate_check <- tibble(
  age_group = c("18_34", "35_49", "50_64", "65p"),
  N_h = c(pop_totals$N18_34, pop_totals$N35_49, pop_totals$N50_64, pop_totals$N65p),
  respondent_n = c(250, 250, 250, 250),
  rr = c(0.35, 0.45, 0.50, 0.65)
) %>%
  mutate(
    selected_n_required = respondent_n / rr,
    implied_sampling_rate = selected_n_required / N_h
  )

# -----------------------------
# 6. Measures of size
# -----------------------------
tract_frame <- tract_frame %>%
  mutate(
    MOS_adult = age18p,
    MOS_rr_adj = age18_34 / 0.35 + age35_49 / 0.45 + age50_64 / 0.50 + age65p / 0.65,
    MOS = MOS_rr_adj
  ) %>%
  arrange(Tract)

bg_frame <- bg_frame %>%
  mutate(
    MOS_adult = age18p,
    MOS_rr_adj = age18_34 / 0.35 + age35_49 / 0.45 + age50_64 / 0.50 + age65p / 0.65,
    MOS = MOS_rr_adj
  ) %>%
  arrange(Tract, BlockGroup)

# -----------------------------
# 7. PSU selection
# -----------------------------
pik_psu <- inclusionprobabilities(tract_frame$MOS, n_psu)
s_psu   <- UPsystematic(pik_psu)

tract_sample <- tract_frame %>%
  mutate(
    pik1 = pik_psu,
    s1 = as.numeric(s_psu),
    psu_weight = 1 / pik1
  ) %>%
  filter(s1 == 1)

# -----------------------------
# 8. SSU selection
# -----------------------------
bg_sample_full <- bg_frame %>%
  semi_join(tract_sample, by = "Tract") %>%
  group_by(Tract) %>%
  group_modify(~ select_one_bg_within_tract(.x)) %>%
  ungroup()

bg_sample <- bg_sample_full %>%
  filter(s2 == 1) %>%
  mutate(
    ssu_weight = 1 / pik2
  )

# -----------------------------
# 9. Person allocation by age domain
# -----------------------------
bg_sample <- bg_sample %>%
  mutate(
    n18_34_float = (250 / 0.35) * age18_34 / sum(age18_34),
    n35_49_float = (250 / 0.45) * age35_49 / sum(age35_49),
    n50_64_float = (250 / 0.50) * age50_64 / sum(age50_64),
    n65p_float   = (250 / 0.65) * age65p   / sum(age65p)
  ) %>%
  mutate(
    n18_34 = pmin(age18_34, floor(n18_34_float)),
    n35_49 = pmin(age35_49, floor(n35_49_float)),
    n50_64 = pmin(age50_64, floor(n50_64_float)),
    n65p   = pmin(age65p,   floor(n65p_float))
  )

bg_sample <- allocate_remainder(bg_sample, "n18_34", "n18_34_float", "age18_34", 250 / 0.35)
bg_sample <- allocate_remainder(bg_sample, "n35_49", "n35_49_float", "age35_49", 250 / 0.45)
bg_sample <- allocate_remainder(bg_sample, "n50_64", "n50_64_float", "age50_64", 250 / 0.50)
bg_sample <- allocate_remainder(bg_sample, "n65p",   "n65p_float",   "age65p",   250 / 0.65)

bg_sample <- bg_sample %>%
  mutate(
    f18_34 = ifelse(age18_34 > 0, n18_34 / age18_34, 0),
    f35_49 = ifelse(age35_49 > 0, n35_49 / age35_49, 0),
    f50_64 = ifelse(age50_64 > 0, n50_64 / age50_64, 0),
    f65p   = ifelse(age65p   > 0, n65p   / age65p,   0)
  )

# -----------------------------
# 10. Final inclusion probabilities and weights
# -----------------------------
bg_sample <- bg_sample %>%
  left_join(
    tract_sample %>% select(Tract, pik1),
    by = "Tract"
  ) %>%
  mutate(
    pik_18_34 = pik1 * pik2 * f18_34,
    pik_35_49 = pik1 * pik2 * f35_49,
    pik_50_64 = pik1 * pik2 * f50_64,
    pik_65p   = pik1 * pik2 * f65p,
    w_18_34 = ifelse(pik_18_34 > 0, 1 / pik_18_34, NA_real_),
    w_35_49 = ifelse(pik_35_49 > 0, 1 / pik_35_49, NA_real_),
    w_50_64 = ifelse(pik_50_64 > 0, 1 / pik_50_64, NA_real_),
    w_65p   = ifelse(pik_65p   > 0, 1 / pik_65p,   NA_real_)
  )

cv_weights <- tibble(
  domain = c("18_34", "35_49", "50_64", "65p"),
  cv_weight = c(
    cv_fun(bg_sample$w_18_34),
    cv_fun(bg_sample$w_35_49),
    cv_fun(bg_sample$w_50_64),
    cv_fun(bg_sample$w_65p)
  )
)

# -----------------------------
# 11. Final frame and sample output files
# -----------------------------
frame_tract_out <- tract_frame %>%
  select(
    Tract, NAME, TotHH, TotPerson,
    age18_34, age35_49, age50_64, age65p, age18p,
    n_bg, zero_pop_flag, lt2_bg_flag, small_age18p_flag,
    MOS_adult, MOS_rr_adj, MOS
  )

frame_bg_out <- bg_frame %>%
  select(
    Tract, BlockGroup, NAME, TotHH, TotPerson,
    age18_34, age35_49, age50_64, age65p, age18p,
    MOS_adult, MOS_rr_adj, MOS
  )

sample_out <- bg_sample %>%
  select(
    Tract, BlockGroup, NAME, TotHH, TotPerson,
    age18_34, age35_49, age50_64, age65p, age18p,
    MOS, pik1, pik2,
    n18_34, n35_49, n50_64, n65p,
    f18_34, f35_49, f50_64, f65p,
    pik_18_34, pik_35_49, pik_50_64, pik_65p,
    w_18_34, w_35_49, w_50_64, w_65p
  )

# -----------------------------
# 12. Variable codebooks
# -----------------------------
frame_codebook <- tibble(
  variable = c(
    "Tract", "NAME", "TotHH", "TotPerson", "age18_34", "age35_49", "age50_64", "age65p",
    "age18p", "n_bg", "zero_pop_flag", "lt2_bg_flag", "small_age18p_flag",
    "MOS_adult", "MOS_rr_adj", "MOS", "BlockGroup"
  ),
  description = c(
    "Census tract identifier used as the PSU ID.",
    "Name or label of the census area.",
    "Total number of households in the area.",
    "Total number of persons in the area.",
    "Number of persons aged 18-34.",
    "Number of persons aged 35-49.",
    "Number of persons aged 50-64.",
    "Number of persons aged 65 and older.",
    "Total number of persons aged 18 and older.",
    "Number of block groups contained in the tract.",
    "Indicator for zero eligible adult population.",
    "Indicator for tracts with fewer than two block groups.",
    "Indicator for tracts with fewer than 30 adults.",
    "Measure of size based on adult population.",
    "Response-rate-adjusted measure of size.",
    "Final measure of size used for selection.",
    "Census block group identifier used as the SSU ID."
  )
)

sample_codebook <- tibble(
  variable = c(
    "Tract", "BlockGroup", "NAME", "TotHH", "TotPerson",
    "age18_34", "age35_49", "age50_64", "age65p", "age18p",
    "MOS", "pik1", "pik2",
    "n18_34", "n35_49", "n50_64", "n65p",
    "f18_34", "f35_49", "f50_64", "f65p",
    "pik_18_34", "pik_35_49", "pik_50_64", "pik_65p",
    "w_18_34", "w_35_49", "w_50_64", "w_65p"
  ),
  description = c(
    "Census tract identifier for the selected PSU.",
    "Census block group identifier for the selected SSU.",
    "Name or label of the selected block group.",
    "Total number of households in the selected block group.",
    "Total number of persons in the selected block group.",
    "Number of persons aged 18-34 in the selected block group.",
    "Number of persons aged 35-49 in the selected block group.",
    "Number of persons aged 50-64 in the selected block group.",
    "Number of persons aged 65 and older in the selected block group.",
    "Total number of adults aged 18 and older in the selected block group.",
    "Final measure of size used in selection.",
    "First-stage inclusion probability for the selected tract.",
    "Second-stage inclusion probability for the selected block group, conditional on tract selection.",
    "Allocated number of selected persons aged 18-34.",
    "Allocated number of selected persons aged 35-49.",
    "Allocated number of selected persons aged 50-64.",
    "Allocated number of selected persons aged 65 and older.",
    "Within-block-group sampling rate for persons aged 18-34.",
    "Within-block-group sampling rate for persons aged 35-49.",
    "Within-block-group sampling rate for persons aged 50-64.",
    "Within-block-group sampling rate for persons aged 65 and older.",
    "Overall inclusion probability for a person aged 18-34.",
    "Overall inclusion probability for a person aged 35-49.",
    "Overall inclusion probability for a person aged 50-64.",
    "Overall inclusion probability for a person aged 65 and older.",
    "Base weight for a person aged 18-34.",
    "Base weight for a person aged 35-49.",
    "Base weight for a person aged 50-64.",
    "Base weight for a person aged 65 and older."
  )
)

# -----------------------------
# 13. Export files
# -----------------------------
write.csv(frame_tract_out, paste0("frame_tract_out_", group_name, ".csv"), row.names = FALSE)
write.csv(frame_bg_out,    paste0("frame_bg_out_", group_name, ".csv"), row.names = FALSE)
write.csv(sample_out,      paste0("sample_out_", group_name, ".csv"), row.names = FALSE)

write.csv(frame_codebook,  paste0("frame_codebook_", group_name, ".csv"), row.names = FALSE)
write.csv(sample_codebook, paste0("sample_codebook_", group_name, ".csv"), row.names = FALSE)
