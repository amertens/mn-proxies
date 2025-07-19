
# ── 0.  Packages ──────────────────────────────────────────────────────────
rm(list = ls())
library(haven)
library(dplyr)
library(janitor)
library(survey)
library(fastDummies)
library(purrr)
library(stringr)
library(here)
library(haven)
library(caret)
source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

options(survey.lonely.psu = "adjust")



d <-read_dta(here("data/MICS/Ghana 2017/Ghana_cleaned.dta")) %>% clean_names()

#drop near zero variance columns
d <- d [,-caret::nzv(d)]

meta <- makeVlist(d)
write.csv(meta,
          file = here("metadata/MICS/misc_metadata.csv"),
          row.names = FALSE,
          na = "")



# ── 2 · Define thematic keywords ──────────────────────────────────────────
themes <- list(
  nutrition        = c("breast", "diet", "meal", "weight",
                       "height", "stunt", "wast", "underweight",
                       "vitamin", "iron", "folic"),
  wash             = c("water", "sanitation", "toilet", "latrine",
                       "hygiene", "handwash", "soap","coli","cook","cooking","vessel","storage",
                       "handwashing","detergent","energy"),
  food_security    = c("food security", "hunger", "fies", "fcs"),
  socio_economic   = c("education", "literacy", "wealth", "quintile",
                       "income", "electricity", "asset", "floor",
                       "roof", "fuel", "household", "number","percentile group",
                       "material","member","internet","goats","sheep","chickens","wscore"),
  health_services  = c("age","malaria", "fever", "diarrhoea", "diarrhea",
                       "ari", "anc", "antenatal", "delivery",
                       "postnatal", "vaccin", "immuni","iodization", "mosquitos")
)

# Build ONE regex:  \b(keyword1|keyword2|…)\b
key_regex <- paste0("\\b(", paste(unlist(themes), collapse = "|"), ")\\b")

# ── 3 · Pick ≤ 100 candidate variables from metadata ─────────────────────
vars_kept <- meta %>%
  mutate(label = tolower(label)) %>%         # normalise once
  filter(str_detect(label, key_regex), !(name %in% c("hhname","hhaddr")))

vars_dropped <- meta %>%
  mutate(label = tolower(label)) %>%         # normalise once
  filter(!str_detect(label, key_regex))

vars_kept <- vars_kept %>%   # keep only relevant labels
  #slice_head(n = 100) %>%                    # hard cap of 100
  pull(name)

# OPTIONAL: preview the short‑list
print(vars_kept)

# ── 4 · Keep only those variables (plus region & weight) in the micro‑data ─
needed <- c("region", grep("weight$", names(d), value = TRUE), vars_kept, "treat_any", "san_shared","animals","floor")
d_trim <- d %>% select(any_of(needed)) %>% clean_names()

# identify the weight column we just kept
wt_var <- grep("weight$", names(d_trim), value = TRUE)[1]

# ── 5 · Labelled integers → factors so they become categoricals ───────────
d_trim <- d_trim %>%
  mutate(
    across(
      where(~ is.labelled(.x) && !is.numeric(.x)),  # *categorical* labelled
      as_factor
    ),
    across(                                         # strip labels from numeric vars
      where(~ is.labelled(.x) &&  is.numeric(.x)),
      ~ haven::zap_labels(.x, preserve_attributes = FALSE)
    )
  )

# ── 6 · Survey design object ──────────────────────────────────────────────
d_trim$hhweight <- as.numeric(d_trim$hhweight)
des <- svydesign(ids = ~1,
                 weights = as.formula(paste0("~", wt_var)),
                 data    = d_trim)

# ── 7 · Numeric summaries (weighted means) ───────────────────────────────

head(d_trim)

num_vars <- d_trim %>%
  select(-region, -all_of(wt_var)) %>%
  select(where(is.numeric)) %>%
  names()

num_tbl <- svyby(
  as.formula(paste("~", paste(num_vars, collapse = "+"))),
  ~region, des, svymean, na.rm = TRUE, vartype = NULL
)

head(num_tbl)
# ── 8 · Categorical summaries (weighted proportions) ─────────────────────
cat_vars <- d_trim %>% select(-region) %>%
  select(-where(is.numeric)) %>%
   names()


  cat_dummy <- d_trim %>%
    select( all_of(wt_var), all_of(cat_vars)) %>%
    fastDummies::dummy_cols(remove_first_dummy = FALSE,
                            remove_selected_columns = TRUE) %>%
    clean_names()
  colnames(cat_dummy)

  cat_dummy <- data.frame(region=d_trim$region, cat_dummy)
  head(cat_dummy)

  des_cat <- svydesign(ids = ~1,
                       weights = as.formula(paste0("~", wt_var)),
                       data    = cat_dummy)

  rhs <- setdiff(colnames(cat_dummy), c("region", wt_var))
  cat_tbl <- svyby(
    as.formula(paste("~", paste(rhs, collapse = "+"))),
    ~region, des_cat, svymean, na.rm = TRUE, vartype = NULL
  )

  region_summary <- left_join(num_tbl, cat_tbl, by = "region")


# ── 9 · Output ────────────────────────────────────────────────────────────
print(dim(region_summary))



# ── 1 · Clean column names (safer for formulas) ─────────────────────────────
d <- d %>% clean_names()

# ── 2 · Identify the weight variable automatically ─────────────────────────
wt_var <- names(d)[str_detect(names(d), "weight$")]
# if (length(wt_var) != 1)
#   stop("Could not uniquely identify a weight column. Please set wt_var manually.")
wt_var <- "hhweight"

# ── 3 · Ensure categorical variables are true factors ──────────────────────
d <- d %>% mutate(across(where(is.labelled), as_factor))

# ── 4 · Build a survey‑design object ───────────────────────────────────────
des <- svydesign(
  ids     = ~1,                 # already PSUs collapsed in your ‘d’
  weights = as.formula(paste0("~", wt_var)),
  data    = d
)

# ── 5 · NUMERIC variables: survey‑weighted means by region ─────────────────
num_vars <- d %>%
  select(-region, -all_of(wt_var)) %>%
  select(where(is.numeric)) %>%
  names()

num_tbl <- svyby(
  as.formula(paste("~", paste(num_vars, collapse = "+"))),
  ~region, des, svymean, na.rm = TRUE, vartype = NULL
)

# ── 6 · CATEGORICAL variables: survey‑weighted proportions by region ──────
cat_vars <- d %>%
  select(-where(is.numeric)) %>%
  select(-region) %>%
  names()

if (length(cat_vars) > 0) {
  cat_dummy <- d %>%
    select(region, all_of(wt_var), all_of(cat_vars)) %>%
    fastDummies::dummy_cols(select_columns = c(all_of(wt_var), all_of(cat_vars)),
                            remove_first_dummy = FALSE,
                            remove_selected_columns = TRUE) %>%
    clean_names()

  des_cat <- svydesign(
    ids     = ~1,
    weights = as.formula(paste0("~", wt_var)),
    data    = cat_dummy
  )

  rhs <- setdiff(colnames(cat_dummy), c("region", wt_var))
  cat_tbl <- svyby(
    as.formula(paste("~", paste(rhs, collapse = "+"))),
    ~region, des_cat, svymean, na.rm = TRUE, vartype = NULL
  )

  # ── 7 · Merge numeric + categorical summaries ────────────────────────────
  region_summary <- left_join(num_tbl, cat_tbl, by = "region")

} else {
  region_summary <- num_tbl
}

# ── 8 · Inspect / save ─────────────────────────────────────────────────────
print(dim(region_summary))  # rows = 10 regions; columns = 1 + all summaries
# write_csv(region_summary, "outputs/mics2017_region_summary.csv")
