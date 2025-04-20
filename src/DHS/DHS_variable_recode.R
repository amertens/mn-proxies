

# new_name => list_of_patterns
hr_rename_spec <- list(

  # IDs
  cluster         = c("^hv001$", "cluster"),
  hh              = c("^hv002$", "household.*number"),
  svy_weight      = c("^hv005$", "household sample weight (6 decimals)"),

  # Geographic and basic
  region          = c("^hv024$", "region"),
  residence_type  = c("^hv025$", "urban|rural|type of place"),
  # If you want to capture subregion or other admin units, add more patterns

  # Household composition
  hh_size         = c("^hv009$", "number of household members"),
  num_under5      = c("^hv014$", "number of children under 5"),
  # You might also want "hv010" = # of eligible women, or "hv012" de jure members, etc.

  # Wealth / assets
  wealth_quint    = c("^hv270$", "wealth index"),
  wealth_score    = c("^hv271$", "wealth score"),
  has_electricity = c("^hv206$", "electricity"),
  has_radio       = c("^hv207$", "radio"),
  has_tv          = c("^hv208$", "television"),
  has_fridge      = c("^hv209$", "refrigerator"),
  has_bicycle     = c("^hv210$", "bicycle"),
  has_motorcycle  = c("^hv211$", "motorcycle|scooter"),
  has_car         = c("^hv212$", "car|truck"),

  # Housing materials / environment
  floor_material  = c("^hv213$", "floor material"),
  wall_material   = c("^hv214$", "wall material"),
  roof_material   = c("^hv215$", "roof material"),

  # Water / sanitation / cooking
  water_source    = c("^hv201$", "source.*drinking water"),
  toilet_type     = c("^hv205$", "toilet"),
  share_toilet    = c("^hv225$", "share.*toilet"),
  cooking_fuel    = c("^hv226$", "fuel"),
  # Additional environment indicators: hv215 (roof?), hv232 (handwashing?), etc.

  # Misc environment / malaria
  has_mosquito_net= c("^hml1$", "mosquito net.*number"),
  # or bednet usage might be in PR (person-level), but some summary here as well.

  # Possibly altitude if you want it for anemia adjustments:
  altitude        = c("^hv040$", "altitude"),

  # Additional variables that might matter for local contexts
  # e.g. hv243a mobile phone, hv244 land ownership, hv246 livestock, etc.
  has_mobile      = c("^hv243a$", "mobile phone"),
  has_computer    = c("^hv243e$", "computer")
  # Etc.

  # If you want “sprayed for mosquitoes,” hv253, hv253a, etc.

)





ir_rename_spec <- list(

  # The standard ID keys:
  cluster    = c("^v001$", "cluster"),
  hh         = c("^v002$", "household.*number"),
  line       = c("^v003$", "respondent.*line"),

  # Basic Demographics
  age_single = c("^v012$", "respondent.*current age"),
  age_5yrgrp = c("^v013$", "age.*5-year"),
  edu_level  = c("^v106$", "highest level of education"),
  edu_years  = c("^v107$", "highest year of education"),
  religion   = c("^v130$", "religion"),
  ethnicity  = c("^v131$", "ethnicity"),
  region     = c("^v024$", "region"),
  residence  = c("^v025$", "urban|rural|type of place"),
  hh_size    = c("^v136$", "number of household members"),
  marital_st = c("^v501$", "marital status"),

  # Socio-economic
  wealth_quint = c("^v190$", "wealth index"),
  wealth_score = c("^v191$", "wealth score"),
  partner_edu  = c("^v701$", "partner.*education"),
  partner_age  = c("^v702$", "partner.*age"),
  partner_occ  = c("^v704$", "partner.*occupation|husband.*occupation"),
  respond_work = c("^v717$", "respondent.*work"),

  # Reproductive
  pregnant_now   = c("^v213$", "currently pregnant"),
  months_preg    = c("^v214$", "duration of current pregnancy"),
  time_sincempl  = c("^v215$", "time since last menstrual period"),
  total_living_ch= c("^v218$", "total children living"),
  living_ch_plus = c("^v219$", "children living.*plus current preg"),
  months_last_bir= c("^v222$", "interval since last birth"),
  age_first_union= c("^v511$", "age at first union"),
  age_first_sex  = c("^v531$", "age at first sexual intercourse"),
  future_fert_pref= c("^v602$", "desire.*child"),
  wanted_more_ch = c("^v605$"),
  ideal_num_ch   = c("^v613$", "ideal number of children"),

  # Nutritional / Anthropometric
  bmi_times100   = c("^v445$", "bmi", "body mass index"),

  # Anemia (IR-based)
  hb_level       = c("^v453$", "hemoglobin", "hb level"),
  anemia_cat     = c("^v457$", "anemia level"),

  # Breastfeeding
  bf_current     = c("^v404$", "currently.*breastfeeding"),
  # For deeper postpartum variables you might add: m34_1, m5_1, etc. if you want last-birth detail

  # Domestic Violence (DV) Module
  dv_selected  = c("^v044$", "domestic violence module"),
  dv_ever_phys = c("^d105$", "pushed|slapped|kicked|physically hurt", "ever physically abused"),
  dv_injury    = c("^d106$", "physical.*injury|hurt.*badly"),

  # HIV-related
  hiv_ever_test   = c("^v784$", "ever tested.*hiv"),
  hiv_time_test   = c("^v785$", "time since last.*hiv test"),
  hiv_place_test  = c("^v826$", "knows place.*hiv test")
  # Possibly others like v781, v782 (knowledge about HIV, place to test)
  # add more patterns if needed
)

pr_rename_spec <- list(

  # IDs
  cluster     = c("^hv001$", "cluster"),
  hh          = c("^hv002$", "household.*number"),
  line        = c("^hvidx$", "line.*number"),

  # Demographics
  sex         = c("^hv104$", "sex", "male.*female"),
  age         = c("^hv105$", "age in years"),
  # Relationship to head: hv101 if you want it
  rel_to_head = c("^hv101$", "relationship.*head"),

  # Education
  edu_level   = c("^hv106$", "highest level of education"),
  edu_years   = c("^hv107$", "years of education"),

  # Household membership
  usual_resident= c("^hv102$", "usual.*resident"),  # 1 if usual, 0 if visitor
  slept_last_night= c("^hv103$", "slept in hh last night"),

  # Biomarkers for anemia:
  # children's anemia category => typically "hc57"
  child_anemia_cat  = c("^hc57$", "anemia level.*child"),
  # women's anemia => "ha57"
  woman_anemia_cat  = c("^ha57$", "anemia level.*woman"),
  # men’s anemia => "hb57"
  man_anemia_cat    = c("^hb57$", "anemia level.*man"),

  # If you want raw hemoglobin for further analysis:
  child_hb      = c("^hc53$", "hemoglobin.*child"),
  woman_hb      = c("^ha53$", "hemoglobin.*woman"),
  man_hb        = c("^hb53$", "hemoglobin.*man"),

  # Alternatively, you might unify them yourself.

  # Possibly bednet or malaria variables
  used_bednet   = c("^hml12$", "slept under net"),
  # or is it "hml10"? Check your metadata. There's also "hml20" for insecticide-treated net, etc.

  # If you want to track mother’s line if child <15
  mother_line   = c("^hv112$", "line number of mother"),
  father_line   = c("^hv114$", "line number of father")

  # Additional variables that might matter:
  # e.g. hv115 marital status (for older members?), hv116 now married?
  # or any child immunization data that appear in PR. Usually better in KR, but you can
  # add them if you want a single wide file.

)
