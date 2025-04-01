
library(dplyr)
library(haven)


df <- readRDS(here("data/DHS/dhs_Ghana_2022.RDS"))
df <-df$Ghana_2022



#---------------------------------------------------------------------------
#2. Read and Clean Household Recode (HR)
#---------------------------------------------------------------------------



HR <- df$HRdata %>%
  # We'll select some key household-level columns:
  select(
    hv001,              # cluster ID
    hv002,              # household ID
    hv024,              # region (de facto)
    hv025,              # type of place (urban/rural)
    hv270,              # wealth index
    # possibly more household/community variables you want
  ) %>%
  # rename them more simply
  rename(
    cluster = hv001,
    hh      = hv002,
    region  = hv024,
    residence_type = hv025,     # 1=urban, 2=rural in many DHS
    wealth_index   = hv270
  ) %>%
  # Each row in HR is unique to a household already, so no group_by needed.
  distinct()

#---------------------------------------------------------------------------
#3. Read and Clean Person Recode (PR)
#---------------------------------------------------------------------------

#The Person Recode has one record per household member. We’ll illustrate how to unify the anemia data for children, women, and men. In DHS-7:
#Children: hc53 (raw hemoglobin) or hc57 (anemia category)
#Adult women: ha53 (Hb) or ha57 (anemia category)
#Adult men: hb53 (Hb) or hb57 (anemia category)


PR <- df$PRdata %>%
  # Keep only members who have (or could have) hemoglobin data
  # We'll rename cluster/household/line for clarity
  select(
    hv001, hv002, hvidx, # IDs for merging and identifying the individual
    hv104,               # sex: 1=male, 2=female
    hv105,               # age in years
    hc57, ha57, hb57     # child, adult woman, adult man anemia category
    # If you need the raw hemoglobin (hc53, ha53, hb53), include them
  ) %>%
  rename(
    cluster = hv001,
    hh      = hv002,
    line    = hvidx
  ) %>%
  # Create a member-type variable
  mutate(
    member_type = case_when(
      hv105 < 15 ~ "child",
      hv104 == 2 ~ "adult_female",   # 15+ female
      hv104 == 1 ~ "adult_male",     # 15+ male
      TRUE       ~ NA_character_
    )
  ) %>%
  # Next, unify the anemia category from the correct variable
  # We'll store it in a single column called "anemia_cat" (1=Mild,2=Mod,3=Sev,0=Not)
  # Note: these categories are numeric codes in DHS.
  mutate(
    anemia_cat = case_when(
      member_type == "child"       ~ hc57,
      member_type == "adult_female"~ ha57,
      member_type == "adult_male"  ~ hb57,
      TRUE                         ~ NA_real_
    )
  ) %>%
  # Keep only rows where we actually have a valid (non-missing) anemia category
  filter(!is.na(anemia_cat) & anemia_cat != 9)  # '9' often means missing / not measured


#---------------------------------------------------------------------------
# 4. Code a Binary “Mod/Severe Anemia” Outcome
#---------------------------------------------------------------------------

# Labels:
#   value      label
# 1     severe
# 2   moderate
# 3       mild
# 4 not anemic
PR$ha57
PR$hb57
PR$hc57

PR <- PR %>%
  mutate(
    modsev_anemia = case_when(
      anemia_cat %in% c(1,2) ~ 1,
      anemia_cat %in% c(3,4) ~ 0,
      TRUE ~ NA_real_
    )
  )

#---------------------------------------------------------------------------
# 5. Merge Household Recode Info Into PR
#---------------------------------------------------------------------------

#Now each person’s record can gain the household-level variables from HR. We do a left_join on (cluster, hh):


PR_merged <- PR %>%
  left_join(HR, by = c("cluster", "hh"))
#Each row in PR_merged represents one person in the household.
#Now it includes region, residence_type, wealth_index, etc.
#from the household file.

#---------------------------------------------------------------------------
#6. Inspect or Subset as Needed
#---------------------------------------------------------------------------

#Now PR_merged is your dataset with:
# A row for each household member who had an anemia category measured (child/woman/man).
#Household-level attributes from HR.
head(PR_merged)


#if you only want children under 5, you’d filter by hv105 < 5. Or if you only want adult women 15–49, you’d filter by member_type == "adult_female" & hv105 <= 49. Etc.



# Example: Keep only those with a valid mod/severe anemia classification
analysis_data <- PR_merged %>%
  filter(!is.na(modsev_anemia))

Household (HR) file: 1 row per household → get household/community context (region, wealth, sanitation, etc.).

Person (PR) file: 1 row per household member → get each person’s age, sex, and (if measured) hemoglobin/anemia.

Merge on (hv001, hv002) to attach HR attributes to each PR row.

Unify child/woman/man anemia categories into a single column if you want to model all members.

Create a binary “moderate/severe anemia” outcome.

With this combined dataset, you can proceed to do your analyses or, later on, merge in more variables from the Women’s Recode (IR) if you want deeper info for the subset of adult women.
