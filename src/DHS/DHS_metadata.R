
#https://github.com/ck37/varimpact
#remotes::install_github("ck37/varimpact")
library(varimpact)
source(paste0(here::here(),"/src/0-functions.R"))


df <- readRDS(here("data/DHS/dhs_Ghana_2022.RDS"))
df <-df$Ghana_2022

#-------------------------------------------------------------------------------
# Create metadata
#-------------------------------------------------------------------------------

# Household Data - Household Recode (HR)
# This dataset has one record for each household. It includes household member's roster but no information from the individual women/men questionnaires is present in this file. The unit of analysis (case) in this file is the household.
#
# Household Listing Data - Household Member Recode (PR)
# This dataset has one record for every household member. It includes variables like sex, age, education, orphanhood, height and weight measurement, hemoglobin, etc. It also includes the characteristics of the households where the individual lives or was visiting. The unit of analysis (case) in this file is the household member.
#
# Individual Women's Data - Individual Recode (IR)
# This dataset has one record for every eligible woman as defined by the household schedule. It contains all the data collected in the women's questionnaire plus some variables from the household. Up to 20 births in the birth history, and up to 6 children under age 5, for whom pregnancy and postnatal care as well as immunization and health data were collected, can be found in this file. The fertility and mortality programs distributed by DHS use this file for data input. The unit of analysis (case) in this file is the woman.
#
# Men's Data - Male Recode (MR)
# This dataset has one record for every eligible man as defined by the household schedule. It contains all the data collected in the men's questionnaire plus some variables from the household. The unit of analysis (case) in this file is the man.
#
# Couple's Data - Couple's Recode (CR)
# This dataset has one record for every couple. It contains data for married or living together men and woman who both declared to be married (living together) to each other and with completed individual interviews (questionnaires). Essentially the file is the result of linking the two files previously described based on whom they both declared as partners. The unit of analysis (case) in this file is the couple in which both partners were interviewed.
#
# Children's Data - Children's Recode (KR)
# This dataset has one record for every child of interviewed women, born in the five years preceding the survey. It contains the information related to the child's pregnancy and postnatal care and immunization and health. The data for the mother of each of these children is included. This file is used to look at child health indicators such as immunization coverage, vitamin A supplementation, and recent occurrences of diarrhea, fever, and cough for young children and treatment of childhood diseases. The unit of analysis (case) in this file is the children of women born in the last 5 years (0-59 months).
#
# Births' data - Birth's Recode (BR)
# This dataset has one record for every child ever born to interviewed women. Essentially, it is the full birth history of all women interviewed including its information on pregnancy and postnatal care as well as immunization and health for children born in the last 5 years. Data for the mother of each of these children is also included. This file can be used to calculate health indicators as well as fertility and mortality rates. The unit of analysis (case) in this file is the children ever born of eligible women.


names(df)


MRdata_mens_recode <- makeVlist(df$MRdata)
IRdata_womens_recode <- makeVlist(df$IRdata)
HRdata_household_recode <- makeVlist(df$HRdata)
BRdata_births_recode <- makeVlist(df$BRdata)
CRdata_couples_recode <- makeVlist(df$CRdata)
KRdata_childrens_recode <- makeVlist(df$KRdata)
PRdata_household_member_recode <- makeVlist(df$PRdata)


write.csv(MRdata_mens_recode, here("metadata/DHS/MRdata_mens_recode.csv"))
write.csv(IRdata_womens_recode, here("metadata/DHS/IRdata_womens_recode.csv"))
write.csv(HRdata_household_recode, here("metadata/DHS/HRdata_household_recode.csv"))
write.csv(BRdata_births_recode, here("metadata/DHS/BRdata_births_recode.csv"))
write.csv(CRdata_couples_recode, here("metadata/DHS/CRdata_couples_recode.csv"))
write.csv(KRdata_childrens_recode, here("metadata/DHS/KRdata_childrens_recode.csv"))
write.csv(PRdata_household_member_recode, here("metadata/DHS/PRdata_household_member_recode.csv"))



