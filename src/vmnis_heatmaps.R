
# Load required libraries
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(haven)
library(here)
library(patchwork)

d <- read_dta(here("data/VMNIS/VMNISIndicator_long_format.dta"))
head(d)

unique(d$Country)


#filter to LMIC's in Africa, Asia, and South America only

LMICs = c("Malawi", "Mozambique", "Zambia", "Zimbabwe", "Angola", "Benin",
          "Burkina Faso", "Burundi", "Cameroon", "Central African Republic",
          "Chad", "Comoros", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo",
          "Djibouti", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon",
          "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho",
          "Liberia", "Madagascar", "Mali", "Mauritania", "Mauritius", "Namibia",
          "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal",
          "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan",
          "Sudan", "Swaziland", "Tanzania", "Togo", "Uganda", "Afghanistan",
          "Bangladesh", "Bhutan", "Cambodia", "China", "Democratic People's Republic of Korea",
          "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Jordan",
          "Kiribati", "Kyrgyzstan", "Lao People's Democratic Republic",
          "Lebanon", "Malaysia", "Maldives", "Marshall Islands", "Micronesia (Federated States of)",
          "Mongolia", "Myanmar", "Nauru", "Nepal", "Pakistan", "Palau",
          "Papua New Guinea", "Philippines", "Samoa", "Solomon Islands",
          "Sri Lanka", "Tajikistan", "Thailand", "Timor-Leste", "Tonga",
          "Turkmenistan", "Tuvalu", "Uzbekistan", "Vanuatu", "Vietmam",
          "Yemen", "Albania", "Armenia", "Azerbaijan","Belize","Colombia",
          "Costa Rica","Ecuador", "Guatemala" , "Mexico", "Panama", "West Bank and Gaza Strip",
          "Viet Nam", "Peru", "United Republic of Tanzania","Bolivia (Plurinational State of)", "Dominican Republic",
          "Guyana", "Haiti", "Jamaica",  "Argentina", "Brazil", "Chile", "Paraguay",  "Morocco",
          "Nicaragua", "Egypt",  "Brazil", "El Salvador", "Tunisia",  "Botswana" , "Kazakhstan" , "Algeria","Paraguay",  "Venezuela (Bolivarian Republic of)",
          "Syrian Arab Republic" )


unique(d$Country)[!(unique(d$Country) %in% LMICs)]

unique(d$Population)

plotdf <- d %>% filter(
  Representativeness=="national",Areacovered=="both urban and rural",
  Country %in% LMICs) %>%
  mutate(Population=case_when(Population %in% c("Infants (1-11 months)","Children (12-23 months)","Neonates (0-28 days)",
                                                "Young infants (0-5 months)","Children (24-59 months)" ) ~"Children <5",
         Population==Population~Population))
head(plotdf)

#to do:
#collapse MN categories and populations, and drop LMIC countries

survey_summary <- plotdf %>%
  group_by(Indicator, Country, Population) %>%
  filter(Date == max(Date)) %>%
  summarize(n_surveys = n(),
            mean=mean(Mean,na.rm=T)) %>%
  ungroup() %>% droplevels()
head(survey_summary)

ggplot(survey_summary, aes(y = Indicator, x = Country, fill=mean)) +
  geom_point(aes(size = mean),
             shape = 21, color = "black") +
  # geom_text(aes(label = ifelse(n_surveys > 0, recent_year, "")),
  #           color = "grey20"

  , size = 3, fontface = "bold") +
  #scale_size_continuous(range = c(3, 12)) +
  scale_fill_gradient(low = "blue", high = "lightblue",
                      na.value = "grey", name = "Most Recent Survey") +
  theme_minimal() +
  facet_wrap(~Population)+
  labs(title = "Survey Count and Recency by Data Source and Country",
       x = "Data Source", y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



