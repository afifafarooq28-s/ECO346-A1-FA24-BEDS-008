# Check that both datasets load correctly

library(readr)

person <- read_csv("data/pslm_person_subset.csv")
hh <- read_csv("data/pslm_hh_subset.csv")

head(person)
head(hh)

dim(person)
dim(hh)
names(person)
names(hh)

library(dplyr)

person <- person %>%
  left_join(
    hh %>% select(hhid, prov_code, urban_rural, hhweight),
    by = "hhid"
  )

# Check the merged data
dim(person)
head(person[, c("hhid", "sex", "age", "prov_code",
                "urban_rural", "hhweight")])

# Keep only rural Punjab
punjab_rural <- person %>%
  filter(prov_code == 2, urban_rural == 1)

# Check the filtered data
dim(punjab_rural)

table(punjab_rural$sex, useNA = "ifany")
summary(punjab_rural$hhweight)

sum(is.na(punjab_rural$hhweight))