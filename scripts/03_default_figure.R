# Question 4: R's default figure, untouched: literate against age, geom_point() only.
library(readr)
library(dplyr)
library(ggplot2)

person <- read_csv("data/pslm_person_subset.csv")
hh <- read_csv("data/pslm_hh_subset.csv")

person <- person %>%
  left_join(hh %>% select(hhid, prov_code, urban_rural, hhweight), by = "hhid")

# Literacy sample for variant 0: rural Punjab, ages 10-64, literate not missing
literacy_sample <- person %>%
  filter(prov_code == 2, urban_rural == 1,
         age >= 10, age <= 64, !is.na(literate))

p <- ggplot(literacy_sample, aes(x = age, y = literate)) +
  geom_point()

p

ggsave("figures/default.svg", plot = p, width = 8, height = 6)
