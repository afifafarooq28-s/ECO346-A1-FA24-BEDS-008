
library(readr)
library(ggplot2)

person <- read_csv("data/pslm_person_subset.csv")
hh <- read_csv("data/pslm_hh_subset.csv")

person <- person %>%
  dplyr::left_join(
    hh %>% dplyr::select(hhid, prov_code, urban_rural, hhweight),
    by = "hhid"
  )

person <- person %>%
  dplyr::filter(prov_code == 2, urban_rural == 1)

students <- person %>%
  dplyr::filter(ed_total > 0)

p <- ggplot(students, aes(x = age, y = ed_total)) +
  geom_point()

p

ggsave("figures/default.svg", plot = p, width = 8, height = 6)
