
library(readr)
library(dplyr)
library(ggplot2)

person <- read_csv("data/pslm_person_subset.csv")
hh <- read_csv("data/pslm_hh_subset.csv")

person <- person %>%
  left_join(
    hh %>% select(hhid, prov_code, urban_rural, hhweight),
    by = "hhid"
  )

person <- person %>%
  filter(
    prov_code == 2,
    urban_rural == 1,
    age >= 10,
    age <= 64,
    !is.na(literate)
  ) %>%
  mutate(
    sex_label = ifelse(sex == 1, "Men", "Women"),
    age_band = cut(
      age,
      breaks = seq(10, 65, by = 5),
      right = FALSE,
      labels = seq(10, 60, by = 5)
    )
  ) %>%
  filter(!is.na(age_band))


literacy <- person %>%
  group_by(age_band, sex_label) %>%
  summarise(
    n = n(),
    literacy_rate = weighted.mean(literate, hhweight, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    age_mid = as.numeric(as.character(age_band)) + 2,
    small_sample = n < 30
  )

gaps <- literacy %>%
  select(age_band, sex_label, literacy_rate) %>%
  tidyr::pivot_wider(names_from = sex_label, values_from = literacy_rate) %>%
  mutate(gap = abs(Men - Women))

largest_gap <- gaps %>%
  slice_max(gap, n = 1, with_ties = FALSE)



end_labels <- literacy %>%
  group_by(sex_label) %>%
  filter(age_mid == max(age_mid)) %>%
  ungroup()

gap_x <- as.numeric(as.character(largest_gap$age_band)) + 2

gap_men <- largest_gap$Men
gap_women <- largest_gap$Women
gap_y <- (gap_men + gap_women) / 2

p <- ggplot(
  literacy,
  aes(x = age_mid, y = literacy_rate, color = sex_label, group = sex_label)
) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = small_sample), size = 2.5) +
  geom_segment(
    aes(x = gap_x, xend = gap_x, y = gap_men, yend = gap_women),
    inherit.aes = FALSE,
    color = "grey35",
    linetype = "dashed"
  ) +
  geom_text(
    data = end_labels,
    aes(label = sex_label),
    hjust = -0.2,
    show.legend = FALSE
  ) +
  
  annotate(
    "text",
    x = gap_x,
    y = min(95, gap_men + 18),
    label = paste0(
      "Largest gap: ",
      largest_gap$age_band, "–",
      as.numeric(as.character(largest_gap$age_band)) + 4,
      " (", round(largest_gap$gap, 1), " pp)"
    ),
    hjust = 0.5,
    color = "grey25",
    size = 3
  ) +
  
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
  scale_color_manual(values = c("Men" = "grey45", "Women" = "#287A55")) +
  
  scale_x_continuous(
    breaks = seq(12, 62, by = 5),
    labels = paste(seq(10, 60, by = 5), seq(14, 64, by = 5), sep = "–"),
    guide = guide_axis(n.dodge = 2)
  ) +
  
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "The literacy gap between men and women widens with age in rural Punjab",
    subtitle = "Rural Punjab, ages 10–64",
    x = "Age band",
    y = "Weighted literacy rate (%)",
    caption = "Source: PSLM 2019-20 person and household data. Sample: rural Punjab residents aged 10-64 with reported literacy status.\nWeighted by household weight (hhweight). Hollow markers indicate fewer than 30 unweighted observations."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot",
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  )

p

ggsave(
  "figures/04_redesigned.svg",
  plot = p,
  width = 10,
  height = 6
)
