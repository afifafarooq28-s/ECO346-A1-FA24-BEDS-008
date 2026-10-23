
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

# Keep rural Punjab
person <- person %>%
  filter(prov_code == 2, urban_rural == 1)

# Keep students with positive education spending
students <- person %>%
  filter(ed_total > 0)
dim(students)
summary(students$ed_total)
summary(students$hhweight)

# Calculate weighted quantiles
weighted_quantile <- function(x, w, p) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  x[which(cumsum(w) / sum(w) >= p)[1]]
}

# Calculate statistics
median_unweighted <- median(students$ed_total, na.rm = TRUE)
median_weighted <- weighted_quantile(students$ed_total, students$hhweight, 0.5)
p10_weighted <- weighted_quantile(students$ed_total, students$hhweight, 0.1)
p90_weighted <- weighted_quantile(students$ed_total, students$hhweight, 0.9)

# Count spending values outside the graph's range
excluded <- sum(students$ed_total < 100 | students$ed_total > 1000000)

# Display results
median_unweighted
median_weighted
p10_weighted
p90_weighted
p90_weighted / p10_weighted
excluded

# Keep spending between Rs 100 and Rs 1,000,000
plot_data <- students %>%
  filter(ed_total >= 100, ed_total <= 1000000) %>%
  mutate(log_spend = log10(ed_total))

# Create histogram
p <- ggplot(plot_data, aes(x = log_spend, weight = hhweight)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.1,
    boundary = 2,
    fill = "grey75",
    color = "white"
  ) +
  geom_vline(
    xintercept = log10(median_weighted),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = log10(p90_weighted),
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 3.82,
    y = 1.45,
    label = "Median: Rs 6,600",
    angle = 0,
    hjust = 0.5,
    size = 3
  ) +
  annotate(
    "text",
    x = 4.55,
    y = 1.25,
    label = "90th percentile: Rs 30,000",
    angle = 0,
    hjust = 0.5,
    size = 3
  ) +
  scale_x_continuous(
    breaks = 2:6,
    labels = c("100", "1,000", "10,000", "100,000", "1,000,000")
  ) +
  labs(
    title = "Most students spend far less on education than the highest spenders",
    subtitle = "Annual education spending among students in rural Punjab",
    x = "Annual education spending (Rs, log scale)",
    y = "Weighted density per log10 unit",
    caption = "Source: PSLM person and household data. Students with positive education spending; weighted by household sampling weights. Three observations outside the plotted range."
  ) +
  coord_cartesian(ylim = c(0, 1.7), clip = "off") +
  theme_minimal()

p

# Save figure
ggsave(
  "figures/02_distribution.svg",
  plot = p,
  width = 10,
  height = 7
)