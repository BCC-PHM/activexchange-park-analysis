library(dplyr)
library(janitor)
library(ggplot2)

data <- read.csv(
  "data/ActiveXchange - Birmingham Council Non-Ranger Parks.csv"
) %>%
  clean_names() %>%
  mutate(
    type = "Non-Ranger Park"
  ) %>%
  rbind(
    read.csv(
      "data/ActiveXchange - Birmingham Council Ranger Parks.csv"
    ) %>%
      clean_names() %>%
      mutate(
        type = "Ranger Park"
      )
  ) %>%
  mutate(
    # Fix September
    date_iso_year_week = gsub("Sept", "Sep", date_iso_year_week),
    date_string = stringr::str_extract(date_iso_year_week, "\\d+\\s\\w+\\s\\d{4}"),
    week_commencing = as.Date(date_string, format = "%d %b %Y")
  ) %>%
  group_by(type) %>%
  arrange(week_commencing, .by_group = TRUE) %>%
  mutate(
    visit_change = visit_count / first(visit_count)
  ) %>%
  ungroup()

plt <- ggplot(
  data,
  aes(
    x = week_commencing,
    y = visit_change,
    color = type
    )
  ) +
  geom_line(lwd = 1.2) +
  theme_bw() +
  labs(
    y = "Relative Change in Visits Since April 2024",
    x = "",
    color = ""
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(t = 0, b = -2)
  ) +
  scale_x_date(
    date_breaks = "4 month",
    date_labels = "%b %Y"
    ) +
  scale_color_manual(
    values = c("#84329B", "#DC582A")
  ) +
  scale_y_continuous(
    limits = c(0, 1.8),
    expand = c(0,0),
    labels = scales::label_percent()
  )
plt

ggsave("output/park-visit-comparison.png", width = 5, height = 3.5,
       plot = plt)