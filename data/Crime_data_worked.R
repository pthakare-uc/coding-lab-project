library(tidyverse)
library(lubridate)
crime_safety_2026 <- read_csv("data/Crimes_-_2026_20261016.csv")
#Dimensions
dim(crime_safety_2026)
#names of Column
names(crime_safety_2026)
#First six rows
head(crime_safety_2026)


library(tidyverse)

# Convert Date into a proper date-time variable

crime_safety_2026 <- crime_safety_2026 %>%
  mutate(
    Date = mdy_hms(Date),
    month = month(Date),
    day_of_week = wday(Date, label = TRUE),
    hour = hour(Date),
    date_only = as.Date(Date)
  )

crime_counts <- crime_safety_2026 %>%
  count(`Primary Type`, sort = TRUE)

# Display the 10 most common crime types
head(crime_counts, 10)


crime_safety_2026 %>%
  count(`Primary Type`, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(
    x = reorder(`Primary Type`, n),
    y = n
  )) +
  geom_col() +
  coord_flip() +
  labs(
    title = "10 Most Common Crime Types in Chicago",
    x = "Crime type",
    y = "Number of recorded incidents"
  ) +
  theme_minimal()


# Most common locations where incidents occur
crime_safety_2026 %>%
  count(`Location Description`, sort = TRUE) %>%
  slice_head(n = 10)

# Most common community areas
crime_safety_2026 %>%
  filter(!is.na(`Community Area`)) %>%
  count(`Community Area`, sort = TRUE) %>%
  slice_head(n = 10)

# Most common police districts
crime_safety_2026 %>%
  count(District, sort = TRUE) %>%
  slice_head(n = 10)


# Crime counts by day of the week
crime_safety_2026 %>%
  count(day_of_week) %>%
  ggplot(aes(x = day_of_week, y = n)) +
  geom_col() +
  labs(
    title = "Recorded Crime by Day of Week",
    x = "Day",
    y = "Number of incidents"
  ) +
  theme_minimal()

# Crime counts by hour
crime_safety_2026 %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_col() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Recorded Crime by Hour of Day",
    x = "Hour (0 = midnight)",
    y = "Number of incidents"
  ) +
  theme_minimal()


# Overall arrest proportion
crime_safety_2026 %>%
  summarise(
    total_incidents = n(),
    arrests_recorded = sum(Arrest, na.rm = TRUE),
    arrest_percentage = mean(Arrest, na.rm = TRUE) * 100
  )

# Arrest proportion by crime type
crime_safety_2026 %>%
  group_by(`Primary Type`) %>%
  summarise(
    total_incidents = n(),
    arrest_percentage = mean(Arrest, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(arrest_percentage))


# Save crime counts as a CSV file
write_csv(
  crime_counts,
  "crime_counts_by_type.csv"
)
