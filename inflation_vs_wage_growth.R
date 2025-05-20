# Load libraries
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, etc.
library(lubridate)   # working with dates
library(maps)        # US map data

# 1) CPI + Inflation (2004–2014)
cpi_clean <- read_csv("~/Desktop/DATA332/r_projects/finals/data/us_cpi_inflation.csv") %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    CPI       = mean(Index, na.rm = TRUE),
    Inflation = mean(Inflation, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(Year >= 2004, Year <= 2014)

# Reference CPI for 2014 (for real-dollar conversions)
cpi_ref <- cpi_clean %>% 
  filter(Year == 2014) %>% 
  pull(CPI)

# 2) Cost of Living Index (COLI)
coli_clean <- read_csv("~/Desktop/DATA332/r_projects/finals/data/cost_of_living.csv") %>%
  select(abb = State, COLI = Conversion) %>%
  mutate(
    State = state.name[match(abb, state.abb)]
  ) %>%
  select(-abb)

# 3) Minimum Wage Data
min_wage_data <- read_csv("~/Desktop/DATA332/r_projects/finals/data/minimum_wage_data.csv")

# Extract 2014 snapshot
min_wage_2014 <- min_wage_data %>%
  filter(Year == 2014) %>%
  transmute(
    State,
    Minimum_Wage = Effective.Minimum.Wage
  )

# 4) 2014 Real Minimum Wage by State
wage_vs_coli <- min_wage_2014 %>%
  left_join(coli_clean, by = "State") %>%
  mutate(Real_Wage = Minimum_Wage / (COLI / 100))

# Bar chart
wage_vs_coli %>%
  drop_na(Real_Wage) %>%
  ggplot(aes(x = reorder(State, Real_Wage), y = Real_Wage)) +
  geom_col(aes(fill = Real_Wage)) +
scale_fill_viridis_c(option = "magma", name = "Real $") +
  coord_flip() +
  labs(
    title = "Real Minimum Wage by State, 2014 (Adjusted for COLI)",
    x     = NULL,
    y     = "Real Wage ($)"
  ) +
  theme_minimal()

# 5) Real Minimum Wage Time Series (2004–2014)
real_wage_ts <- min_wage_data %>%
  filter(Year >= 2004, Year <= 2014) %>%
  transmute(
    Year,
    State,
    Nominal_Min_Wage = Effective.Minimum.Wage
  ) %>%
  left_join(cpi_clean, by = "Year") %>%
  mutate(Real_Min_Wage = Nominal_Min_Wage * (cpi_ref / CPI))

# National average trend
avg_real_wage <- real_wage_ts %>%
  group_by(Year) %>%
  summarize(Avg_Real_Min_Wage = mean(Real_Min_Wage, na.rm = TRUE),
            .groups = "drop")

ggplot(avg_real_wage, aes(Year, Avg_Real_Min_Wage)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Average Real Minimum Wage, U.S. States (2004–2014, 2014 $)",
    x     = "Year",
    y     = "Real Minimum Wage ($, 2014)"
  ) +
  theme_minimal()

# 6) State-by-State Growth (2004 → 2014)
state_growth <- real_wage_ts %>%
  filter(Year %in% c(2004, 2014)) %>%
  select(State, Year, Real_Min_Wage) %>%
  pivot_wider(
    names_from   = Year,
    names_prefix = "Y",
    values_from  = Real_Min_Wage
  ) %>%
  drop_na() %>%
  mutate(
    Absolute_Change = Y2014 - Y2004,
    Percent_Change  = (Y2014 / Y2004 - 1) * 100
  )

top10    <- state_growth %>% slice_max(Percent_Change, n = 10)
bottom10 <- state_growth %>% slice_min(Percent_Change, n = 10)

bind_rows(
  top10    %>% mutate(Category = "Top 10 Growth"),
  bottom10 %>% mutate(Category = "Bottom 10 Growth")
) %>%
  ggplot(aes(reorder(State, Percent_Change), Percent_Change, fill = Category)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("Top 10 Growth" = "forestgreen",
               "Bottom 10 Growth" = "firebrick")
  ) +
  labs(
    title    = "Real Minimum Wage Growth by State (2004→2014)",
    subtitle = "Top & Bottom 10 by % Change",
    x        = NULL,
    y        = "Percent Change (%)",
    fill     = NULL
  ) +
  theme_minimal()

# 7) Choropleth: 2014 Real Wage by State
states_map <- map_data("state")

choropleth_df <- states_map %>%
  left_join(
    wage_vs_coli %>%
      drop_na(Real_Wage) %>%
      transmute(region = tolower(State), Real_Wage),
    by = "region"
  )

ggplot(choropleth_df, aes(long, lat, group = group, fill = Real_Wage)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "2014 Real Minimum Wage by State (Adjusted for COLI)",
    fill  = "Real Wage ($)"
  ) +
  theme_void()

# 8) Real Wage vs. COLI Scatter + r
scatter_df <- wage_vs_coli %>% drop_na(Real_Wage, COLI)
r_value    <- cor(scatter_df$Real_Wage, scatter_df$COLI)

cat("Pearson r =", round(r_value, 2), "\n")

ggplot(scatter_df, aes(COLI, Real_Wage)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title    = "2014 Real Minimum Wage vs. Cost-of-Living Index",
    subtitle = paste("Pearson r =", round(r_value, 2)),
    x        = "COLI (% of U.S. Avg)",
    y        = "Real Wage ($, 2014)"
  ) +
  theme_minimal()

# 9) Clustering: Real Wage & COLI (2014)
cluster_df <- wage_vs_coli %>%
  drop_na(Real_Wage, COLI) %>%
  select(Real_Wage, COLI) %>%
  scale()

# Elbow plot (base R)
wss <- map_dbl(1:10, ~ kmeans(cluster_df, centers = .x, nstart = 25)$tot.withinss)
plot(1:10, wss, type = "b",
     xlab = "k", ylab = "Within-cluster SS",
     main = "Elbow Method")

# Choose k = 3, then:
set.seed(42)
km_res <- kmeans(cluster_df, centers = 3, nstart = 25)

clustered_states <- wage_vs_coli %>%
  drop_na(Real_Wage, COLI) %>%
  mutate(Cluster = factor(km_res$cluster))

ggplot(clustered_states, aes(Real_Wage, COLI, color = Cluster)) +
  geom_point(size = 3) +
  labs(
    title = "Clusters of States by 2014 Real Wage & COLI",
    x     = "Real Minimum Wage ($, 2014)",
    y     = "COLI (% of U.S. Avg)",
    color = "Cluster"
  ) +
  theme_minimal()