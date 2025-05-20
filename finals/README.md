# Inflation vs. Wage Growth: Are Incomes Keeping Up?

## Project Overview
This Shiny application explores how state minimum wages in the U.S. compare once adjusted for local cost of living. We:

- Adjust each state’s nominal minimum wage to 2014 dollars using Consumer Price Index (CPI) data.
- Incorporate a state-level Cost‑of‑Living Index (COLI) to compute “real” wages.
- Track the evolution of real wages from 2004 to 2014.
- Identify top and bottom 10 states by percentage growth.
- Examine the relationship between purchasing power and cost of living.
- Cluster states based on real wage & COLI.

## Folder Structure
```
├── app.R               # Main Shiny app
├── data/
│   ├── us_cpi_inflation.csv
│   ├── cost_of_living.csv
│   └── minimum_wage_data.csv
├── www/
│   └── kanban.png      # Project Kanban board screenshot
└── README.md
```

## Installation
1. Clone this repository.
2. From RStudio or an R console:
   ```r
   install.packages(c("shiny", "tidyverse", "lubridate", "maps", "viridis", "DT"))
   shiny::runApp("app.R")
   ```

## Usage
- **Introduction**: Research motivation, project scope, and Kanban board.
- **Data**: Interactive tables (CPI, COLI, real wage snapshot).
- **2014 Snapshot**: Bar chart + choropleth of real wages by state (2014).
- **Trend**: National average real wage trend (2004–2014).
- **Growth**: Top & bottom 10 states by real‑wage percentage change.
- **Relationship**: Scatter plot of real wage vs. COLI with Pearson’s r.
- **Clustering**: Elbow plot + k‑means clusters of states by real wage & COLI.

## Key Code Snippets
```r
# Compute real wages for 2014:
wage_vs_coli <- min_wage_2014 %>%
  left_join(coli_clean, by = "State") %>%
  mutate(Real_Wage = Minimum_Wage / (COLI / 100))

# Plot national trend:
ggplot(avg_real_wage, aes(Year, Avg_Real_Min_Wage)) +
  geom_line(color = "#1f78b4") + geom_point() +
  labs(title = "Average Real Wage (2004–2014)", y = "Real Wage ($, 2014)")
```

## Data Sources
- **Wage Estimates**: https://www.kaggle.com/datasets/bls/wage-estimates
- **CPI & Inflation**: https://www.kaggle.com/datasets/tunguz/us-consumer-price-index-and-inflation-cpi
- **Cost‑of‑Living Index**: https://www.kaggle.com/datasets/lukkardata/cost-of-living-missouri-economic-research

---

*Prepared for Data 332 Finals project.*
