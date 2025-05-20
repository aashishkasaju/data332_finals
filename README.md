# Inflation vs. Wage Growth Dashboard

### **Authors:**  
Kritan Shrestha & Aashish Kasaju

---

## Project Overview
This Shiny application examines how U.S. state minimum wages have kept pace (or not) with inflation and local living costs from **2004–2014**. We:
- **Deflate** each state’s statutory minimum wage into constant 2014 dollars using the Consumer Price Index (CPI).  
- **Adjust** for regional price differences via the Cost‑of‑Living Index (COLI).  
- **Visualize** real wages with bar charts, choropleth maps, trend lines, and growth rankings.  
- **Analyze** the correlation between COLI and real wage (Pearson’s _r_).  
- **Forecast** future purchasing power with a linear regression and a random forest model.

All interactive charts are organized into separate tabs for clean navigation and understanding.

---

## Things to Keep in Mind
- **Analysis period:** 2004 to 2014.  
- **Datasets joined:** Minimum wage history + CPI; Minimum wage history + COLI.  
- **Real wage base year:** 2014 (CPI reference).  
- **Interactive features:** Year sliders, COLI input for predictions.  

---

## Source of Data

- **[Consumer Price Index & Inflation](https://www.kaggle.com/datasets/tunguz/us-consumer-price-index-and-inflation-cpi)** (`us_cpi_inflation.csv`): Monthly CPI values and annual inflation rates (1913–2014).
  ![us cpi inflation](https://github.com/user-attachments/assets/94b57403-616d-46da-aad9-374bdcdb25af)
  
- **[Cost‑of‑Living Index](https://www.kaggle.com/datasets/lukkardata/cost-of-living-missouri-economic-research)** (`cost_of_living.csv`): Annual state‑level COLI values (U.S. average = 100).
  ![cost of living ](https://github.com/user-attachments/assets/60efa47a-a6fd-4319-846f-b864510b6d01)

- **[Minimum Wage History](https://www.kaggle.com/datasets/lislejoem/us-minimum-wage-by-state-from-1968-to-2017)** (`minimum_wage_data.csv`): Effective minimum wage for each state, annual from 2004–2014.
  ![US MIN wage](https://github.com/user-attachments/assets/dd3abc5b-3a41-4d2f-bd77-6edb222ed882)

---

## Shiny App Tabs
1. **Introduction**  
   - Title, research summary, app structure, and data sources.  
   - Explanation of why this research matters: purchasing power insights, policy impact, regional disparities.

2. **Scope & Backlog**  
   - Checklist: joined datasets, tabbed layout, labeled visuals, prediction model.  
   - Future enhancements: state filters, additional economic indicators, data downloads.

3. **Data Preview**  
   - Table showing Minimum Wage, COLI, and Real Wage for the selected year.   
<img width="1437" alt="preview" src="https://github.com/user-attachments/assets/93f8133c-2368-4e7e-a286-f0bb65c127d4" />

4. **Snapshot**  
   - Bar chart of real minimum wage by state for the chosen year.   
<img width="1437" alt="snapshot" src="https://github.com/user-attachments/assets/79068c35-94f4-4d46-b1a7-5590ebec7790" />

5. **Map Snapshot**  
   - Choropleth map of real wages across states for the chosen year.  
<img width="1440" alt="map" src="https://github.com/user-attachments/assets/ddeaefd8-b3ce-471f-9d51-1b90a30dd280" />

6. **Trend Analysis**  
   - Line chart of national average real minimum wage (2004–2014).  
<img width="1402" alt="trend" src="https://github.com/user-attachments/assets/63937625-57e9-4642-ae93-9b272be0dbb0" />

7. **Growth Analysis**  
   - Bar chart of Top 10 and Bottom 10 states by percent change (2004→2014).  
<img width="1440" alt="growth" src="https://github.com/user-attachments/assets/df0d7d5a-03b6-4e1f-a397-8aba24b7cb90" />

8. **Relationship**  
   - Scatterplot of Real Wage vs. COLI with linear fit and Pearson’s _r_.  
<img width="1439" alt="relationship" src="https://github.com/user-attachments/assets/89cec7c8-c7ed-4c0b-b402-5b2cdd0b2a10" />

9. **Prediction Model**  
   - Slider to input COLI (%).  
   - Text outputs: linear model and random forest predictions.  
<img width="1440" alt="prediction" src="https://github.com/user-attachments/assets/5bed34ce-6687-488a-924d-7393c7b09738" />

---

## Project Files
```
/data
  ├─ us_cpi_inflation.csv
  ├─ cost_of_living.csv
  └─ minimum_wage_data.csv
/app.R
/images
  ├─ data_preview.png
  ├─ snapshot_bar.png
  ├─ map_snapshot.png
  ├─ trend_line.png
  ├─ growth_analysis.png
  ├─ relationship_scatter.png
  └─ prediction_model.png
README.md
```

---

## Requirements
Install these R packages before running:
```r
install.packages(c(
  "shiny", "shinythemes", "tidyverse",
  "lubridate", "maps", "viridis",
  "RColorBrewer", "randomForest"
))
```

---

## Loading & Cleaning Data
```r
# Define raw GitHub URLs for CSV datasets
cpi_url  <- "https://raw.githubusercontent.com/aashishkasaju/data332_finals/main/data/us_cpi_inflation.csv"
coli_url <- "https://raw.githubusercontent.com/aashishkasaju/data332_finals/main/data/cost_of_living.csv"
wage_url <- "https://raw.githubusercontent.com/aashishkasaju/data332_finals/main/data/minimum_wage_data.csv"

# Load & clean datasets
cpi_clean <- read_csv(cpi_url) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    CPI       = mean(Index, na.rm = TRUE),
    Inflation = mean(Inflation, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(Year >= 2004, Year <= 2014)
cpi_ref <- cpi_clean %>% filter(Year == 2014) %>% pull(CPI)

coli_clean <- read_csv(coli_url) %>%
  select(abb = State, COLI = Conversion) %>%
  mutate(State = state.name[match(abb, state.abb)]) %>%
  select(-abb)

min_wage_data <- read_csv(wage_url)
states_map    <- map_data("state")
```

---

## Core Transformation Example
```r
# Calculate real wage for Snapshot
snapshot_data <- min_wage_data %>%
  filter(Year == input$year2) %>%
  left_join(coli_clean, by="State") %>%
  mutate(Real_Wage = Effective.Minimum.Wage / (COLI / 100))
```

---

## Prediction Model Rationale
We chose **Linear Regression** for interpretability and **Random Forest** for capturing non-linearities:
```r
lm_mod <- lm(Real_Wage ~ COLI, data=scatter_df)
rf_mod <- randomForest(Real_Wage ~ COLI, data=scatter_df, ntree = 100)
```
**Reference:** Breiman, L. (2001). _Random Forests_. _Machine Learning, 45_(1), 5–32.

---

## Running the App
```
shinyApp(ui, server)
```
## Click the link below to visit the interactive shiny app 

https://kritanshrestha.shinyapps.io/final/
