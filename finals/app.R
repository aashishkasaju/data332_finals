# app.R

# 0) Libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(viridis)
library(DT)

# 1) Load & prepare data
cpi_clean <- read_csv("~/Desktop/DATA332/r_projects/finals/data/us_cpi_inflation.csv") %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    CPI       = mean(Index, na.rm = TRUE),
    Inflation = mean(Inflation, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(Year >= 2004, Year <= 2014)

cpi_ref <- cpi_clean %>%
  filter(Year == 2014) %>%
  pull(CPI)

coli_clean <- read_csv("~/Desktop/DATA332/r_projects/finals/data/cost_of_living.csv") %>%
  transmute(
    State = state.name[match(State, state.abb)],
    COLI  = Conversion
  )

min_wage_data <- read_csv("~/Desktop/DATA332/r_projects/finals/data/minimum_wage_data.csv")

# 2014 snapshot
wage_vs_coli <- min_wage_data %>%
  filter(Year == 2014) %>%
  transmute(
    State,
    Minimum_Wage = Effective.Minimum.Wage
  ) %>%
  left_join(coli_clean, by = "State") %>%
  mutate(Real_Wage = Minimum_Wage / (COLI / 100))

# time series & national average
real_wage_ts <- min_wage_data %>%
  filter(between(Year, 2004, 2014)) %>%
  transmute(Year, State, Nominal = Effective.Minimum.Wage) %>%
  left_join(cpi_clean, by = "Year") %>%
  mutate(Real_Min_Wage = Nominal * (cpi_ref / CPI))

avg_real_wage <- real_wage_ts %>%
  group_by(Year) %>%
  summarize(Avg_Real_Min_Wage = mean(Real_Min_Wage, na.rm = TRUE), .groups = "drop")

# growth 2004→2014
state_growth <- real_wage_ts %>%
  filter(Year %in% c(2004, 2014)) %>%
  select(State, Year, Real_Min_Wage) %>%
  pivot_wider(names_from = Year, names_prefix = "Y", values_from = Real_Min_Wage) %>%
  drop_na() %>%
  mutate(
    Absolute_Change = Y2014 - Y2004,
    Percent_Change  = (Y2014 / Y2004 - 1) * 100
  )

# scatter & correlation
scatter_df <- wage_vs_coli %>% drop_na(Real_Wage, COLI)
r_value    <- cor(scatter_df$Real_Wage, scatter_df$COLI)

# clustering
cluster_df <- scatter_df %>% select(Real_Wage, COLI) %>% scale()
set.seed(42)
km_res <- kmeans(cluster_df, centers = 3, nstart = 25)
clustered_states <- scatter_df %>% mutate(Cluster = factor(km_res$cluster))

# map data
states_map <- map_data("state")
choropleth_df <- states_map %>%
  left_join(
    wage_vs_coli %>%
      drop_na(Real_Wage) %>%
      transmute(region = tolower(State), Real_Wage),
    by = "region"
  )

# 2) UI
ui <- navbarPage(
  title = "Minimum‐Wage & Cost‐of‐Living",
  
  tabPanel("Introduction",
           fluidPage(
             fluidRow(
               column(12,
                      h1("Inflation vs. Wage Growth: Are Incomes Keeping Up?"),
                      hr()
               )
             ),
             fluidRow(
               column(8,
                      p("In the United States, state minimum wages vary widely, but those sticker 
              rates don’t tell the whole story once you account for differences in cost 
              of living. This app adjusts each state’s minimum wage to 2014 dollars based 
              on the Consumer Price Index (CPI) and a state‐level Cost‐of‐Living Index (COLI), 
              then tracks how “real” wages evolved from 2004–2014, compares growth across 
              states, and clusters states by purchasing power.")
               ),
               column(4,
                      h4("Project Kanban"),
                      img(src = "kanban.png", width = "100%", height = "auto"),
                      p(em("Board shows task breakdown & progress."))
               )
             ),
             fluidRow(
               column(12,
                      h3("App Structure & Data Sources"),
                      tags$ul(
                        tags$li(strong("Data: "), "CPI & inflation (1913–2014), COLI by state, annual minimum‐wage history"),
                        tags$li(strong("2014 Snapshot: "), "Bar chart & choropleth of 2014 real minimum wage by state"),
                        tags$li(strong("Trend: "), "National average real wage 2004–2014"),
                        tags$li(strong("Growth: "), "Top & bottom 10 states by real‐wage % change (2004→2014)"),
                        tags$li(strong("Relationship: "), "Scatter of real wage vs. COLI with Pearson’s r"),
                        tags$li(strong("Clustering: "), "K‐means clusters of states by purchasing power & cost of living")
                      )
               )
             )
           )
  ),
  
  tabPanel("Data",
           DT::dataTableOutput("cpi_table"),
           DT::dataTableOutput("coli_table"),
           DT::dataTableOutput("wage_table")
  ),
  
  tabPanel("2014 Snapshot",
           sidebarLayout(
             sidebarPanel(p("Bar chart & map of 2014 real minimum wage")),
             mainPanel(
               plotOutput("bar2014"),
               plotOutput("map2014")
             )
           )
  ),
  
  tabPanel("Trend", plotOutput("trend_plot")),
  
  tabPanel("Growth", plotOutput("growth_plot")),
  
  tabPanel("Relationship", plotOutput("scatter_plot")),
  
  tabPanel("Clustering",
           plotOutput("elbow_plot"),
           plotOutput("cluster_plot")
  )
)

# 3) Server
server <- function(input, output, session) {
  
  # Data tables
  output$cpi_table  <- DT::renderDataTable(cpi_clean)
  output$coli_table <- DT::renderDataTable(coli_clean)
  output$wage_table <- DT::renderDataTable(wage_vs_coli)
  
  # 2014 bar
  output$bar2014 <- renderPlot({
    wage_vs_coli %>%
      drop_na(Real_Wage) %>%
      ggplot(aes(reorder(State, Real_Wage), Real_Wage)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "2014 Real Wage by State", x = NULL, y = "Real $") +
      theme_minimal(base_size = 14)
  })
  
  # 2014 map
  output$map2014 <- renderPlot({
    ggplot(choropleth_df, aes(long, lat, group = group, fill = Real_Wage)) +
      geom_polygon(color = "white", size = 0.2) +
      coord_quickmap() +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      labs(title = "2014 Real Wage Map", fill = "Real $") +
      theme_void()
  })
  
  # Trend
  output$trend_plot <- renderPlot({
    ggplot(avg_real_wage, aes(Year, Avg_Real_Min_Wage)) +
      geom_line(color = "#1f78b4", linewidth = 1.2) +
      geom_point(color = "#1f78b4", size = 2) +
      labs(title = "Average Real Wage (2004–2014)", x = "Year", y = "Real $ (2014)") +
      theme_minimal(base_size = 14)
  })
  
  # Growth
  output$growth_plot <- renderPlot({
    bind_rows(
      slice_max(state_growth, Percent_Change, n = 10)  %>% mutate(Category = "Top 10"),
      slice_min(state_growth, Percent_Change, n = 10)  %>% mutate(Category = "Bottom 10")
    ) %>%
      ggplot(aes(reorder(State, Percent_Change), Percent_Change, fill = Category)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Top 10" = "#2ca25f", "Bottom 10" = "#de2d26")) +
      labs(title = "Real‐Wage Growth by State (2004→2014)", x = NULL, y = "% Change") +
      theme_minimal(base_size = 14)
  })
  
  # Relationship
  output$scatter_plot <- renderPlot({
    ggplot(scatter_df, aes(COLI, Real_Wage)) +
      geom_point(aes(color = Real_Wage), size = 3) +
      scale_color_viridis_c(option = "plasma", name = "Real $") +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(
        title    = "2014 Real Wage vs. Cost-of-Living Index",
        subtitle = paste0("Pearson r = ", round(r_value, 2)),
        x        = "COLI (% of U.S. Avg)",
        y        = "Real Wage ($)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Elbow
  output$elbow_plot <- renderPlot({
    wss <- map_dbl(1:10, ~ kmeans(cluster_df, centers = .x, nstart = 25)$tot.withinss)
    plot(
      1:10, wss, type = "b",
      xlab = "k", ylab = "Within-cluster SS",
      main = "Elbow Method"
    )
  })
  
  # Clusters
  output$cluster_plot <- renderPlot({
    ggplot(clustered_states, aes(Real_Wage, COLI, color = Cluster)) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Dark2", name = "Cluster") +
      labs(
        title = "State Clusters by Real Wage & COLI (2014)",
        x     = "Real Wage ($)",
        y     = "COLI (% of U.S. Avg)"
      ) +
      theme_minimal(base_size = 14)
  })
}

# 4) Run the app
shinyApp(ui, server)