# app.R

# Load core Shiny + UI theming + tidyverse for data manipulation
library(shiny)          # Core Shiny functions
library(shinythemes)    # Pre-built themes for Shiny
library(tidyverse)      # dplyr, ggplot2, readr, etc.
library(lubridate)      # Working with dates
library(maps)           # Base map data for choropleth
library(viridis)        # Color scales for continuous data
library(RColorBrewer)   # Additional discrete color palettes
library(randomForest)   # Random forest modeling

# Define raw GitHub URLs for CSV datasets
cpi_url  <- "https://raw.githubusercontent.com/aashishkasaju/data332_finals/main/finals/data/us_cpi_inflation.csv"
coli_url <- "https://raw.githubusercontent.com/aashishkasaju/data332_finals/main/finals/data/cost_of_living.csv"
wage_url <- "https://raw.githubusercontent.com/aashishkasaju/data332_finals/main/finals/data/minimum_wage_data.csv"

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

# Static computations
wage_2014   <- min_wage_data %>%
  filter(Year == 2014) %>%
  transmute(State, Minimum_Wage = Effective.Minimum.Wage) %>%
  left_join(coli_clean, by="State") %>%
  mutate(Real_Wage = Minimum_Wage / (COLI/100))
scatter_df <- wage_2014 %>% drop_na(Real_Wage, COLI)
r_value    <- cor(scatter_df$Real_Wage, scatter_df$COLI)

time_series <- min_wage_data %>%
  filter(Year >= 2004, Year <= 2014) %>%
  transmute(State, Year, Nominal_Min_Wage = Effective.Minimum.Wage) %>%
  left_join(cpi_clean, by="Year") %>%
  mutate(Real_Min_Wage = Nominal_Min_Wage * (cpi_ref/CPI))
avg_real_wage <- time_series %>%
  group_by(Year) %>%
  summarize(Avg_Real_Min_Wage = mean(Real_Min_Wage, na.rm=TRUE), .groups="drop")

growth <- time_series %>%
  filter(Year %in% c(2004, 2014)) %>%
  select(State, Year, Real_Min_Wage) %>%
  pivot_wider(names_from=Year, names_prefix="Y", values_from=Real_Min_Wage) %>%
  drop_na() %>%
  mutate(
    Absolute_Change = Y2014 - Y2004,
    Percent_Change  = (Y2014/Y2004 - 1)*100
  )
top10    <- growth %>% slice_max(Percent_Change, n=10, with_ties=FALSE)
bottom10 <- growth %>% slice_min(Percent_Change, n=10, with_ties=FALSE)

# Build predictive models
lm_mod <- lm(Real_Wage ~ COLI, data=scatter_df)
rf_mod <- randomForest(Real_Wage ~ COLI, data=scatter_df, ntree=100)

# UI definition
ui <- navbarPage(
  "Inflation vs. Wage Growth Dashboard",
  theme=shinytheme("flatly"),
  
  tabPanel("Introduction", fluidPage(
    titlePanel("Inflation vs. Minimum Wage Growth (2004–2014)"),
    p("This dashboard explores how U.S. state‐level minimum wages kept pace—or failed to—against inflation and local living costs from 2004 to 2014. 
    By first converting statutory wage rates into constant 2014 dollars using the Consumer Price Index (CPI), and then adjusting for regional price 
    differences via a state‐level Cost-of-Living Index (COLI), we reveal each state’s true purchasing power. You can interactively select any year to
    see bar charts and maps of real wages, track the national average trend over the decade, compare top and bottom performers in percentage growth, analyze
    the correlation between cost of living and real wage, and even forecast real wage outcomes from hypothetical COLI values using both linear regression and 
    random‐forest models. This comprehensive approach shines a light on where minimum-wage workers gained or lost ground—and why."),
    h4("App Structure & Data Sources"),
    tags$ul(
      tags$li("Data: CPI & inflation (1913–2014), COLI by state, annual minimum wage history"),
      tags$li("Snapshot: Bar & Map for a chosen year"),
      tags$li("Trend: National avg real wage (2004–2014)"),
      tags$li("Growth: Top & bottom 10 states by % change"),
      tags$li("Relationship: Scatter vs. COLI (Pearson’s r)"),
      tags$li("Modeling: Linear & Random Forest predictions based on COLI")
    )
  )),
  
  tabPanel("Scope & Backlog", fluidPage(
    h3("Scope & Requirements"),
    tags$ul(
      tags$li("Join datasets: Wage+CPI, Wage+COLI"),
      tags$li("Separate tabs per visualization"),
      tags$li("Interactive year sliders"),
      tags$li("Clear legends & color scales"),
      tags$li("Bonus: Prediction Model tab")
    ),
    h3("Backlog & Next Steps"),
    tags$ul(
      tags$li("Add state filter dropdown"),
      tags$li("Incorporate additional economic indicators"),
      tags$li("Enable data/plot download options")
    )
  )),
  
  tabPanel("Data Preview", fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput("year1","Select Year:",min=2004,max=2014,value=2014,sep="")),
      mainPanel(h3("Joined Data Snapshot"), tableOutput("tblPreview"))
    )
  )),
  
  tabPanel("Snapshot", fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput("year2","Select Year:",min=2004,max=2014,value=2014,sep="")),
      mainPanel(
        h3(textOutput("titleBar")),
        plotOutput("plotBar", height="900px", width="100%")  # further increased
      )
    )
  )),
  
  tabPanel("Map Snapshot", fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput("year3","Select Year:",min=2004,max=2014,value=2014,sep="")),
      mainPanel(
        h3(textOutput("titleMap")),
        plotOutput("plotMap", height="600px")
      )
    )
  )),
  
  tabPanel("Trend Analysis", fluidPage(
    tags$br(),
    plotOutput("plotTrend", height="500px")
  )),
  
  tabPanel("Growth Analysis", fluidPage(
    tags$br(),
    plotOutput("plotGrowth", height="500px")
  )),
  
  tabPanel("Relationship", fluidPage(
    tags$br(),
    plotOutput("plotScatter", height="500px")
  )),
  
  tabPanel("Prediction Model", fluidPage(
    sliderInput("coli_input","COLI Input (%):",min=min(scatter_df$COLI),max=max(scatter_df$COLI),value=100),
    verbatimTextOutput("predLM"),
    verbatimTextOutput("predRF")
  ))
)

# Server logic
server <- function(input, output, session) {
  output$tblPreview <- renderTable({
    min_wage_data %>%
      filter(Year==input$year1) %>%
      transmute(State,Minimum_Wage=Effective.Minimum.Wage) %>%
      left_join(coli_clean,by="State") %>%
      mutate(Real_Wage=Minimum_Wage/(COLI/100))
  })
  
  output$titleBar <- renderText({paste0("Real Minimum Wage by State, ",input$year2)})
  output$plotBar <- renderPlot({
    df <- min_wage_data %>%
      filter(Year==input$year2) %>%
      transmute(State,Minimum_Wage=Effective.Minimum.Wage) %>%
      left_join(coli_clean,by="State") %>%
      mutate(Real_Wage=Minimum_Wage/(COLI/100))
    ggplot(df,aes(reorder(State,Real_Wage),Real_Wage,fill=Real_Wage))+geom_col()+scale_fill_viridis_c(name="Real $")+coord_flip()+theme_minimal(base_size=18)
  })
  
  output$titleMap <- renderText({paste0(input$year3," Real Minimum Wage Map")})
  output$plotMap <- renderPlot({
    tmp <- min_wage_data %>%
      filter(Year==input$year3) %>%
      transmute(State,Minimum_Wage=Effective.Minimum.Wage) %>%
      left_join(coli_clean,by="State") %>%
      mutate(Real_Wage=Minimum_Wage/(COLI/100))
    ggplot(states_map %>% left_join(tmp%>%transmute(region=tolower(State),Real_Wage),by="region"),aes(long,lat,group=group,fill=Real_Wage))+geom_polygon(color="white",size=0.2)+coord_quickmap()+scale_fill_viridis_c(name="Real $")+theme_void(base_size=18)
  })
  
  output$plotTrend <- renderPlot({
    ggplot(avg_real_wage,aes(Year,Avg_Real_Min_Wage))+geom_line(linewidth=1)+geom_point(size=3)+labs(x="Year",y="Avg Real Wage ($)")+theme_minimal(base_size=18)
  })
  
  output$plotGrowth <- renderPlot({
    bind_rows(top10%>%mutate(Category="Top 10 Growth"),bottom10%>%mutate(Category="Bottom 10 Growth"))%>%ggplot(aes(reorder(State,Percent_Change),Percent_Change,fill=Category))+geom_col()+coord_flip()+scale_fill_brewer(palette="Set1")+theme_minimal(base_size=18)
  })
  
  output$plotScatter <- renderPlot({
    ggplot(scatter_df,aes(COLI,Real_Wage))+geom_point(size=2)+geom_smooth(method="lm",se=FALSE,linewidth=1)+labs(x="COLI (% of U.S. avg)",y="Real Wage ($)",title=paste0("Real Wage vs. COLI (Pearson r = ",round(r_value,2),")"))+theme_minimal(base_size=18)
  })
  
  output$predLM <- renderPrint({pred<-predict(lm_mod,newdata=data.frame(COLI=input$coli_input));cat("Linear Model Prediction:",round(pred,2))})
  output$predRF <- renderPrint({pred<-predict(rf_mod,newdata=data.frame(COLI=input$coli_input));cat("Random Forest Prediction:",round(pred,2))})
}

# Run app
shinyApp(ui, server)