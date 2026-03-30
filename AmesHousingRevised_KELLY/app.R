library(shiny)
library(tidyverse)

# LOAD DATA
ames <- readr::read_csv("data/AmesHousing.csv")

ames <- ames |>
  rename(
    Gr_Liv_Area = `Gr Liv Area`,
    Year_Built  = `Year Built`
  )

stopifnot(all(c("SalePrice","Gr_Liv_Area","Year_Built") %in% names(ames)))

ui <- fluidPage(
  titlePanel("Subsetting, Modeling, and Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Choose X Variable",
                  choices = c("Gr_Liv_Area", "Year_Built"),
                  selected = "Gr_Liv_Area"),
      sliderInput("year", "Minimum Year Built",
                  min = min(ames$Year_Built, na.rm = TRUE),
                  max = max(ames$Year_Built, na.rm = TRUE),
                  value = min(ames$Year_Built, na.rm = TRUE)),
      
      sliderInput("area", "Max Living Area",
                  min = min(ames$Gr_Liv_Area, na.rm = TRUE),
                  max = max(ames$Gr_Liv_Area, na.rm = TRUE),
                  value = 4000),
      radioButtons("log_price",
                   label    = "Sale Price Scale",
                   choices  = c("Log Scale"   = "log",
                                "Raw Dollars" = "raw"),
                   selected = "log"),
    ),  
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Plot", plotOutput("scatter")),
        tabPanel("Model Summary", verbatimTextOutput("model"))
      )
    )
  )
)

server <- function(input, output) {
  
  # FILTERED DATA 
  data_filtered <- reactive({
    df <- ames |>
      filter(
        Year_Built  >= input$year,
        Gr_Liv_Area <= input$area
      )
    
    if (input$log_price == "log") {      
      df <- df |> mutate(y_var = log(SalePrice))
    } else {
      df <- df |> mutate(y_var = SalePrice)
    }
    df
  })
  
  # MODEL
  model_fit <- reactive({
    req(nrow(data_filtered()) > 30)
    
    lm(
      y_var ~ Gr_Liv_Area + I(Gr_Liv_Area^2) + Year_Built,  # <-- y_var instead of log_price
      data = data_filtered()
    )
  })
  
  # RAW DATA PLOT
  output$scatter <- renderPlot({
    df <- data_filtered()
    req(nrow(df) > 0)
    
    x <- input$xvar
    
    y_label <- if (input$log_price == "log") "Log(Sale Price)" else "Sale Price ($)"  
    
    ggplot(df, aes(x = .data[[x]], y = y_var)) +  
      geom_point(alpha = 0.4, color = "violet") +
      geom_smooth(method = "lm", formula = y ~ x, color = "darkred") +
      labs(
        x     = x,
        y     = y_label,                   
        title = paste("Raw Relationship:", x, "vs", y_label)
      ) +
      theme_minimal()
  })
  
  # MODEL SUMMARY
  output$model <- renderPrint({
    summary(model_fit())
  })
}

shinyApp(ui, server)
