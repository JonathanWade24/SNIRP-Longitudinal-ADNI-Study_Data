# Check and install required packages
packages <- c("shiny", "tidyverse", "DT", "ggpubr", "plotly", "cluster")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required libraries
library(shiny)
library(tidyverse)
library(DT)
library(ggpubr)
library(plotly)
library(cluster)

# Function to load the selected dataset
load_data <- function(dataset) {
  if (dataset == "WM") {
    data <- read_csv("WM_old.csv")
  } else if (dataset == "GM") {
    data <- read_csv("GM_old.csv")
  } else if (dataset == "Thick") {
    data <- read_csv("Thick_old.csv")
  }
  return(data)
}

# Define UI
ui <- fluidPage(
  titlePanel("Brain Data Exploration and Hypothesis Testing"),
  sidebarLayout(
    sidebarPanel(
      h4("Instructions:"),
      p("1. Select the dataset you want to explore."),
      p("2. Use the filters to narrow down the data."),
      p("3. Choose variables for visualization and analysis."),
      p("4. Apply filters to update the data."),
      p("5. Navigate through the tabs to view the data table, summary statistics, visualizations, trendlines, hypothesis tests, clustering, and distribution plots."),

      selectInput("dataset", "Select Dataset:", choices = c("WM", "GM", "Thick"), selected = "WM"),
      selectInput("rid", "Patient ID (RID):", choices = NULL),
      selectInput("diagnosis", "Diagnosis:", choices = NULL),
      sliderInput("ageRange", "Age Range:", min = 0, max = 100, value = c(0, 100)),
      selectInput("gender", "Gender:", choices = NULL),
      selectInput("tbiStatus", "TBI Status:", choices = c("All", "TBI", "Non-TBI"), selected = "All"),
      selectInput("timeVar", "Time Variable:", choices = "months_since_bl_exam", selected = "months_since_bl_exam"),
      selectInput("var1", "Variable 1:", choices = NULL),
      selectInput("var2", "Variable 2:", choices = NULL),
      selectInput("testType", "Hypothesis Test Type:", choices = c("Correlation", "T-Test", "ANOVA", "Chi-Squared", "Linear Regression")),
      selectInput("clusterVar1", "Clustering Variable 1:", choices = NULL),
      selectInput("clusterVar2", "Clustering Variable 2:", choices = NULL),
      selectInput("distVar", "Distribution Variable:", choices = NULL),
      selectInput("groupVar", "Group by:", choices = NULL),
      actionButton("applyFilters", "Apply Filters")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview", DTOutput("dataTable")),
        tabPanel("Summary Statistics", verbatimTextOutput("summaryStats")),
        tabPanel("Visualizations", plotlyOutput("dataPlot")),
        tabPanel("Trendlines", plotlyOutput("trendPlot")),
        tabPanel("Hypothesis Testing", verbatimTextOutput("hypothesisTest")),
        tabPanel("Clustering", plotlyOutput("clusterPlot")),
        tabPanel("Distribution", plotlyOutput("distPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load initial dataset
  initialData <- reactive({
    load_data("WM")
  })

  # Update input choices based on initial data
  observe({
    data <- initialData()
    updateSelectInput(session, "rid", choices = c("All", unique(data$rid)))
    updateSelectInput(session, "diagnosis", choices = c("All", unique(data$dx_bl)))
    updateSelectInput(session, "gender", choices = c("All", unique(data$ptgender)))
    updateSelectInput(session, "var1", choices = names(data), selected = "age")
    updateSelectInput(session, "var2", choices = names(data), selected = "mmse")
    updateSelectInput(session, "clusterVar1", choices = names(data), selected = "age")
    updateSelectInput(session, "clusterVar2", choices = names(data), selected = "mmse")
    updateSelectInput(session, "distVar", choices = names(data), selected = "age")
    updateSelectInput(session, "groupVar", choices = c("None", names(data)), selected = "None")
    updateSliderInput(session, "ageRange", min = min(data$age, na.rm = TRUE), max = max(data$age, na.rm = TRUE), value = c(min(data$age, na.rm = TRUE), max(data$age, na.rm = TRUE)))
  })

  # Reactive expression for the selected dataset
  dataset <- reactive({
    load_data(input$dataset)
  })

  # Filtered data reactive expression
  filteredData <- reactive({
    data <- dataset()
    tbi_filter <- if (input$tbiStatus == "All") TRUE else if (input$tbiStatus == "TBI") data$tbi == 1 else data$tbi == 0

    data %>%
      filter((rid == input$rid | input$rid == "All") &
               (dx_bl == input$diagnosis | input$diagnosis == "All") &
               (age >= input$ageRange[1] & age <= input$ageRange[2]) &
               (ptgender == input$gender | input$gender == "All") &
               tbi_filter)
  })

  # Render data table
  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10, autoWidth = TRUE, server = TRUE))
  })

  # Render summary statistics
  output$summaryStats <- renderPrint({
    summary(filteredData())
  })

  # Render data plot
  output$dataPlot <- renderPlotly({
    req(input$var1, input$var2, input$timeVar)
    plot_data <- filteredData() %>%
      filter(!is.na(!!sym(input$var2)))

    p <- ggplot(plot_data, aes_string(x = input$timeVar, y = input$var2, color = "tbi")) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Time Series Plot of", input$var2, "by", input$timeVar), x = input$timeVar, y = input$var2)
    ggplotly(p)
  })

  # Render trend plot
  output$trendPlot <- renderPlotly({
    req(input$var1, input$var2, input$timeVar)
    plot_data <- filteredData() %>%
      filter(!is.na(!!sym(input$var2)))

    p <- ggplot(plot_data, aes_string(x = input$timeVar, y = input$var2, color = "tbi")) +
      geom_line() +
      geom_point() +
      geom_smooth(method = "lm", aes_string(group = "tbi")) +
      theme_minimal() +
      labs(title = paste("Trendline of", input$var2, "by", input$timeVar), x = input$timeVar, y = input$var2)
    ggplotly(p)
  })

  # Render hypothesis test
  output$hypothesisTest <- renderPrint({
    req(input$var1, input$var2)
    test_data <- filteredData()

    if (input$testType == "Correlation") {
      if (is.numeric(test_data[[input$var1]]) & is.numeric(test_data[[input$var2]])) {
        test_result <- cor.test(test_data[[input$var1]], test_data[[input$var2]])
      } else {
        test_result <- paste("Correlation test requires both variables to be numeric. Variable 1 is", class(test_data[[input$var1]]), "and Variable 2 is", class(test_data[[input$var2]]))
      }
    } else if (input$testType == "T-Test") {
      if ((is.numeric(test_data[[input$var1]]) & is.factor(test_data[[input$var2]])) ||
          (is.factor(test_data[[input$var1]]) & is.numeric(test_data[[input$var2]]))) {
        if (is.numeric(test_data[[input$var1]]) & is.factor(test_data[[input$var2]])) {
          test_result <- t.test(test_data[[input$var1]] ~ as.factor(test_data[[input$var2]]))
        } else {
          test_result <- t.test(test_data[[input$var2]] ~ as.factor(test_data[[input$var1]]))
        }
      } else {
        test_result <- paste("T-test requires one numeric and one factor variable. Variable 1 is", class(test_data[[input$var1]]), "and Variable 2 is", class(test_data[[input$var2]]))
      }
    } else if (input$testType == "ANOVA") {
      if (is.factor(test_data[[input$var1]]) & is.numeric(test_data[[input$var2]])) {
        test_result <- summary(aov(as.numeric(test_data[[input$var2]]) ~ as.factor(test_data[[input$var1]])))
      } else if (is.factor(test_data[[input$var2]]) & is.numeric(test_data[[input$var1]])) {
        test_result <- summary(aov(as.numeric(test_data[[input$var1]]) ~ as.factor(test_data[[input$var2]])))
      } else {
        test_result <- paste("ANOVA requires one numeric and one factor variable. Variable 1 is", class(test_data[[input$var1]]), "and Variable 2 is", class(test_data[[input$var2]]))
      }
    } else if (input$testType == "Chi-Squared") {
      if (is.factor(test_data[[input$var1]]) & is.factor(test_data[[input$var2]])) {
        test_result <- chisq.test(table(as.factor(test_data[[input$var1]]), as.factor(test_data[[input$var2]])))
      } else {
        test_result <- paste("Chi-Squared test requires both variables to be factors. Variable 1 is", class(test_data[[input$var1]]), "and Variable 2 is", class(test_data[[input$var2]]))
      }
    } else if (input$testType == "Linear Regression") {
      if (is.numeric(test_data[[input$var1]]) & is.numeric(test_data[[input$var2]])) {
        model <- lm(as.numeric(test_data[[input$var2]]) ~ as.numeric(test_data[[input$var1]]))
        test_result <- summary(model)
      } else {
        test_result <- paste("Linear Regression requires both variables to be numeric. Variable 1 is", class(test_data[[input$var1]]), "and Variable 2 is", class(test_data[[input$var2]]))
      }
    }
    test_result
  })

  # Render cluster plot
  output$clusterPlot <- renderPlotly({
    req(input$clusterVar1, input$clusterVar2)
    plot_data <- filteredData() %>%
      select(input$clusterVar1, input$clusterVar2) %>%
      drop_na()

    if (nrow(plot_data) > 1) {
      num_clusters <- 3
      kmeans_result <- kmeans(plot_data, centers = num_clusters)
      plot_data$cluster <- as.factor(kmeans_result$cluster)

      p <- ggplot(plot_data, aes_string(x = input$clusterVar1, y = input$clusterVar2, color = "cluster")) +
        geom_point() +
        theme_minimal() +
        labs(title = paste("K-means Clustering of", input$clusterVar1, "and", input$clusterVar2), x = input$clusterVar1, y = input$clusterVar2)
      ggplotly(p)
    } else {
      return(NULL)
    }
  })

  # Render distribution plot
  output$distPlot <- renderPlotly({
    req(input$distVar)
    plot_data <- filteredData()

    if (input$groupVar == "None") {
      p <- ggplot(plot_data, aes_string(x = input$distVar)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Distribution of", input$distVar), x = input$distVar, y = "Frequency")
    } else {
      p <- ggplot(plot_data, aes_string(x = input$distVar, fill = input$groupVar)) +
        geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Distribution of", input$distVar, "by", input$groupVar), x = input$distVar, y = "Frequency", fill = input$groupVar)
    }

    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
