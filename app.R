
# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Generate dummy data
n_records <- 1000

sales_data <- tibble(
  date = seq(as.Date("2023-01-01"), by = "day", length.out = n_records),
  product = sample(c("Smartphone", "Laptop", "Tablet", "Smartwatch", "Headphones"), n_records, replace = TRUE),
  category = case_when(
    product %in% c("Smartphone", "Tablet") ~ "Mobile",
    product == "Laptop" ~ "Computer",
    TRUE ~ "Accessories"
  ),
  quantity = sample(1:5, n_records, replace = TRUE),
  price = case_when(
    product == "Smartphone" ~ runif(n_records, 500, 1200),
    product == "Laptop" ~ runif(n_records, 800, 2000),
    product == "Tablet" ~ runif(n_records, 300, 800),
    product == "Smartwatch" ~ runif(n_records, 200, 500),
    product == "Headphones" ~ runif(n_records, 50, 300)
  ),
  total_sales = quantity * price,
  customer_rating = sample(1:5, n_records, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.3, 0.35))
)



#Rshiny Dashboard

ui <- dashboardPage(
  dashboardHeader(title = "TechSales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Sales Analysis", tabName = "sales", icon = icon("chart-line")),
      menuItem("Product Performance", tabName = "products", icon = icon("box")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales"),
                valueBoxOutput("avg_rating"),
                valueBoxOutput("top_product")
              ),
              fluidRow(
                box(plotlyOutput("sales_trend"), width = 8),
                box(plotlyOutput("category_pie"), width = 4)
              )
      ),
      tabItem(tabName = "sales",
              fluidRow(
                box(plotlyOutput("daily_sales"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("product_sales"), width = 6),
                box(plotlyOutput("category_sales"), width = 6)
              )
      ),
      tabItem(tabName = "products",
              fluidRow(
                box(plotlyOutput("product_ratings"), width = 6),
                box(plotlyOutput("product_quantity"), width = 6)
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(DTOutput("raw_data"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$total_sales <- renderValueBox({
    total <- sum(sales_data$total_sales)
    valueBox(
      paste0("$", format(round(total / 1e6, 2), nsmall = 2), "M"),
      "Total Sales",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$avg_rating <- renderValueBox({
    avg_rating <- mean(sales_data$customer_rating)
    valueBox(
      round(avg_rating, 2),
      "Average Customer Rating",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$top_product <- renderValueBox({
    top_product <- sales_data %>%
      group_by(product) %>%
      summarize(total_sales = sum(total_sales)) %>%
      arrange(desc(total_sales)) %>%
      slice(1) %>%
      pull(product)
    
    valueBox(
      top_product,
      "Top Selling Product",
      icon = icon("trophy"),
      color = "purple"
    )
  })
  
  output$sales_trend <- renderPlotly({
    daily_sales <- sales_data %>%
      group_by(date) %>%
      summarize(total_sales = sum(total_sales))
    
    plot_ly(daily_sales, x = ~date, y = ~total_sales, type = "scatter", mode = "lines") %>%
      layout(title = "Daily Sales Trend", xaxis = list(title = "Date"), yaxis = list(title = "Total Sales"))
  })
  
  output$category_pie <- renderPlotly({
    category_sales <- sales_data %>%
      group_by(category) %>%
      summarize(total_sales = sum(total_sales))
    
    plot_ly(category_sales, labels = ~category, values = ~total_sales, type = "pie") %>%
      layout(title = "Sales by Category")
  })
  
  output$daily_sales <- renderPlotly({
    plot_ly(sales_data, x = ~date, y = ~total_sales, color = ~product, type = "scatter", mode = "markers") %>%
      layout(title = "Daily Sales by Product", xaxis = list(title = "Date"), yaxis = list(title = "Total Sales"))
  })
  
  output$product_sales <- renderPlotly({
    product_sales <- sales_data %>%
      group_by(product) %>%
      summarize(total_sales = sum(total_sales))
    
    plot_ly(product_sales, x = ~product, y = ~total_sales, type = "bar") %>%
      layout(title = "Total Sales by Product", xaxis = list(title = "Product"), yaxis = list(title = "Total Sales"))
  })
  
  output$category_sales <- renderPlotly({
    category_sales <- sales_data %>%
      group_by(category) %>%
      summarize(total_sales = sum(total_sales))
    
    plot_ly(category_sales, x = ~category, y = ~total_sales, type = "bar") %>%
      layout(title = "Total Sales by Category", xaxis = list(title = "Category"), yaxis = list(title = "Total Sales"))
  })
  
  output$product_ratings <- renderPlotly({
    product_ratings <- sales_data %>%
      group_by(product) %>%
      summarize(avg_rating = mean(customer_rating))
    
    plot_ly(product_ratings, x = ~product, y = ~avg_rating, type = "bar") %>%
      layout(title = "Average Customer Ratings by Product", xaxis = list(title = "Product"), yaxis = list(title = "Average Rating"))
  })
  
  output$product_quantity <- renderPlotly({
    product_quantity <- sales_data %>%
      group_by(product) %>%
      summarize(total_quantity = sum(quantity))
    
    plot_ly(product_quantity, x = ~product, y = ~total_quantity, type = "bar") %>%
      layout(title = "Total Quantity Sold by Product", xaxis = list(title = "Product"), yaxis = list(title = "Total Quantity"))
  })
  
  output$raw_data <- renderDT({
    datatable(sales_data, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)