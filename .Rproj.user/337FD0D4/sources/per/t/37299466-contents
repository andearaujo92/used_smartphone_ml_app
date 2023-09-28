library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)

model_lr <- readRDS(file = "final_linear_reg_model.rds")
brands <- unique(df$device_brand)
os <- unique(df$os)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(menuItem("Dashboard",tabName = "dash", icon = icon("dashboard"))),
  dashboardBody(markdown('<h1 style="text-align: center;">Used Smartphone Price Prediction<br><br></h1>'),
                tabItem(tabName = "dash",
                box(selectInput(inputId = "os", label = "OS", choices = os)),
                box(selectInput(inputId = "band1", label = "Does it have 4G?", choices = c("yes","no"))),
                box(selectInput(inputId = "band2", label = "Does it have 5G?", choices = c("yes","no"))),
                box(sliderInput(inputId = "r_cam", label = "Rear Camera mp", min = 0, max = 60, value = 48, step = 1)),
                box(sliderInput(inputId = "ft_cam", label = "Front Camera mp", min = 0, max = 40, value = 12, step = 1)),
                box(sliderInput(inputId = "int_mem", label = "Internal Memory", min = 0, max = 2048, value = 256, step = 8)),
                box(sliderInput(inputId = "ram", label = "Ram Memory", min = 0, max = 16, value = 4, step = 1)),
                box(sliderInput(inputId = "bat", label = "Battery", min = 200, max = 6000, value = 3500, step = 100)),
                box(sliderInput(inputId = "days", label = "Days Used", min = 0, max = 365*4, value = 365, step = 1)),
                valueBoxOutput(outputId = "preds", width = 6))
  ))


server <- function(input, output) { 

  output$preds <- renderValueBox({ 
    
  pred <- predict(model_lr, tibble(
                              "device_brand" = "NA",
                              "os" = input$os,
                              "screen_size" = 0,
                              "4g" = input$band1,
                              "5g" = input$band2,
                              "rear_camera_mp" = input$r_cam,
                              "front_camera_mp"= input$ft_cam,
                              "internal_memory"= input$int_mem,
                              "ram"= input$ram,
                              "battery"= input$bat,
                              "weight" = 0,
                              "release_year" = 2012,
                              "days_used"= input$days,
                              "normalized_new_price" = 5)
                  )
    valueBox(value = paste0("$ ",round(pred * 60, 2)), subtitle = "Used Price Prediction", icon = icon("tag"))
  })
  
  
  
  }

shinyApp(ui, server)