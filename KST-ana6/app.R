# MCMC results analysis - Bayesian Regression (2)
# =====================================================================================                  
#
# =====================================================================================
# Written by Duong Trinh
# University of Glasgow - KST project
# This version: 23 July 2022
# Input: "/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/Nsim500nsave2000nburn100_2307/resCoTab.rds"
# Outputs:
# =====================================================================================

# setwd("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/DuongR_ShinyApp/KST-ana6")

library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(janitor)

resCoTab <- readRDS("data.rds")
ui <- fluidPage(
  titlePanel('Simulation Results - 2'),
  fluidRow(
    column(3,
           wellPanel(
             selectInput('n', 'No. observations', 100),
             selectInput('p', 'No. predictors', c(50,100,150)),
             numericInput('DGP', 'DGP', 1, min = 1, max = 10),
             em(textOutput('describe'))
           )
    ),
    column(9,
           plotOutput('plot1')
    )
  ),
  fluidRow(
    column(3,
           wellPanel(
             selectInput('methods', 'Methods', levels(resCoTab$Method), multiple = TRUE)
           )
    ),    
    column(9,
           dataTableOutput('table1')
    ) 
  )
)

server <- function(input, output) {
  selectedData <- reactive({
    resCoTab %>% 
      filter(DGP == input$DGP, n == input$n, p == input$p, 
             (Method %in% input$methods))  
  })
  
  output$plot1 <- renderPlot({
    ggplot(selectedData(), aes(x = Method, y = Value, col = Metric)) +
      geom_point(size = 1.5) +
      geom_hline(yintercept = 1, col = 'red', size = 0.2) +
      coord_flip()
  })
  
  vecDes <- c("Homoskedasticity, Uncorrelated predictors, Rsquared = 0.4",
              "Homoskedasticity, Uncorrelated predictors, Rsquared = 0.8",
              "Homoskedasticity, Correlated predictors - rho = 0.4, Rsquared = 0.4",
              "Homoskedasticity, Correlated predictors - rho = 0.4, Rsquared = 0.8",
              "Homoskedasticity, Correlated predictors - rho = 0.8, Rsquared = 0.4",
              "Homoskedasticity, Correlated predictors - rho = 0.8, Rsquared = 0.8",
              "Heteroskedasticity, Uncorrelated predictors, Rsquared = 0.4",
              "Heteroskedasticity, Uncorrelated predictors, Rsquared = 0.8",
              "Stochastic volatility, Uncorrelated predictors, Rsquared = 0.4",
              "Stochastic volatility, Uncorrelated predictors, Rsquared = 0.8"
  )
  
  output$describe <- renderText({
    paste(vecDes[input$DGP])
  })
  
  output$table1 <- renderDataTable({
    selectedData() %>% 
      select(-c("DGP","n","p","Group")) %>% 
      adorn_rounding(digits = 2, rounding = "half up") %>% 
      spread(Metric, Value)
  })
  
}
  
  
shinyApp(ui = ui, server = server)