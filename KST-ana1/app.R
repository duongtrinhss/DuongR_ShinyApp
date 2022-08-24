# MCMC results analysis - Bayesian Regression (1)
# =====================================================================================                  
#
# =====================================================================================
# Written by Duong Trinh
# University of Glasgow - KST project
# This version: July 2022
# Outputs:
# =====================================================================================

library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

resFiTab <- readRDS("data.rds")

ui <- fluidPage(
  titlePanel('Simulation Results - 1'),
  fluidRow(
    column(3,
           wellPanel(
             selectInput('metric', 'Performance Metrics', c("Bias", "MSE", "Precision", "Recall")),
            selectInput('n', 'No. observations', 100),
            selectInput('p', 'No. predictors', c(50,100,150)),
            numericInput('DGP', 'DGP', 1, min = 1, max = 8),
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
             selectInput('methods', 'Methods', levels(resFiTab$Method), multiple = TRUE)
           )
    ),    
    column(9,
           tabsetPanel(
             tabPanel(dataTableOutput('table1')),
             tabPanel(dataTableOutput('table2'))
           )
           
    ) 
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    resFiTab %>% 
      filter(DGP == input$DGP, n == input$n, p == input$p, 
             (Method %in% input$methods))  
    })
  
  output$plot1 <- renderPlot({
    ggplot(selectedData(), aes_string(x = "Method", y = input$metric, fill = "Group")) +
      geom_bar( stat = "identity") +
      scale_fill_manual(values = c("Freq" = "#E9D8A6", 
                                   "Naive" = "#EE9B00", "Naive_svt" = "#CA6702", 
                                   "Bayes" = "#94D2BD", "Bayes_svt" = "#0A9396")) +
      coord_flip()
  })
  
  output$table1 <- renderDataTable({
    selectedData() %>% select(-c("DGP","n","p","Group")) 
  })
  
  output$table2 <- renderDataTable({
    DT::datatable(selectedData() %>% 
      select("Group", input$metric) %>% 
      group_by(Group) %>% 
      summarise_at(input$metric, ~round(mean(.),3)),
    options = list(paging = FALSE, searching = FALSE),
    caption = "Summary")
    })
  
  vecDes <- c("Homoskedasticity, Uncorrelated predictors, Rsquared = 0.4",
              "Homoskedasticity, Uncorrelated predictors, Rsquared = 0.8",
              "Homoskedasticity, Correlated predictors - rho = 0.4, Rsquared = 0.4",
              "Homoskedasticity, Correlated predictors - rho = 0.4, Rsquared = 0.8",
              "Homoskedasticity, Correlated predictors - rho = 0.8, Rsquared = 0.4",
              "Homoskedasticity, Correlated predictors - rho = 0.8, Rsquared = 0.8",
              "Heteroskedasticity",
              "Stochastic volatility"
              )
  
  output$describe <- renderText({
    paste(vecDes[input$DGP])
  })
  
}

shinyApp(ui = ui, server = server)

