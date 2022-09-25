#' MCMC results analysis - Bayesian Regression (1)
#' =====================================================================================                  
#'
#' =====================================================================================
#' Written by Duong Trinh
#' University of Glasgow - KST project
#' This version: 25 September 2022
#' Input: + resFiMean.rds: "/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/Nsim500nsave2000nburn100_2309/resFiTab.rds"
#'        + resCoMean.rds: "/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/Nsim500nsave2000nburn100_2309/resCoTab.rds"
#' Outputs:
#' =====================================================================================
# setwd("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/DuongR_ShinyApp/KST-ana8")

library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)         # for DataTable
library(janitor)    # for Round

resDfAbMe <- readRDS("resFiMean.rds")
resDfReMe <- readRDS("resCoMean.rds")

ui <- fluidPage(
  titlePanel("Monte Carlo Results"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        wellPanel(
          selectInput('metric', 'Performance Metrics', c("Bias", "MSE", "AUPRC")),
          selectInput('n', 'No. observations', 100),
          selectInput('p', 'No. predictors', c(50,100,150)),
          numericInput('DGP', 'DGP', 1, min = 1, max = 10),
          em(textOutput('describe'))
        ),
        fluidRow(style = "height:150px;")
      ),
      fluidRow(
        wellPanel(
          selectInput('methods', 'Methods', levels(resDfAbMe$Method), multiple = TRUE)
        ),
        fluidRow(style = "height:200px;")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Original",
                 fluidRow(
                   plotOutput('plotAbMe'),
                   fluidRow(style = "height:150px;")
                 ),
                 fluidRow(
                   tabsetPanel(
                     tabPanel(dataTableOutput('tableAbMe1')),
                     tabPanel(dataTableOutput('tableAbMe2'))
                   ),
                 )
        ),
        tabPanel("Relative",
                 fluidRow(
                   plotOutput('plotReMe'),
                   fluidRow(style = "height:150px;")
                   ),
                 fluidRow(dataTableOutput('tableReMe'))
        )
      )
    )
  )
)

server <- function(input, output) {
  
  selectedDataAbMe <- reactive({
    resDfAbMe %>% 
      filter(DGP == input$DGP, n == input$n, p == input$p, 
             (Method %in% input$methods))  
    })
  
  selectedDataReMe <- reactive({
    resDfReMe %>% 
      filter(DGP == input$DGP, n == input$n, p == input$p, 
             (Method %in% input$methods))  
  })
  
  output$plotAbMe <- renderPlot({
    ggplot(selectedDataAbMe(), aes_string(x = "Method", y = input$metric, fill = "Group")) +
      geom_bar( stat = "identity") +
      scale_fill_manual(values = c("Freq" = "#E9D8A6", 
                                   "Naive" = "#EE9B00", "Naive_svt" = "#CA6702", 
                                   "Bayes" = "#94D2BD", "Bayes_svt" = "#0A9396")) +
      coord_flip() 
  }) 
  
  output$tableAbMe1 <- renderDataTable({
    selectedDataAbMe() %>% select(-c("DGP","n","p","Group")) 
  })
  
  output$tableAbMe2 <- renderDataTable({
    DT::datatable(selectedDataAbMe() %>% 
      select("Group", input$metric) %>% 
      group_by(Group) %>% 
      summarise_at(input$metric, ~round(mean(.),3)),
    options = list(paging = FALSE, searching = FALSE),
    caption = "Summary")
    })
  
  output$plotReMe <- renderPlot({
    ggplot(selectedDataReMe(), aes(x = Method, y = Value, col = Metric)) +
      geom_point(size = 1.5) +
      geom_hline(yintercept = 1, col = 'red', size = 0.2) +
      coord_flip()
  })
  
  output$tableReMe <- renderDataTable({
    selectedDataReMe() %>% 
      select(-c("DGP","n","p","Group")) %>% 
      adorn_rounding(digits = 2, rounding = "half up") %>% 
      spread(Metric, Value)
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
  
}

shinyApp(ui = ui, server = server)

