#' MCMC results analysis - Bayesian Regression (1)
#' =====================================================================================                  
#'
#' =====================================================================================
#' Written by Duong Trinh
#' University of Glasgow - KST project
#' This version: 03 August 2022
#' Input: + resFiMean.rds: "/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/Nsim500nsave2000nburn100_2707/resFiTab.rds"
#'        + resCoMean.rds: "/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/Nsim500nsave2000nburn100_2707/resCoTab.rds"
#'        + resFiMedian.rds: "/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/Nsim500nsave2000nburn100_0308/resFiTab.rds"
#'        + resFiMedian.rds: "/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/Nsim500nsave2000nburn100_0308/resCoTab.rds"
#' Outputs:
#' =====================================================================================
# setwd("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/DuongR_ShinyApp/KST-ana7")

library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)         # for DataTable
library(janitor)    # for Round

resDfAbMe <- readRDS("resFiMean.rds")
resDfReMe <- readRDS("resCoMean.rds")
resDfAbMd <- readRDS("resFiMedian.rds")
resDfReMd <- readRDS("resCoMedian.rds")


ui <- fluidPage(
  titlePanel("Monte Carlo Results"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        wellPanel(
          selectInput('metric', 'Performance Metrics', c("Bias", "MSE", "Precision", "Recall")),
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
        ),
        tabPanel("Original - ext",
                 fluidRow(
                   plotOutput('plotAbMd'),
                   fluidRow(style = "height:150px;")
                 ),
                 fluidRow(
                   tabsetPanel(
                     tabPanel(dataTableOutput('tableAbMd1')),
                     tabPanel(dataTableOutput('tableAbMd2'))
                   )
                 )
        ),
        tabPanel("Relative - ext",
                 fluidRow(
                   plotOutput('plotReMd'),
                   fluidRow(style = "height:150px;")
                 ),
                 fluidRow(dataTableOutput('tableReMd'))
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
  
  selectedDataAbMd <- reactive({
    resDfAbMd %>% 
      filter(DGP == input$DGP, n == input$n, p == input$p, 
             (Method %in% input$methods))  
  })
  
  selectedDataReMd <- reactive({
    resDfReMd %>% 
      filter(DGP == input$DGP, n == input$n, p == input$p, 
             (Method %in% input$methods))  
  })
  
  selectedDataAbMeMd <- reactive({
    rbind(resDfAbMe %>% 
            filter(DGP == input$DGP, n == input$n, p == input$p,
                   (Method %in% input$methods)) %>% 
            mutate(Center = "Mean"),
          resDfAbMd %>%
            filter(DGP == input$DGP, n == input$n, p == input$p,
                 (Method %in% input$methods)) %>% 
            mutate(Center = "Median")) %>% 
      mutate(Center = factor(Center, levels = c("Mean","Median")))
  })
  
  selectedDataReMeMd <- reactive({
    rbind(resDfReMe %>% 
            filter(DGP == input$DGP, n == input$n, p == input$p,
                   (Method %in% input$methods)) %>% 
            mutate(Center = "Mean"),
          resDfReMd %>% 
            filter(DGP == input$DGP, n == input$n, p == input$p,
                   (Method %in% input$methods)) %>% 
            mutate(Center = "Median")) %>% 
      mutate(Center = factor(Center, levels = c("Mean","Median")))
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
  
  output$plotAbMd <- renderPlot({
    ggplot(selectedDataAbMeMd(), 
           aes_string(x = "Method", y = input$metric, col = "Center", fill = "Group")) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
      scale_fill_manual(values = c("Freq" = "#E9D8A6", 
                                   "Naive" = "#EE9B00", "Naive_svt" = "#CA6702", 
                                   "Bayes" = "#94D2BD", "Bayes_svt" = "#0A9396")) +
      scale_color_manual(values = c("#999999", "#56B4E9")) +
      coord_flip() 
  }) 
  
  output$tableAbMd1 <- renderDataTable({
    selectedDataAbMd() %>% select(-c("DGP","n","p","Group")) 
  })
  
  output$tableAbMd2 <- renderDataTable({
    DT::datatable(selectedDataAbMd() %>% 
                    select("Group", input$metric) %>% 
                    group_by(Group) %>% 
                    summarise_at(input$metric, ~round(mean(.),3)),
                  options = list(paging = FALSE, searching = FALSE),
                  caption = "Summary")
  })
  
  output$plotReMd <- renderPlot({
    ggplot(selectedDataReMeMd(), 
           aes(x = Method, y = Value, col = Metric, shape = Center)) +
      geom_point(size = 1.5) +
      geom_hline(yintercept = 1, col = 'red', size = 0.2) +
      scale_shape_manual(values = c(2,3)) +
      coord_flip()
  })
  
  output$tableReMd <- renderDataTable({
    selectedDataReMd() %>% 
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

