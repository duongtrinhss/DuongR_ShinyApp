# Bayesian Shrinkage priors
# =====================================================================================                  
#
# =====================================================================================
# Written by Duong Trinh
# University of Glasgow - KST project
# This version: 28 July 2022
# Outputs:
# =====================================================================================
library(shiny)
library(ggplot2)
library(tidyverse)
library(purrr)

ndraws <- 1e5


ui <- fluidPage(
  titlePanel('CTS-studt prior'),
  fluidRow(
  column(2,
          wellPanel(
            numericInput(inputId = 'beta1', 'beta - Enter a number', value = 0.001),
            selectizeInput('alpha1', 'alpha - Enter a vector', choices = NULL, multiple = TRUE, options = list(create = TRUE)),
            actionButton("simulate1", "Simulate!")
          )
  ),
  column(5,
    plotOutput(outputId = 'plot_pa1')
  ),
  column(5,
         plotOutput(outputId = 'tail_pa1')
  )
  ),
  fluidRow(
    column(2,
           wellPanel(
             numericInput(inputId = 'alpha2', 'alpha - Enter a number', value = 1),
             selectizeInput('beta2', 'beta - Enter a vector', choices = NULL, multiple = TRUE, options = list(create = TRUE)),
             actionButton("simulate2", "Simulate!")
           )
    ),
    column(5,
           plotOutput(outputId = 'plot_pa2')
    ),
    column(5,
           plotOutput(outputId = 'tail_pa2')
    )
  )
)

server <- function(input,output) {
  df1_pa1 <- eventReactive(input$simulate1,{
    lapply(as.numeric(input$alpha1), function(x){
      tausq <- 1/rgamma(ndraws, shape = x, scale = 1/input$beta1)
      map_dbl(tausq, ~rnorm(1, mean = 0, sd = sqrt(.x)))
    }
    )
  })
  
  df2_pa1 <- reactive({
    data_frame(x = unlist(df1_pa1()), alpha = factor(rep(input$alpha1, each = ndraws)))
  })
  
  output$plot_pa1 <- renderPlot({
    ggplot(df2_pa1(), aes(x=x, color = alpha)) +
      geom_density() +
      xlim(-10,10) + coord_cartesian(c(-5,5)) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  })
  
  output$tail_pa1 <- renderPlot({
    ggplot(df2_pa1(), aes(x=x, color = alpha)) +
      geom_line(aes(y = 1 - ..y.., linetype = alpha), stat='ecdf') +
      coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 0.5), expand = FALSE) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  })
  
  df1_pa2 <- eventReactive(input$simulate2,{
    lapply(as.numeric(input$beta2), function(x){
       tausq <- 1/rgamma(ndraws, shape = input$alpha2, scale = 1/x)
       map_dbl(tausq, ~rnorm(1, mean = 0, sd = sqrt(.x)))
    }
    )
  })
  
  df2_pa2 <- reactive({
    data_frame(x = unlist(df1_pa2()), beta = factor(rep(input$beta2, each = ndraws)))
  })
  
  output$plot_pa2 <- renderPlot({
    ggplot(df2_pa2(), aes(x=x, color = beta)) +
      geom_density() +
      xlim(-10,10) + coord_cartesian(c(-5,5)) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  })
  
  output$tail_pa2 <- renderPlot({
    ggplot(df2_pa2(), aes(x=x, color = beta)) +
    geom_line(aes(y = 1 - ..y.., linetype = beta), stat='ecdf') +
    coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 0.5), expand = FALSE) +
    theme(axis.title = element_blank(), legend.position = "bottom") + 
    guides(lty=guide_legend(nrow=1,byrow=TRUE))
  })
  
}

shinyApp(ui = ui, server = server)


