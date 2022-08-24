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
library(LaplacesDemon)

ndraws <- 1e5


ui <- navbarPage(
  "Baysian shrinkage priors",
  tabPanel('CTS-studt prior', value = "one",
    fluidRow(
      column(2,
        wellPanel(
          numericInput(inputId = 'b1', 'b - Enter a number', value = 0.001),
          selectizeInput('a1', 'a - Enter a vector', choices = NULL, multiple = TRUE, options = list(create = TRUE)),
          actionButton("simulate1_st", "Simulate!")
          )
      ),
      column(5,
        plotOutput(outputId = 'plot_pa1_st'), fluidRow(style = "height:200px;")
      ),
      column(5,
        plotOutput(outputId = 'tail_pa1_st'), fluidRow(style = "height:200px;")
      )
      ),
      fluidRow(
        column(2,
          wellPanel(
            numericInput(inputId = 'a2', 'a - Enter a number', value = 1),
            selectizeInput('b2', 'b - Enter a vector', choices = NULL, multiple = TRUE, options = list(create = TRUE)),
            actionButton("simulate2_st", "Simulate!")
          )
        ),
        column(5,
          plotOutput(outputId = 'plot_pa2_st'), fluidRow(style = "height:200px;")
        ),
        column(5,
          plotOutput(outputId = 'tail_pa2_st'), fluidRow(style = "height:200px;")
        )
        )       
  ),
  tabPanel('CTS-lasso prior', value = "two",
    fluidRow(
      column(2,
            wellPanel(
              numericInput(inputId = 'd1', 'd - Enter a number', value = 1),
              selectizeInput('c1', 'c - Enter a vector', choices = NULL, multiple = TRUE, options = list(create = TRUE)),
              actionButton("simulate1_ls", "Simulate!")
            )
      ),
      column(5,
            plotOutput(outputId = 'plot_pa1_ls'), fluidRow(style = "height:200px;")
      ),
      column(5,
            plotOutput(outputId = 'tail_pa1_ls'), fluidRow(style = "height:200px;")
      )
    ),
    fluidRow(
      column(2,
            wellPanel(
              numericInput(inputId = 'c2', 'c - Enter a number', value = 1),
              selectizeInput('d2', 'd - Enter a vector', choices = NULL, multiple = TRUE, options = list(create = TRUE)),
              actionButton("simulate2_ls", "Simulate!")
            )
      ),
      column(5,
            plotOutput(outputId = 'plot_pa2_ls'), fluidRow(style = "height:200px;")
      ),
      column(5,
            plotOutput(outputId = 'tail_pa2_ls'), fluidRow(style = "height:200px;")
      )
    )
  ),
  tabPanel('CTS-horse-mx prior', value = "three",
    column(2,
    ),
    column(10,
    )
  ),
  tabPanel('CTS-horse-sl prior', value = "for",
    column(2,
    ),
    column(10,
    )     
  ),
  tabPanel('Summary', value = "five",
    column(2,
      wellPanel(
        numericInput(inputId = 'a', 'a - Enter a number', value = 1),
        numericInput(inputId = 'b', 'b - Enter a number', value = 0.001),
        numericInput(inputId = 'c', 'c - Enter a number', value = 1),
        numericInput(inputId = 'd', 'd - Enter a number', value = 1),
        actionButton("simulateComb", "Simulate!")
      )
     ),
     column(5,
       plotOutput(outputId = 'plotComb')
     ),
     column(5,
       plotOutput(outputId = 'tailComb')
     )
  )
)

server <- function(input,output) {

# CTS-studt prior ====
  df1_pa1_st <- eventReactive(input$simulate1_st,{
    lapply(as.numeric(input$a1), function(x){
      tausq <- 1/rgamma(ndraws, shape = x, scale = 1/input$b1)
      map_dbl(tausq, ~rnorm(1, mean = 0, sd = sqrt(.x)))
    }
    )
  })
  
  df2_pa1_st <- reactive({
    data_frame(x = unlist(df1_pa1_st()), a = factor(rep(input$a1, each = ndraws)))
  })
  
  output$plot_pa1_st <- renderPlot({
    ggplot(df2_pa1_st(), aes(x=x, color = a)) +
      geom_density() +
      xlim(-10,10) + coord_cartesian(c(-5,5)) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  output$tail_pa1_st <- renderPlot({
    ggplot(df2_pa1_st(), aes(x=x, color = a)) +
      geom_line(aes(y = 1 - ..y.., linetype = a), stat='ecdf') +
      coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 0.5), expand = FALSE) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  df1_pa2_st <- eventReactive(input$simulate2_st,{
    lapply(as.numeric(input$b2), function(x){
      tausq <- 1/rgamma(ndraws, shape = input$a2, scale = 1/x)
      map_dbl(tausq, ~rnorm(1, mean = 0, sd = sqrt(.x)))
    }
    )
  })
  
  df2_pa2_st <- reactive({
    data_frame(x = unlist(df1_pa2_st()), b = factor(rep(input$b2, each = ndraws)))
  })
  
  output$plot_pa2_st <- renderPlot({
    ggplot(df2_pa2_st(), aes(x=x, color = b)) +
      geom_density() +
      xlim(-10,10) + coord_cartesian(c(-5,5)) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  output$tail_pa2_st <- renderPlot({
    ggplot(df2_pa2_st(), aes(x=x, color = b)) +
      geom_line(aes(y = 1 - ..y.., linetype = b), stat='ecdf') +
      coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 0.5), expand = FALSE) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  
# CTS-lasso prior ====
  df1_pa1_ls <- eventReactive(input$simulate1_ls,{
    lapply(as.numeric(input$c1), function(x){
      lambdasq <- rgamma(ndraws, shape = x, scale = input$d1)
      tausq <-  map_dbl(lambdasq, ~rexp(1, rate = .x/2))
      map_dbl(tausq, ~ rnorm(1, mean = 0, sd = sqrt(.x)))
    }
    )
  })
  
  df2_pa1_ls <- reactive({
    data_frame(x = unlist(df1_pa1_ls()), c = factor(rep(input$c1, each = ndraws)))
  })
  
  output$plot_pa1_ls <- renderPlot({
    ggplot(df2_pa1_ls(), aes(x=x, color = c)) +
      geom_density() +
      xlim(-10,10) + coord_cartesian(c(-5,5)) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  output$tail_pa1_ls <- renderPlot({
    ggplot(df2_pa1_ls(), aes(x=x, color = c)) +
      geom_line(aes(y = 1 - ..y.., linetype = c), stat='ecdf') +
      coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 0.5), expand = FALSE) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  df1_pa2_ls <- eventReactive(input$simulate2_ls,{
    lapply(as.numeric(input$d2), function(x){
      lambdasq <- rgamma(ndraws, shape = input$c2, scale = x)
      tausq <-  map_dbl(lambdasq, ~rexp(1, rate = .x/2))
      map_dbl(tausq, ~ rnorm(1, mean = 0, sd = sqrt(.x)))
    }
    )
  })
  
  df2_pa2_ls <- reactive({
    data_frame(x = unlist(df1_pa2_ls()), d = factor(rep(input$d2, each = ndraws)))
  })
  
  output$plot_pa2_ls <- renderPlot({
    ggplot(df2_pa2_ls(), aes(x=x, color = d)) +
      geom_density() +
      xlim(-10,10) + coord_cartesian(c(-5,5)) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  output$tail_pa2_ls <- renderPlot({
    ggplot(df2_pa2_ls(), aes(x=x, color = d)) +
      geom_line(aes(y = 1 - ..y.., linetype = d), stat='ecdf') +
      coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 0.5), expand = FALSE) +
      theme(axis.title = element_blank(), legend.position = "bottom") + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 600)
  
  # Combination ====
  CTS_studt <- eventReactive(input$simulateComb, {
    lapply(input$a, function(a){
      tausq <- 1/rgamma(ndraws, shape = a, scale = 1/input$b)
      map_dbl(tausq, ~rnorm(1, mean = 0, sd = sqrt(.x)))
    })
  })
  
  CTS_lasso <- eventReactive(input$simulateComb, {
    lapply(input$c, function(c){
      lambdasq <- rgamma(ndraws, shape = c, scale = input$d)
      tausq <-  map_dbl(lambdasq, ~rexp(1, rate = .x/2))
      map_dbl(tausq, ~ rnorm(1, mean = 0, sd = sqrt(.x)))
    })
  })
  
  CTS_horsemx <- eventReactive(input$simulateComb, {
    lapply(ndraws, function(n){
      nu <- 1/rgamma(n, shape = 1/2, scale = 1)
      lamdasq <- map_dbl(nu, ~ 1/rgamma(1, shape = 1/2, scale = .x))
      xi <- 1/rgamma(n, shape = 1/2, scale = 1)
      tausq <- map_dbl(xi, ~ 1/rgamma(1, shape = 1/2, scale = .x))
      temp <- lamdasq*tausq
      map_dbl(temp, ~rnorm(1, mean = 0, sd = .x))
    })
  })

  CTS_horsesl <- eventReactive(input$simulateComb, {
    lapply(ndraws, function(n){
      lambda <- rhalfcauchy(n, scale= 1)
      tau <- map_dbl(lambda, ~rhalfcauchy(1, scale = .x))
      map_dbl(tau, ~rnorm(1, mean = 0, sd = .x))
    })
  })
  
  df_comb <- reactive({
    data_frame(CTS_studt = unlist(CTS_studt()), CTS_lasso = unlist(CTS_lasso()), 
               CTS_horsemx = unlist(CTS_horsemx()), CTS_horsesl = unlist(CTS_horsesl()))
  })
  
  df_long <- reactive({
    gather(df_comb(), Prior, value) %>% 
      mutate(Prior = factor(Prior, levels = list("CTS_studt" = "CTS_studt", "CTS_lasso" = "CTS_lasso", "CTS_horsemx" = "CTS_horsemx", "CTS_horsesl" = "CTS_horsesl")),
    asymp = case_when(Prior=="CTS_horsemx" ~ 0,
                      Prior=="CTS_horsesl" ~ 0,
                      TRUE ~ NA_real_)
    )
    })
 
  output$plotComb <- renderPlot({
    ggplot(df_long(), aes(x = value, colour = Prior)) +
      stat_density(geom = "line", position = "identity") +
      geom_vline(aes(xintercept = asymp), colour = "grey") +
      #scale_linetype_manual(values = c(4,2,3,1)) +
      scale_color_manual(values = c(1,2,3,4)) +
      xlim(-10,10) + coord_cartesian(xlim=c(-5,5), ylim =c(0,10)) +
      theme(axis.title = element_blank(), legend.position = "bottom")
  }, height = 1000)
  
  ##### Tail behavior -----
  # Plot the survival function, i.e., 1-CDF
  # See https://stats.stackexchange.com/questions/86429/which-has-the-heavier-tail-lognormal-or-gamma
  
  output$tailComb <- renderPlot({
    ggplot(df_long(), aes(x = value, group = Prior)) +
      geom_line(aes(y = 1 - ..y.., colour = Prior), stat='ecdf') +
      #scale_linetype_manual(values = c(4,2,3,1)) +
      scale_color_manual(values = c(1,2,3,4)) +
      coord_cartesian(xlim = c(0, 8), ylim = c(0, 1), expand = FALSE) +
      theme(axis.title = element_blank(), legend.position = "bottom", legend.title = element_blank()) + 
      guides(lty=guide_legend(nrow=1,byrow=TRUE))
  }, height = 1000)
  
}

shinyApp(ui = ui, server = server)