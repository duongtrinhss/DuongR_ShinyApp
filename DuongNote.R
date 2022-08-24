# library(shiny)
# 
# ui <- fluidPage(
#   actionButton(inputId = "clicks",
#                label = "Click me"),
#   sliderInput(inputId = "num", 
#               label = "Choose a number",
#               value = 25, min = 1, max = 100),
#   textInput(inputId = "title",
#             label = "Write title",
#             value = "Histogram of Random Normal Values"),
#   plotOutput("hist"),
#   verbatimTextOutput("stats")
# )
# 
# 
# server <- function(input, output) {
#   data <- reactive({
#     rnorm(input$num) # reactive values
#   }) # reactive expression - build a reactive object
#   output$hist <- renderPlot({ # reactive functions 
#     hist((data()), main = isolate(input$title)) # isolate() makes an non-reactive object
#   })
#   output$stats <- renderPrint({
#     summary(data())
#   })
#   # observeEvent(input$clicks, {
#   #   print(as.numeric(input$clicks))
#   # })
#   observe({
#     print(as.numeric(input$clicks))
#   })
# }
# 
# shinyApp(ui = ui, server = server)


library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a numeber",
              value = 25, min = 1, max = 100),
  
  actionButton(inputId = "go",
               label = "Update"),
  
  plotOutput("hist")
)

server <-  function(input, output) {
  data <-  eventReactive(input$go, {
    rnorm(input$num)
  })
  output$hist <-  renderPlot({
    hist(data())
  })
}

shinyApp(ui = ui, server = server)

# Reactive toolkit ====
## Display output with render*() ====
## renderPlot(), renderPrint()
## Trigger code with observeEvent() ====
## Delay reactions with eventReactive() =====
## Manage  state with reactiveValues() ====



names(tags)

names(tags$h1())
tags$h1
tags$h1()
tags$a()
tags$a(href = "www.rstudio.com", "RStudio")

# Using Rshiny.io

# install.packages("rsconnect")
# library(rsconnect)
# rsconnect::setAccountInfo(name='duongtrinh', token='F7918F96CA07A6C704F1FE3BB7D7021B', secret='cq3T2UKsYhMeXXHQ863jTqhbUtJgcbDQFKbAcikw')
# rsconnect::deployApp('path/to/your/app')
