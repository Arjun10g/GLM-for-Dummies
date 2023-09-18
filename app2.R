# Choosing a predictor (categorical vs continous)
distribution_gen <- function(n, skew = c('none', 'negative skew', 'positive skew', 'bimodal'),degree) {
  switch(
    skew,
    'none' = rnorm(n),
    'negative skew' = rbeta(n, degree, 1),
    'positive skew' = rbeta(n, 1, degree),
    'bimodal' = c(rnorm(n/2, mean = 2), rnorm(n/2, mean = -2))
  )
}

gen_model <- function(n, skew_pred, degree_pred, skew_error, degree_error, pred, intercept, slope){
  if(pred == 'numeric'){
    # Assuming you have a distribution_gen function for numeric data
    x <- distribution_gen(n, skew_pred, degree_pred)  %>% scale # Replace 'n' with appropriate arguments
  } else if(pred == 'categorical'){
    x <- rbinom(n, 1, 0.5)
  }
  
  error <- distribution_gen(n, skew_error, degree_error) %>% scale  # Replace 'n' with appropriate arguments
  
  y <- intercept + slope * x + error
  
  dat <- data.frame(x, y)
  return(dat)
}

library(shiny)
library(tidyverse)
library(janitor)
library(shinyjs)

ui <- fluidPage(
  tags$head(includeCSS("www/clean.css")),
  titlePanel(tags$h1('Introduction to simple GLMs', style = 'text-align:center;')),
  sidebarPanel(
    tags$h3("Sample Size"),
    sliderInput("num", "Number of Random Numbers", value = 10, min = 1, max = 1000,step = 5),
    tags$h3('Choose Predictor Description'),
    selectInput('ptype', 'Predictor Type', c('numeric', 'categorical'), selected = 'numeric'),
    selectInput('pskew', 'Predictor Skew', c('none', 'negative skew', 'positive skew', 'bimodal'), selected = 'none'),
    sliderInput('pdegree', 'Degree of Skew for predictor', value = 1, min = 1, max = 100),
    sliderInput('intercept', 'Intercept for predictor', value = 0, min = 0, max = 5),
    sliderInput('slope', 'Slope for predictor', value = 0, min = 0, max = 5),
    tags$h3('Choose Error Description'),
    selectInput('eskew', 'Error Skew', c('none', 'negative skew', 'positive skew', 'bimodal'), selected = 'none'),
    sliderInput('edegree', 'Degree of Skew for error', value = 1, min = 1, max = 100),
    actionButton('plot',label = 'Plot'),
    actionButton('model', 'Model')
  ),
  mainPanel(
    plotOutput('hist'),
    verbatimTextOutput('mod')
  )
)

server <- function(input, output, session) {
  observeEvent(c(input$plot, input$model), {
    dat <- gen_model(n = input$num, skew_pred = input$pskew, degree_pred = input$pdegree,
                     skew_error = input$eskew, degree_error = input$edegree,
                     pred = input$ptype, intercept = input$intercept, slope = input$slope)
    
    output$hist <- renderPlot({
      if (input$ptype == 'numeric') {
        plot <- dat %>% ggplot(aes(x, y)) + 
          geom_point(col = 'black', fill = 'orange', alpha = 1, size = 3, shape = 21) +
          geom_smooth(method = 'lm', col = 'black', alpha = 0.4) +
          scale_x_continuous(name = 'Predictor') +
          labs(y = 'Outcome') +
          papaja::theme_apa()
      } else {
        plot <- dat %>% ggplot(aes(factor(x), y)) + 
          geom_boxplot(col = 'black', fill = 'orange', alpha = 1) +
          scale_x_discrete(name = 'Predictor') +
          labs(y = 'Outcome') +
          papaja::theme_apa()
      }
      plot  # Return the plot object
    })
    
    output$mod <- renderPrint({
      if(input$ptype == 'categorical'){
      model <- lm(y ~ factor(x), data = dat)
      model %>% summary()
      }else{
        model <- lm(y ~ x, data = dat)
      }
      model
    })
  })
}


shinyApp(ui, server)




