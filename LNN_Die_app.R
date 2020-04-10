#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

ui3 <- fluidPage(
  titlePanel('Law of Large Numbers: Dice Roll'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = 'tabset',
      tabPanel('point',
               sliderInput('n1', "Number of Rolls", min=1000, max=10000, value = 5000)
      ),
      tabPanel('hist',
               sliderInput('n2', "Number of Rolls", min=1000, max=10000, value = 5000)
      ))),
    mainPanel(
      plotOutput('LNN')
      )
  )
)

server3 <- function(input, output){

  
  r <- 1 # 'n' number of rolls; 'r' desired roll
  
  output$LNN <- renderPlot({
    
        roll1 <- function(n1){ # function to simulate a six-faced die roll
      die <- 1:6
      result <- sample(die, size = n1, replace = T)
      return(result)
    }
    
    roll2 <- function(n2){ # function to simulate a six-faced die roll
      die <- 1:6
      result <- sample(die, size = n2, replace = T)
      return(result)
    }
    
    set.seed(1)
    v1 <- roll1(input$n1); 
    num_roll1 <- seq_along(v1); v1_prop <- (cumsum(v1 == r)) / num_roll1
    df1 <- cbind.data.frame(num_roll1, v1_prop, v1)
    
    set.seed(1)
    v2 <- roll2(input$n2); 
    num_roll2 <- seq_along(v2); v2_prop <- (cumsum(v2 == r)) / num_roll2
    df2 <- cbind.data.frame(num_roll2, v2_prop, v2)
    
    if(input$tabset == 'point'){
    ggplot(df1, aes(x=num_roll1, y=v1_prop)) + geom_point(color = 'dodgerblue4') + scale_y_continuous(limits = c(0,0.25)) + 
      geom_hline(yintercept=0.16667, color = 'goldenrod4', linetype = 'dashed') + xlab('Number of Rolls') + ylab('Proportion of 1s') +
        ggtitle('Proportion "1" results')
    }
    else{
      ggplot(df2, aes(x=v2)) + geom_histogram(aes(y=..density..), colour='black', fill='dodgerblue4', bins = 6) +
               xlab('Die Results') + ggtitle('Proportion of Die Results')
    }
  })
}
shinyApp(ui3, server3)

