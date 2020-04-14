library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Density Graph Plotly Demo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textOutput("Placeholder")
        ),

        # Show a plot of the generated distribution
        mainPanel( # Threshold Adjustment Slider
           sliderInput('TSlide', label = "PTE Density Curve", min = 0.0, max = 20, value = 5),
           plotlyOutput('Patients')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Density curves for PTE and non-PTE patients
    PTE <- data.frame(length = rnorm(100000, 6, 2))
    NPTE <- data.frame(length = rnorm(50000, 7, 2.5))
    
    #Now, combine your two dataframes into one.  First make a new column in each.
    PTE$cat <- 'PTE'
    NPTE$cat <- 'NPTE'
    
    #and combine into your new data frame Lengths
    Lengths <- rbind(PTE, NPTE)
    
    output$Patients <- renderPlotly({
       p<- ggplot(Lengths, aes(length, fill = cat )) + geom_density(alpha = 0.2) +
           geom_vline(xintercept = input$TSlide, linetype = 'dashed') +
           xlab('xlabel') + ylab('Density') +ggtitle('title') + theme(legend.title = element_blank())
       ggplotly(p)             
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
