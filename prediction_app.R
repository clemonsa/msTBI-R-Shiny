library(shiny)
server= function(input, output) { }
ui <- fluidPage(
  titlePanel(h1(strong("Predicting seizure events in moderate-to-severe TBI"))),
  mainPanel(
    p("Arvon Clemens, Felix Proessl, Dominic DiSanto")),
    
  fluidRow(
    
    column(5,
           h2(strong("Input Dashboard")),
           h3(strong("Imaging Data")),
           checkboxInput("checkbox", label = "None", value = TRUE),
           checkboxInput("checkbox", label = "None", value = TRUE),
           
           numericInput("num",
                        label = h3(strong("Number of present contusions in CT imaging")),
                        value = 1),
           checkboxInput("checkbox", label = ">4", value = TRUE),
    
           
           h3(strong("Injury/Hospitalization Information")),
           checkboxInput("checkbox", label = "Post-traumatic amnesia > 24h?", value = TRUE),
           checkboxInput("checkbox", label = "GCS < 8?", value = TRUE),
           checkboxInput("checkbox", label = "Days to follow commands > 7?", value = TRUE),
           
           checkboxInput("checkbox", label = "Alcohol present at time of hospitalization?", value = TRUE),
           checkboxInput("checkbox", label = "Acute seizures?", value = TRUE),
           
           
           h3(strong("Pre-Injury Information")),
           checkboxInput("checkbox", label = "Pre-Injury history of incarcaration?", value = TRUE),
           checkboxInput("checkbox", label = "Pre-Injury psych hospitalization/institutionalization?", value = TRUE),
           checkboxInput("checkbox", label = "History of neurodegenerative disease?", value = TRUE))

  ),
)

shinyApp(ui, server)

