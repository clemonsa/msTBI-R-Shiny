library(shiny)

ui <-  fluidPage(
  titlePanel(h1(strong("Predicting seizure events in moderate-to-severe TBI"))),
  titlePanel(h3("Arvon Clemens, Felix Proessl, Dominic DiSanto")),
  
sidebarLayout(
  sidebarPanel(
    
           h2(strong("Input Dashboard")),
           h3(strong("Imaging Data")),
           checkboxInput("sdh", label = "Subdural Hematoma?", value = NULL),
           checkboxInput("ctfrag", label = "Intracranial Fragments?", value = NULL),
           
           numericInput("contusions",
                        label = h3(strong("Number of present contusions in CT imaging")),
                        value = NULL),
           checkboxInput("contusions_greater4", label = ">4", value = NULL),
           
           
           h3(strong("Injury/Hospitalization Information")),
           checkboxInput("pta", label = "Post-traumatic amnesia > 24h?", value = NULL),
           checkboxInput("gcs", label = "GCS < 8?", value = NULL),
           checkboxInput("ttfc", label = "Days to follow commands > 7?", value = NULL),
           
           checkboxInput("etoh", label = "Alcohol present at time of hospitalization?", value = NULL),
           checkboxInput("acute_sz", label = "Acute seizures?", value = NULL),
           
           
           h3(strong("Pre-Injury Information")),
           checkboxInput("incarcerate", label = "Pre-Injury history of incarcaration?", value = NULL),
           checkboxInput("psychhosp", label = "Pre-Injury psych hospitalization/institutionalization?", value = NULL),
           checkboxInput("neurodegen", label = "History of neurodegenerative disease?", value = NULL))
    
  ,
  mainPanel(
    textOutput("selected_inputs"), 
    textOutput("placeholder"),
    plotOutput("dummy_plot"))  
    
    ))


server= function(input, output) { 
  output$selected_inputs <- renderText({
    paste("You've selected a patient with", input$contusions, "contusion.")
  })
  output$placeholder <- renderText({
  "Placeholder text. We will have other tables and output here"
  })
  }


shinyApp(ui, server)
