library(shiny)
library(DT)
ui <-  fluidPage(
  
# title and author information
  titlePanel(h1(strong("Predicting seizure events in moderate-to-severe TBI"))),
  titlePanel(h3("Arvon Clemens, Felix Proessl, Dominic DiSanto")),
  
# section for input of information/data
sidebarLayout(
  sidebarPanel(
    
    
      # Imaging Inputs (SDH, EDH, CT Fragment)    
           h2(strong("Input Dashboard")),
           h3(strong("Imaging Data")),
           checkboxInput("sdh", label = "Subdural Hematoma?", value = NULL),
           checkboxInput("ctfrag", label = "Intracranial Fragments?", value = NULL),
           checkboxInput("edh", label = "Epidural Hematoma?", value = NULL),

           
      # CT Dropdown
           selectInput("contusion_num", h3(strong("Number of Contusions:")),
                       c("1" = 1,
                         "2" = 2,
                         "3" = 3,
                         "4+" = 4)),
           
           
     # Hospitalization/Injury Data (injury severity [PTA, TTFC, GCS], Alcohol, Acute Seizures, Craniotomy/Craniectomy)
           h3(strong("Injury/Hospitalization Information")),
           checkboxInput("pta", label = "Post-traumatic amnesia > 24h?", value = NULL),
           checkboxInput("gcs", label = "GCS < 8?", value = NULL),
           checkboxInput("ttfc", label = "Days to follow commands > 7?", value = NULL),
           
           checkboxInput("etoh", label = "Alcohol present at time of hospitalization?", value = NULL),
           checkboxInput("acute_sz", label = "Acute seizures?", value = NULL),
           checkboxInput("craniotomy", label="Craniotomy?", value=NULL),
           checkboxInput("craniectomy", label="Craniectomy?", value=NULL),
           
     # Pre-injury medical history information (Incarceration, Psych Hospitalization, Neurodegenerative Disease)
           h3(strong("Pre-Injury Information")),
           checkboxInput("incarcerate", label = "Pre-Injury history of incarcaration?", value = NULL),
           checkboxInput("psychhosp", label = "Pre-Injury psych hospitalization/institutionalization?", value = NULL),
           checkboxInput("neurodegen", label = "History of neurodegenerative disease?", value = NULL)),

# mainPanel: output section for tabular and plotted results
  mainPanel(
    textOutput("placeholder"),
    dataTableOutput("predicted_prob"))  
    
    ))


server= function(input, output) { 
  
  output$predicted_prob <- renderDataTable({ 
  datatable(data.frame(`Predicted Probability` = 1 / (1 + exp(-(input$incarcerate*0.555 + input$acute_sz*1.352 + input$craniectomy*1.197 + input$ctfrag*0.748 +
                     input$etoh*0.484 + input$sdh*0.465 + input$psychhosp*.434 + input$craniotomy*0.399 +
                     input$neurodegen*0.389 + input$edh*0.313)
                     )), 
             Threshold=0.1546,
             Sensitivity=0.75456,
             Specificity=0.6008), editable = T, rownames = F, options = list(lengthChange=FALSE, 
                                                                             sDom  = '<"top">lrt<"bottom">ip'))
  })
  
  
  output$placeholder <- renderText({
    "Placeholder text. We will have other tables and output here"
  })

}


shinyApp(ui, server)
