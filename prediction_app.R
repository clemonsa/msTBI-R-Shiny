library(shiny)
library(magrittr)
library(shinyjs)
library(DT)

##############################################
###### User Interface & Input Section ########
##############################################

ui <-  fluidPage(
  
  
# Title and Headers
  titlePanel(h1(strong("Predicting Seizure Events in Moderate-to-Severe Traumatic Brain Injury"))),
  titlePanel(h3(strong("BIOST 2094: Advanced R Computing\nFinal Presentation Shiny Application"))),
  titlePanel(h3(strong("Arvon Clemens II, Felix Proessl, Dominic DiSanto"))),
  div(style="vertical-align:top; width: 100px;",HTML("<br>")),
  
sidebarLayout(
  
  
  # Sidebar (Input Section)
    sidebarPanel(
      # Imaging Inputs (SDH, EDH, CT Fragment)    
           h2(strong("Input Dashboard")),
           shinyjs::useShinyjs(),
           id = "side-panel",
           actionButton("reset_button", "New Patient/Reset to Defaults"),
           
           h3(strong("Imaging Data")),
           checkboxInput("sdh", label = ("Subdural Hematoma"), value = NULL),
           checkboxInput("ctfrag", label = "Intracranial Fragments", value = NULL),
           checkboxInput("edh", label = "Epidural Hematoma", value = NULL),

           
      # Contusions Dropdown
           selectInput("contusion_num", h3(strong("Number of Contusions:")),
                       c("0" = 0,
                         "1" = 1,
                         "2" = 2,
                         "3" = 3,
                         "4+" = 4)),
           
           
     # Hospitalization/Injury Data (injury severity [PTA, TTFC, GCS], Alcohol, Acute Seizures, Craniotomy/Craniectomy)
           h3(strong("Injury/Hospitalization Information")),
           checkboxInput("pta", label = "Post-Traumatic Amnesia Episode of 24+ Hours", value = NULL),
           checkboxInput("gcs", label = "Best GCS (Within 24 Hours of Injury) of 8 or Less", value = NULL),
           checkboxInput("ttfc", label = "Days to Follow Commands of 7+ Days", value = NULL),
           
           checkboxInput("etoh", label = "Alcohol Present at Time of Hospitalization", value = NULL),
           checkboxInput("acute_sz", label = "Acute Seizures", value = NULL),
           checkboxInput("craniotomy", label="Craniotomy", value=NULL),
           checkboxInput("craniectomy", label="Craniectomy", value=NULL),
           
     # Pre-injury medical history information (Incarceration, Psych Hospitalization, Neurodegenerative Disease)
           h3(strong("Pre-Injury Information")),
           checkboxInput("incarcerate", label = "Pre-Injury History of Incarcaration", value = NULL),
           checkboxInput("psychhosp", label = "Pre-Injury Psychiatric Hospitalization/Institutionalization", value = NULL),
           checkboxInput("neurodegen", label = "Pre-Injury Diagnosis of Neurodegenerative Disease", value = NULL)
     
  ), # closure to sidebarPanel(
  
  
    
  # mainPanel: output section for tabular and plotted results
    mainPanel(
        textOutput("bg_header"),
        tags$head((tags$style("#bg_header{font-size:22px; font-weight: bold;}"))),

        div(style="vertical-align:top; width: 100px;",HTML("<br>")),
        
        textOutput("bg_subheader"),
        tags$head((tags$style("#bg_subheader{font-size:18px; font-style:italic; font-weight: bold;}"))),
        
        textOutput("bg_text"),
        tags$head((tags$style("#bg_text{font-size:16px;}"))),
      
        div(style="vertical-align:top; width: 100px;",HTML("<br>")),
      
        textOutput("instructions_subheader"),
        tags$head((tags$style("#instructions_subheader{font-size:18px; font-style:italic; font-weight: bold;}"))),
        
        textOutput("instructional_text"),
        tags$head((tags$style("#instructional_text{font-size:16px;}"))),
    
        div(style="vertical-align:top; width: 100px;",HTML("<br>")),
        
        textOutput("tabular_header"),
        tags$head((tags$style("#tabular_header{font-size:22px; font-weight: bold;}"))),
        div(style="vertical-align:top; width: 100px;",HTML("<br>")),
        
        dataTableOutput("output_table"),
        div(style="vertical-align:top; width: 100px;",HTML("<br>")),
        sliderInput("thresholdslider", "Manually Set Classification Threshold", min=0, max=1,value = 0.110563, width = "600px", round = -4, step = 0.001),
        actionButton("reset_threshold", "Reset to Default Classification Threshold"),
        div(style="vertical-align:top; width: 100px;",HTML("<br>")),
        
        div(style="vertical-align:top; width: 100px;",HTML("<br>")),
        textOutput("graph_header"),
        tags$head((tags$style("#graph_header{font-size:22px; font-weight: bold;}"))),
        ) # closure to mainPanel(
      ), # closure to sidebarLayout(    


    # Footer Equations/Notes
      hr(),

      div(style="vertical-align:top; width: 100px;",HTML("<br>")),
      textOutput("calculation_header"),
      tags$head((tags$style("#calculation_header{font-size:22px; font-weight: bold;}"))),

      div(style="vertical-align:top; width: 100px;",HTML("<br>")),
      textOutput("logit_formula_text"),
      tags$head((tags$style("#logit_formula_text{font-size:16px;}"))),
      
      
      tabPanel("Diagnostics", 
               withMathJax(uiOutput("logit_formula"))),
      
      div(style="vertical-align:top; width: 100px;",HTML("<br>")),
      textOutput("phat_text"),
      tags$head((tags$style("#phat_text{font-size:16px;}"))),
      
      
      tabPanel("Diagnostics", 
               withMathJax(uiOutput("pred_prob_calc")))

) # closure to fluid page(

#######################################################
############ Behind the Scenes Functions# ############# 
#######################################################


# Importing Data
phats <- read.csv("Sparse_GLM_Phat_DeID.csv")
phats_event <- phats[phats$Observed_Event==1,]
phats_nonevent <- phats[phats$Observed_Event==0,]
perform_mat <- read.csv("Threshold_Performance_Mat.csv")

# Calculating Phat and Calculating Status
    pred_prob_func <- function(input) {
    pred_prob <- 1 / (1 + exp(-(-3.162 + input$incarcerate*0.555 + input$acute_sz*1.352 + input$craniectomy*1.197 + input$ctfrag*0.748 +
                                  input$etoh*0.484 + input$sdh*0.465 + input$psychhosp*.434 + input$craniotomy*0.399 +
                                  input$neurodegen*0.389 + input$edh*0.313 + max(input$gcs, input$ttfc, input$pta)*0.289 + 
                                  (input$contusion_num=="1")*0.248 + (input$contusion_num=="2")*0.303 + (input$contusion_num=="3")*0.519 + (input$contusion_num=="4")*0.523)))  
    classification <- ifelse(pred_prob >= input$thresholdslider, "PTS Event", "No Event")
    return(list(pred_prob=pred_prob, classification=classification))
    }


# Determining theoretical patient quantile
    quantile_fun <- function(input) {
      total_percent <- nrow(phats[phats[,"Phat"]<pred_prob_func(input)$pred_prob,]) / nrow(phats)  
      pte_percent <- nrow(phats_event[phats_event[,"Phat"]<pred_prob_func(input)$pred_prob,]) / nrow(phats_event)  
      nonpte_percent <- nrow(phats_nonevent[phats_nonevent[,"Phat"]<pred_prob_func(input)$pred_prob,]) / nrow(phats_nonevent)  
      return(list(total_percent=total_percent, pte_percent=pte_percent, nonpte_percent=nonpte_percent))
    }


# Determining threshold specific sensitivity & specificity 
    model_per <- function(input) {
      new_sens <- round(perform_mat[which.min(abs(perform_mat$threshold-input$thresholdslider)),2], 4)
      new_spec <- round(perform_mat[which.min(abs(perform_mat$threshold-input$thresholdslider)),3], 4)
      return(list(sens=new_sens, spec=new_spec))
    }

    
  
#######################################################
########### Server and Output Information ############# 
#######################################################

server= function(input, output) { 
  
# Background Text/Formulae

  
  output$bg_header <- renderText({
    "Instructions & Relevant Information"
  })
  
  output$bg_subheader <- renderText({
    "Background"
  })
  
  output$bg_text <- renderText({
    "The present application uses results from Amy Wagner's research group from the University of Pittsburgh School of Medicine's Department of Physical Medicine & Rehabilitation.
    The prediction model used comes from the unpublished work \"A Prognostic Model of Seizure Events Following Moderate-to-Severe Traumatic Brain Injury\" (Breslin et al, 2020; In preparation)."
  })
  
  output$instructions_subheader <- renderText({
    "Instructions"
  })
  
  output$instructional_text <- renderText({
    "Enter patient information in the sidepanel to the left. Output information is included in the table and graphic below. Reset buttons are included to return default values for new patient input. 
    The classification threshold can be altered using the sliding bar. The default threshold value of 0.111 was chosen to maximize sensitivity for a specificity of greater than or equal to 0.60 (60%)."
  })
  
  output$calculation_header <- renderText({
    "Calculations & Analytic Background"
  })

    
    
    output$logit_formula_text <- renderText({
      "Logistic regresion formulation of the log odds-ratio (or logit):"
    })
    
    
    
  output$logit_formula <- renderUI({
    return(HTML(paste0("<p> $$logit(\\hat{p}) = -0.3162 + (TBI Severity) \\times 0.289 + (EDH) \\times 0.313 +
                       (Neurodegenerative Disorder) \\times 0.386 + (Craniotomy) \\times 0.399 + (Psych Hospitalization) \\times 0.434 + 
                       (SDH) \\times 0.465 + (Alchol Presence) \\times 0.484 + $$
                       \n
                       $$ (Contusions (1)) \\times 0.248 + (Contusions (2)) \\t!imes 0.303 +
                       (Contusions (3)) \\times 0.519 + (Contusions (4+)) \\times 0.523 + (Incarceration) \\times 0.555 + 
                       (Intracranial Fragments) \\times 0.758 + (Craniectomy) \\times 1.197 + (Acute Seizures) \\times 1.352.$$ </p>")))
  })
  
  
  output$phat_text <- renderText({
    "Individual predicted probability values then calculated using the following formula:"
  })
  
  
  output$pred_prob_calc <- renderUI({
    return(HTML(paste0("<p>$$ \\hat{p} = \\frac{1}{1+exp(-logit(\\hat{p}))} $$ </p>")))
    
  })
  
  
# Table 
  output$tabular_header <- renderText({
    "Tabular Summary"
  })
  

  output$output_table <- renderDataTable({ 
  datatable(data.frame(
            `PTS Prediction` = pred_prob_func(input)$classification,
            `Predicted Probability` = round(pred_prob_func(input)$pred_prob, 3),
             Threshold = input$thresholdslider,
            `Total Percentile` = round(quantile_fun(input)$total_percent, 4)*100,
            `PTS Percentile` = round(quantile_fun(input)$nonpte_percent, 4)*100,
            `No PTS Percentile` = round(quantile_fun(input)$pte_percent, 4)*100,
            Sensitivity = model_per(input)$sens,
             Specificity = model_per(input)$spec), 
        rownames = F, 
        options = list(dom = 't', 
                        ordering=F)) %>% formatStyle(columns = c(1:2), fontWeight = 'bold') %>% 
      formatStyle(columns = c(1:8), `font-size`="20px")
    
  })
  

# Graphics/Visualization
  
  output$graph_header <- renderText({
    "Visual Summary"
  })
  
  
  observeEvent(input$reset_button, {
    shinyjs::reset("side-panel")
  })
  
  observeEvent(input$reset_threshold, {
    shinyjs::reset("thresholdslider")
  })
  
}


shinyApp(ui, server)
