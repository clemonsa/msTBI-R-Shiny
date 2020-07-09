# Seizure Event Prediction Following Brain Injury
# Originally written April 2020 by Dominic DiSanto, Arvon Clemens, Felix Proessl as a Final Project for Advanced R Computing 
# Currently updated/maintained by: Dominic DiSanto (jdominicdisanto@gmail.com)
# Last Updated: 7/7/2020
# Version: 0.7

# For more information, please visit 
# https://github.com/domdisanto/PostTBI_SeizurePrediction_App/blob/master/README.md 


#////////////////////////////////////////////////////////////////////////////////////////////////////////////#

# Loading Packages
  library(shiny)
  library(magrittr)
  library(shinyjs)
  library(DT)
  library(ggplot2)
  library(ggthemes)
  library(plotly)
  library(shinythemes)


#///////////////////////////////////////////////////////////////////////////#

# Behind the Scenes Functions & Data Import -------------------------------

#///////////////////////////////////////////////////////////////////////////#


  # Importing Data
    phats <- read.csv("Sparse_GLM_Phat_DeID.csv")
    phats_graph <- phats %>% mutate(`Seizure Event Status` = ifelse(Observed_Event==1, "Seizure Event", "No Seizure Event"))
    
    phats_yr1 <- read.csv("Sparse_GLM_Phat_Yr1_DeID.csv")
    phats_yr1_graph <- phats_yr1 %>% mutate(`Seizure Event Status` = ifelse(Observed==1, "Seizure Event", "No Seizure Event"))

    phats_event <- phats[phats$Observed_Event==1,]
    phats_nonevent <- phats[phats$Observed_Event==0,]
    
    phats_yr1_event <- phats_yr1_graph[phats_yr1_graph$Observed==1,]
    phats_yr1_nonevent <- phats_yr1_graph[phats_yr1_graph$Observed==0,]
    
    # Calculating Phat and Calculating Status
    pred_prob_func <- function(input) {
      pred_prob <- 1 / (1 + exp(-(-3.162 + input$incarcerate*0.555 + input$acute_sz*1.352 + input$craniectomy*1.197 + input$ctfrag*0.748 +
                                    input$etoh*0.484 + input$sdh*0.465 + input$psychhosp*.434 + input$craniotomy*0.399 +
                                    input$neurodegen*0.389 + input$edh*0.313 + max(input$gcs, input$ttfc, input$pta)*0.289 + 
                                    (input$contusion_num=="1")*0.248 + (input$contusion_num=="2")*0.303 + (input$contusion_num=="3")*0.519 + (input$contusion_num=="4")*0.523)))  
      classification <- ifelse(pred_prob >= input$thresholdslider, "PTS Event", "No Event")
      return(list(pred_prob=pred_prob, classification=classification))
    }
    
    pred_prob_func_yr1 <- function(input) {
      
    }
    
  # Determining theoretical patient quantile
    quantile_fun <- function(input=NULL, value=NULL) {
      if((is.null(input) & is.null(value)) | (!is.null(input) & !is.null(value))) stop("Please enter information for either only input OR value. Not both")
      if(is.null(input)) pred_prob_val <- value
      if(is.null(value)) pred_prob_val <-  pred_prob_func(input)$pred_prob
      
      total_percent <- nrow(phats[phats[,"Phat"] < pred_prob_val,]) / nrow(phats)  
      pte_percent <- nrow(phats_event[phats_event[,"Phat"] < pred_prob_val,]) / nrow(phats_event)  
      nonpte_percent <- nrow(phats_nonevent[phats_nonevent[,"Phat"] < pred_prob_val,]) / nrow(phats_nonevent)  
      
      return(list(total_percent=total_percent, pte_percent=pte_percent, nonpte_percent=nonpte_percent))
    }
    
    
    # Determining threshold specific sensitivity & specificity manually using only the phat file
    model_per <- function(input) {
      new_sens <- round(sum((phats$Phat > input$thresholdslider) & phats$Observed_Event==1) / (sum((phats$Phat > input$thresholdslider) & phats$Observed_Event==1) + 
                                                                                           sum((phats$Phat < input$thresholdslider) & phats$Observed_Event==1)), 3) # TP / (TP + FN) 
      new_spec <- round(sum((phats$Phat < input$thresholdslider) & phats$Observed_Event==0) / (sum((phats$Phat < input$thresholdslider) & phats$Observed_Event==0) + 
                                                                                           sum((phats$Phat > input$thresholdslider) & phats$Observed_Event==0)), 3) # TN / (TN + FP) 
      # sens automatic using perform_mat    # round(perform_mat[which.min(abs(perform_mat$threshold-input$thresholdslider)),3], 4)
      new_ppv <- round(sum((phats$Phat > input$thresholdslider) & phats$Observed_Event==1) / (sum((phats$Phat > input$thresholdslider) & phats$Observed_Event==1) + 
                                                                                          sum((phats$Phat > input$thresholdslider) & phats$Observed_Event==0)), 3) # TP / (TP + FP) 
      new_npv <- round(sum((phats$Phat < input$thresholdslider) & phats$Observed_Event==0) / (sum((phats$Phat < input$thresholdslider) & phats$Observed_Event==0) + 
                                                                                          sum((phats$Phat < input$thresholdslider) & phats$Observed_Event==1)), 3) # TN / (TN + FN) 
      return(list(sens=new_sens, spec=new_spec, ppv=new_ppv, npv=new_npv))
    }
      
      


#/////////////////////////////////////////////#
###### User Interface & Input Section ########
#/////////////////////////////////////////////#




ui <-  fluidPage(
         navbarPage("Seizure Prediction Following TBI", theme=shinytheme("lumen"),
   
           tabPanel("Instructional", fluid=T, 
                  
                   h2(strong("Predicting Seizure Events in Individuals with Moderate-to-Severe Traumatic Brain Injury")),
                       h3(strong("Instructions")),
                         h4(em("Under Construction")),
                          
                        h3(strong("Background")),
                          h4("The present application uses results from Amy Wagner's research group from the University of Pittsburgh School of Medicine's Department of Physical Medicine & Rehabilitation.
                          The prediction model used comes from the unpublished work \"A Prognostic Model of Seizure Events Following Moderate-to-Severe Traumatic Brain Injury\" (Breslin et al, 2020; In preparation)."),
                            
                          h4("The prediction calculation is derived from a group LASSO derived logistic regression, fit in a cohort of over 4,000 individuals with moderate-to-severe traumatic brain injury, who enrolled in the 
                              Traumatic Brain Injury Model Systems from 2012 to 2017. This page contains information related to the model development, input data, and the the required data necessary to apply this tool to", 
                              em("your specific patient.")), 
                    
                          
                        h3(strong("Abbreviations:")),
                          h4(strong("CT"), " = Computed Tomography", br(),
                             strong("EDH"), " = Epidural Hematoma", br(), 
                             strong("GCS"), " = Glasgow Coma Scale", br(), 
                             strong("PPV"), " = Positive Predictive Value", br(),
                             strong("PTS"), " = Post-Traumatic Seizures", br(),
                             strong("SDH"), " = Subdural Hematoma", br(),
                             strong("TBI"), " = Traumatic Brain Injury"),
                                
                        h3(strong("Variable Descriptions & Data Sources:")),
                          h4(em("Below is a brief summary of the information required for the calculation of an individual's predicted probability and possible sources from which the data can be obtained. 
                                 The specific variables and their assessment via patient interview and/or medical record review are more thoroughly discussed on the ", 
                                 a("TBIMS website.", href="https://www.tbindsc.org/Syllabus.aspx"))),
                   
                    # Variables & Data Sources 
                      datatable(data.frame(
                        Variable = c("SDH", "Intracranial Fragments", "EDH", "Contusions",
                                      "Post-Traumatic Amnesia", "GCS", "Time to Follow Commands", "Alcohol at Hospitalization", "Acute Seizures", "Craniotomy/Craniectomy",
                                      "Pre-Injury History of Incarceration", "Pre-Injury History of Psychiatric Hospitalization", "Pre-Injury Neurodegenerative Disease"),
                        `Data Source` = c("Review of CT imaging and/or radiologist summary report", # SDH
                                          "Review of CT imaging and/or radiologist summary report; Alternatively present of penetrating brain injury suffices as evidence of present fragments", # Intracranial Fragments
                                          "Review of CT imaging and/or radiologist summary report", # EDH
                                          "Review of CT imaging and/or radiologist summary report; Presence of 4 or more contusions (even if total number is unknown) are included in the \"4+\" category ", # Contusions
                                          "Patient or physician report", # PTA
                                          "Patient or physician report", # GCS
                                          "Patient or physician report", # TTFC
                                          "ICD-9 Coding", # ETOH/Alcohol
                                          "ICD-9 Coding", # Acute Seizures
                                          "ICD-9 Coding", # ETOH/Alcohol
                                          "ICD-9 Coding", # ETOH/Alcohol
                                          "ICD-9 Coding", # ETOH/Alcohol
                                          "ICD-9 Coding" # ETOH/Alcohol
                        )),
                                          
                        rownames = F, options=list(dom='t', ordering=F, headerCallback = DT::JS(
                          "function(thead) {",
                          "  $(thead).css('font-size', '25px');",
                          "}"))) %>% 
                          formatStyle(columns = 1, fontWeight = 'bold') %>% 
                          formatStyle(columns = c(1:2), `font-size`="17px")
                    

                    
                    
                 ),
       
 
 tabPanel("Baseline Prediction Model", fluid=TRUE, icon=icon("chart-bar"),
  # Title and Headers
  h1(strong("Predicting Seizure Events in Moderate-to-Severe Traumatic Brain Injury")),
  h2(strong("Baseline Model")),
  div(style="vertical-align:top; width: 100px;",HTML("<br>")),
  
  sidebarLayout(
    
    
    # Sidebar (Input Section)
    sidebarPanel(
      # Imaging Inputs (SDH, EDH, CT Fragment)    
      h2(strong("Input Dashboard")),
      shinyjs::useShinyjs(),
      id = "side-panel",
      
      actionButton("reset_button_yr1", "New Patient/Reset to Defaults"),
      
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
      h3(strong("Tabular Summary")),
      tags$head((tags$style("#tabular_header{font-size:22px; font-weight: bold;}"))),
      
      h4(em(strong("Patient Level Results"))),
      dataTableOutput("patient_table"),
      h4(em(strong("Model Performance Information"))),
      dataTableOutput("model_table"),
      div(style="vertical-align:top; width: 100px;",HTML("<br>")),
      sliderInput("thresholdslider", "Manually Set Classification Threshold", min=0, max=1,value = 0.110563, width = "600px", round = -4, step = 0.001),
      actionButton("reset_threshold", "Reset to Default Classification Threshold"),
      div(style="vertical-align:top; width: 100px;",HTML("<br>")),
      
      div(style="vertical-align:top; width: 100px;",HTML("<br>")),
      h3(strong("Visual Summary")),
      tags$head((tags$style("#graph_header{font-size:22px; font-weight: bold;}")))#,
      
      # plotlyOutput('int_plot')
      
    ) # closure to mainPanel(
  ), # closure to sidebarLayout(    
  
  
  
  # Footer Equations/Notes
  hr(),
  div(style="vertical-align:top; width: 100px;",HTML("<br>")),
  h3("Calculations & Analytic Background"),
  tags$head((tags$style("#calculation_header{font-size:22px; font-weight: bold;}"))),
  
  div(style="vertical-align:top; width: 100px;",HTML("<br>")),
  h4("Logistic regression formulation of the log odds-ratio (or logit):"),
  tags$head((tags$style("#logit_formula_text{font-size:16px;}"))),
  
  
  tabPanel("Diagnostics", 
           uiOutput("logit_formula")),
  
  div(style="vertical-align:top; width: 100px;",HTML("<br>")),
  h4("Individual predicted probability values then calculated using the following formula:"),
  tags$head((tags$style("#phat_text{font-size:16px;}"))),
  
  
  tabPanel("Diagnostics", 
           uiOutput("pred_prob_calc"))
  
  ) # end to second tabpanel
 ) # closure to entire tabsetpanel
) # closure to fluid page(


#//////////////////////////////////////////////////////#
########### Server and Output Information ############# 
#//////////////////////////////////////////////////////#

server= function(input, output) { 
  
  # Background Text/Formulae
  output$logit_formula <- renderUI({
    withMathJax(paste("$$logit(\\hat{p}) = -0.3162 + (TBI Severity) \\times 0.289 + (EDH) \\times 0.313 +
                       (Neurodegenerative Disorder) \\times 0.386 + (Craniotomy) \\times 0.399 + (Psych Hospitalization) \\times 0.434 + 
                       (SDH) \\times 0.465 + (Alchol Presence) \\times 0.484 +$$
                       \n
                       $$(Contusions (1)) \\times 0.248 + (Contusions (2)) \\times 0.303 +
                       (Contusions (3)) \\times 0.519 + (Contusions (4+)) \\times 0.523 + (Incarceration) \\times 0.555 + 
                       (Intracranial Fragments) \\times 0.758 + (Craniectomy) \\times 1.197 + (Acute Seizures) \\times 1.352$$"))
  })
  

  
  
  output$pred_prob_calc <- renderUI({
    withMathJax(paste("$$\\hat{p} = \\frac{1}{1+exp(-logit(\\hat{p}))}$$"))
  })
  
  
  # Table 
  output$patient_table <- renderDataTable({ 
    datatable(data.frame(
      `PTS Prediction` = pred_prob_func(input)$classification,
      `Predicted Probability` = round(pred_prob_func(input)$pred_prob, 3),
      Threshold = input$thresholdslider,
      `Total Percentile` = round(quantile_fun(input=input)$total_percent, 4)*100,
      `PTS Percentile` = round(quantile_fun(input=input)$pte_percent, 4)*100,
      `No PTS Percentile` = round(quantile_fun(input=input)$nonpte_percent, 4)*100),
      rownames = F, 
      options = list(dom = 't', 
                     ordering=F,
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().container()).css({'font-size': '", "17px", "'});"),
        "}"))) %>% formatStyle(columns = c(1:2), fontWeight = 'bold')
  })
  
  output$model_table <- renderDataTable({ 
    datatable(data.frame(
      Sensitivity = model_per(input)$sens,
      Specificity = model_per(input)$spec, 
      PPV = model_per(input)$ppv,
      NPV = model_per(input)$npv),
      rownames = F, 
      options = list(dom = 't', 
                     ordering=F,
        initComplete = htmlwidgets::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().container()).css({'font-size': '", "17px", "'});"),
          "}")))
    })
  
  observeEvent(input$reset_button, {
    shinyjs::reset("side-panel")
  })
  
  observeEvent(input$reset_threshold, {
    shinyjs::reset("thresholdslider")
  })
  
  
  # Graphics/Visualization
  
  output$graph_header <- renderText({
    "Visual Summary"
  })
  
  
  output$int_plot <- renderPlotly({
    
    phats_graph$`Total Percentile` <- unlist(t(sapply(phats_graph$Phat, function(x) quantile_fun(value=x)))[,1])
    phats_graph$`PTS Percentile` <- unlist(t(sapply(phats_graph$Phat, function(x) quantile_fun(value=x)))[,2])
    phats_graph$`No PTS Percentile` <- unlist(t(sapply(phats_graph$Phat, function(x) quantile_fun(value=x)))[,3])
    
    int_plot <- ggplot(phats_graph, aes(pts_per=`No PTS Percentile`)) + geom_density(aes(x=Phat, fill = `Seizure Event Status`), alpha=0.5) +
      geom_vline(xintercept = input$thresholdslider, linetype = 'dashed') +
      geom_vline(xintercept = pred_prob_func(input)$pred_prob) +
      xlab('Threshold Percentage') + ylab('Density') +
      theme_minimal() + scale_fill_manual(values=c("#5D3A9B", "#E66100"), name="")
    
    
    ggplotly(int_plot, tooltip=c("x", "pts_per"))
    
  })
  
}


shinyApp(ui, server)
