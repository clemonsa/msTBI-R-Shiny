library(shiny)
library(magrittr)
library(DT)
library(ggplot2)
library(plotly)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Density Graph Plotly Demo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textOutput("Placeholder"),
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
            
        ), # Closure to sidebarPanel(

        # Show a plot of the generated distribution
        mainPanel( # Threshold Adjustment Slider
           sliderInput('thresholdslider', label = "PTE Density Curve", min = 0.0, max = 1, value = 0.33),
           plotlyOutput('Patients')
        )
    )
)

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

# Define server logic required to draw a histogram
server <- function(input, output) {
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
  
  # Create data.frame of percentiles
  prange <- seq(0, 1, 0.001)
  Percentiles <- data.frame(sapply(prange, quantile_fun))
  
  PTE <- Percentiles[2, ]; PTE <- unlist(PTE); PTE <- data.frame(PTE); colnames(PTE) <- 'percent'
  NPTE <- Percentiles[3, ]; NPTE <- unlist(NPTE); NPTE <- data.frame(NPTE); colnames(NPTE) <- 'percent'
  
  PTE$cat <- 'PTE'; NPTE$cat <- 'No PTE'
  
  Patients <- rbind(PTE, NPTE);
  
  
  output$Patients <- renderPlotly({
    p<- ggplot(Patients, aes(percent, fill = cat )) + geom_density(alpha = 0.2) +
      geom_vline(xintercept = input$thresholdslider, linetype = 'dashed') +
      theme(legend.title = element_blank()) + 
      xlab('Threshold Percentage') + ylab('Density') + ggtitle('Post-Traumatic Seizures Risk')
    ggplotly(p)             
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
