#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyWidgets)
library(randomForest)
library(dplyr)
library(DT)
library(caret)

ui <- fluidPage(
  titlePanel("Stroke Risk Assessment"),
  
  sidebarLayout(
    div(style = "background-color: lightblue;",
        sidebarPanel(
          selectInput("gender", "Gender", c("Male", "Female")),
          numericInput("age", "Age", 50),
          selectInput("hypertension", "Hypertension", c("Yes", "No")),
          selectInput("heart_disease", "Heart Disease", c("Yes", "No")),
          selectInput("ever_married", "Ever Married", c("Yes", "No")),
          selectInput("work_type", "Work Type", c("Private", "Self-employed", "children", "Govt_job", "Never_worked")),
          selectInput("residence_type", "Residence Type", c("Urban", "Rural")),
          numericInput("avg_glucose_level", "Avg Glucose Level", 120),
          numericInput("bmi", "BMI", 25),
          selectInput("smoking_status", "Smoking Status", c("smokes", "formerly smoked", "never smoked")),
          div(style = "text-align: center;",
              actionButton("predict_button", "Predict Probability", style = "background-color: green; color: white; font-size: 120%; font-weight: bold;")
          )
        )
    ),
    mainPanel(
      fluidPage(
        tabsetPanel(
          tabPanel("Details & Comments", 
                   icon = icon("comment"),
                   HTML("<b>"),
                   h2("Input Values"),
                   HTML("</b>"),
                   hr(),
                   p(h4("Gender: "), fluidRow(column(3,textOutput("gender_display")))),
                   p(h4("Age: "), textOutput("age_display")),
                   p(h4("Hypertension: "), textOutput("hypertension_display")),
                   p(h4("Heart Disease: "), textOutput("heart_disease_display")),
                   p(h4("Ever Married: "), textOutput("ever_married_display")),
                   p(h4("Work Type: "), textOutput("work_type_display")),
                   p(h4("Residence Type: "), textOutput("residence_type_display")),
                   p(h4("Avg Glucose Level: "), textOutput("avg_glucose_level_display")),
                   p(h4("BMI: "), textOutput("bmi_display")),
                   p(h4("Smoking Status: "), textOutput("smoking_status_display"))
          ),
          
          tabPanel("Results & Suggestion", 
                   icon = icon("square-poll-vertical"),
                   div(style = "text-align:center",h3("Probability and Risk of Stroke:")),
                   div(style = "text-align:center; background: lightblue; padding: 10px;",
                       HTML("<h3><b>"),
                       textOutput("stroke_prob"),
                       HTML("</b>"),
                       HTML("<h3><b>"),
                       textOutput("risk"),
                       HTML("</b>")
                   ),
                   fluidRow(
                     column(1),
                     column(10,
                            shiny::HTML("<br><center><h4>Stroke Prevention for low-risk patient</h4></center><br>"),
                            shiny::HTML("<h5><br><b>Regular exercise</b><br>
                       <br>-Regular exercise can lower bad cholesterol levels in the blood, which can reduce the risk of clogged arteries and stroke.<br> 
                       <br><b>Maintaining a healthy diet</b><br>
                       <br>-Maintaining a healthy diet is important for stroke patients because certain dietary factors can increase the risk of stroke. A diet that is high in saturated and trans fats, cholesterol, and salt can contribute to high blood pressure, high cholesterol, and obesity, which are all risk factors for stroke.<br>
                       <br><b>Limiting alcohol consumption</b><br>
                       <br>-Consuming excessive amounts of alcohol can lead to weight gain, which can increase the risk of stroke by increasing the risk of obesity, high blood pressure, and diabetes.<br></h5>"),
                            shiny::HTML("<br><center><h4>Suggestions for low-risk patient</h4></center><br>"),
                            shiny::HTML("<h5><br>Regular check-ups with the healthcare professional to monitor the blood pressure and cholesterol<br><h5>"),
                            shiny::HTML("<br><center><h4>Stroke Prevention for moderate-risk patient</h4></center><br>"),
                            shiny::HTML("<h5><br><b>Medications to lower blood pressure and cholesterol</b><br>
                                    <br>-Hypertension may damage the vessels in brain and making them more prone to blockage while high cholesterol will increase the risk of blood clots.<br>
                                    <br><b>Not smoking</b><br><br>- Consuming excessive amounts of alcohol can lead to weight gain, which can increase the risk of stroke by increasing the risk of obesity, high blood pressure, and diabetes.<br>
                                   <br><b>Regular exercise</b><br>
                                   <br>- Regular exercise can lower bad cholesterol levels in the blood, which can reduce the risk of clogged arteries and stroke.<br>
                                   <br><b>Maintaining a healthy diet</b><br>
                                   <br>- Maintaining a healthy diet is important for stroke patients because certain dietary factors can increase the risk of stroke. A diet that is high in saturated and trans fats, cholesterol, and salt can contribute to high blood pressure, high cholesterol, and obesity, which are all risk factors for stroke.<br>
                                   </h5>"),
                            shiny::HTML("<br><center><h4>Suggestion for moderate-risk patient</h4></center><br>"),
                            shiny::HTML("<h5>
                                   <br>It is generally recommended that men should limit their alcohol intake to no more than 2 drinks per day, and women should limit their alcohol intake to no more than 1 drink per day. A standard drink is defined as 12 ounces of beer, 5 ounces of wine, or 1.5 ounces of distilled spirits.It is generally recommended that men should limit their alcohol intake to no more than 2 drinks per day, and women should limit their alcohol intake to no more than 1 drink per day. A standard drink is defined as 12 ounces of beer, 5 ounces of wine, or 1.5 ounces of distilled spirits.<br>
                                   <br>Regular check-ups with the healthcare professional to monitor any side effects, adjust the medications accordingly<br>
                                   <br>Medications should be taken with the consults of professional to keep track of their blood pressure and cholesterol levels to make sure that they are within healthy range.<br></h5>"),
                            shiny::HTML("<br><center><h4>Stroke Prevention for high-risk patient</h4></center><br>"),
                            shiny::HTML("<h5><br><b>Regular exercise</b><br>
                                   <br>- Regular exercise can lower bad cholesterol levels in the blood, which can reduce the risk of clogged arteries and stroke.<br>
                                   <br><b>Maintaining a healthy diet</b><br>
                                   <br>- Maintaining a healthy diet is important for stroke patients because certain dietary factors can increase the risk of stroke. A diet that is high in saturated and trans fats, cholesterol, and salt can contribute to high blood pressure, high cholesterol, and obesity, which are all risk factors for stroke.<br>
                                   <br><b>Not smoking</b><br>
                                   <br>- Smoking can increase the risk of blood clots, which can then travel to the brain and cause a stroke.<br>
                                   <br><b>Limiting alcohol consumption</b><br>
                                   <br>- Consuming excessive amounts of alcohol can lead to weight gain, which can increase the risk of stroke by increasing the risk of obesity, high blood pressure, and diabetes.<br>
                                   </h5>"),
                            shiny::HTML("<br><center><h4>Suggestion for High-risk patient</h4></center><br>"),
                            shiny::HTML("<h5><br><b>Rehabilitation</b><br>
                                   <br>Physical, occupational, and speech therapy can help stroke patients regain as much function as possible and improve their quality of life.<br>
                                   <br><b>Medications</b><br>
                                   <br>Medications such as anticoagulants, antiplatelets, and thrombolytics may be used to help prevent another stroke and manage symptoms such as pain, spasticity, and depression.<br>
                                   <br><b>Monitoring</b><br>
                                   <br>Regular monitoring of vital signs, blood tests, and other medical evaluations to detect and manage any complications that may arise.<br>
                                   <br><b>Learn about stroke</b><br>
                                   <br>Understanding the causes, symptoms, and consequences of stroke can help stroke survivors and their families better cope with the condition.<br>
                                   <br><b>Stay active</b><br>
                                   <br>Staying active, both physically and mentally, can help stroke survivors maintain their strength, flexibility, and cognitive abilities.<br>
                                   </h5>"),
                            
                     ),
                     
                   ),
                   column(1)
          ),
          
          tabPanel("Dataset", 
                   icon = icon("database"),
                   DT::dataTableOutput("mytable")),
          
          tabPanel("Summary", 
                   icon = icon("book"),
                   h3("Summary"),
                   verbatimTextOutput("summary")),
          
          tabPanel("About",
                   icon = icon("info"),
                   fluidRow(
                     column(2),
                     column(8,
                            shiny::HTML("<br><center><h1>Stroke Risk Assessment</h1> </center><br>"),
                            shiny::HTML("<h3>This is a Shiny app in R that aims to predict the probability of stroke for a given individual. The app takes in user input for various demographic and health-related factors such as gender, age, hypertension, heart disease, and smoking status. The app then uses a random forest model to predict the probability of stroke, which is trained on a dataset. </h3>")
                     ),
                     column(3)
                   ),
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # load the dataset
  datacsv <- read.csv("Cleaned_Stroke_Latest.csv",stringsAsFactors = F)
  datacsv$stroke <- as.factor(datacsv$stroke)
  datacsv$gender <- as.factor(datacsv$gender)
  datacsv$hypertension <- as.factor(datacsv$hypertension)
  datacsv$heart_disease <- as.factor(datacsv$heart_disease)
  datacsv$ever_married <- as.factor(datacsv$ever_married)
  datacsv$work_type <- as.factor(datacsv$work_type)
  datacsv$Residence_type <- as.factor(datacsv$Residence_type)
  datacsv$smoking_status <- as.factor(datacsv$smoking_status)
  
  
  updateSelectInput(session, "gender", choices = unique(datacsv$gender))
  updateSelectInput(session, "hypertension", choices = unique(datacsv$hypertension))
  updateSelectInput(session, "heart_disease", choices = unique(datacsv$heart_disease))
  updateSelectInput(session, "ever_married", choices = unique(datacsv$ever_married))
  updateSelectInput(session, "work_type", choices = unique(datacsv$work_type))
  updateSelectInput(session, "residence_type", choices = unique(datacsv$Residence_type))
  updateSelectInput(session, "smoking_status", choices = unique(datacsv$smoking_status))
  
  
  
  # train the random forest model on the loaded dataset
  set.seed(123)
  rf_model <- randomForest::randomForest(stroke ~ ., data = datacsv, importance = TRUE, ntree = 2000)
  
  # reactive function to create a dataframe from the user input
  new_data <- reactive({
    data.frame(
      gender = input$gender,
      age = input$age,
      hypertension = input$hypertension,
      heart_disease = input$heart_disease,
      ever_married = input$ever_married,
      work_type = input$work_type,
      Residence_type = input$residence_type,
      avg_glucose_level = input$avg_glucose_level,
      bmi = input$bmi,
      smoking_status = input$smoking_status
    )
  })
  
  # render the input values in the "Details & Comments" tabPanel
  output$gender_display <- renderText({input$gender})
  output$age_display <- renderText({input$age})
  output$hypertension_display <- renderText({input$hypertension})
  output$heart_disease_display <- renderText({input$heart_disease})
  output$ever_married_display <- renderText({input$ever_married})
  output$work_type_display <- renderText({input$work_type})
  output$residence_type_display <- renderText({input$residence_type})
  output$avg_glucose_level_display <- renderText({input$avg_glucose_level})
  output$bmi_display <- renderText({input$bmi})
  output$smoking_status_display <- renderText({input$smoking_status})
  
  observeEvent(input$predict_button, {
    # extract the dataframe from reactive function
    newdatas <- new_data()
    newdatas$gender <- factor(newdatas$gender, levels = levels(datacsv$gender))
    newdatas$hypertension <- factor(newdatas$hypertension, levels = levels(datacsv$hypertension))
    newdatas$heart_disease <- factor(newdatas$heart_disease, levels = levels(datacsv$heart_disease))
    newdatas$ever_married <- factor(newdatas$ever_married, levels = levels(datacsv$ever_married))
    newdatas$work_type <- factor(newdatas$work_type, levels = levels(datacsv$work_type))
    newdatas$Residence_type <- factor(newdatas$Residence_type, levels = levels(datacsv$Residence_type))
    newdatas$smoking_status <- factor(newdatas$smoking_status, levels = levels(datacsv$smoking_status))
    ##datacsv <- datacsv %>% select(-stroke)
    newdatas$age <- as.numeric(as.character(newdatas$age))
    newdatas$avg_glucose_level <- as.numeric(as.character(newdatas$avg_glucose_level))
    newdatas$bmi <- as.numeric(as.character(newdatas$bmi))
    
    # predict the probability of stroke
    predicted_probs <- predict(rf_model, newdatas, type = "prob")
    print(predicted_probs)
    print(datacsv$stroke)
    print(confusionMatrix(predicted_probs,datacsv$stroke))
    
    # Extract the probability of stroke from the returned probs
    stroke_prob <- min(max(predicted_probs[1,2],0),1)
    stroke_prob_pct <- stroke_prob*100
    if(stroke_prob_pct > 100){stroke_prob_pct <- 100}
    if (stroke_prob_pct >= 0 && stroke_prob_pct <= 20) {
      printwrite <- "Low Risk"
    } else if (stroke_prob_pct > 20 && stroke_prob_pct <= 50) {
      printwrite <- "Medium Risk"
    } else if (stroke_prob_pct > 50 && stroke_prob_pct <= 100) {
      printwrite <- "High Risk"
    }
    
    # render the output in the main panel
    # Update the text output with the predicted probability
    output$stroke_prob <- renderText(paste0(stroke_prob_pct,"%"))
    output$risk <- renderText(printwrite)
  })
  
  output$mytable <- DT::renderDataTable({
    datacsv
  })
  
  output$summary <- renderPrint({
    summary(datacsv)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)