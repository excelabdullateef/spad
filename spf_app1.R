####################################
# SPAD - STUDENT PERFORMANCE ANALYTICS DASHBOARD 
# Author: Abdullateef Adedeji
# Description: Predicts students' exam scores based on top 8 factors 
# using a trained Linear Regression Model.
####################################

# Import libraries
library(shiny)
library(caret)
library(tidyverse)
library(plotly)
library(data.table)
library(shinythemes)
library(googlesheets4)
# Read in the model

# Non-interactive authentication for ShinyApps.io
# Ensure your .secrets folder is uploaded with the app
gs4_auth(cache = ".secrets", email = "adedejiabdullateef.a@gmail.com")
sheet_id <- "1BE1yWMpSCvSVSvGSJ3a3E-lSzFKYLfX7NHeNxMMUZE0"
model <- readRDS("spfmodel.rds")
model1 <- readRDS("spfmodel1.rds")
spf <- read_csv("spf2.csv")

####################################
# USER INTERFACE
####################################
ui <- fluidPage(
  title = "SPAD: Student Performance Analytics Dashboard",
  theme = shinytheme("cerulean"), 
  
  
  # Custom CSS for a modern look
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$style(HTML("
    /* 1. Global Header Branding */
    .header-container { 
      padding: 25px 0; 
      background-color: #003366; /* Primary Blue */
      border-bottom: 5px solid #FF8C00; /* Secondary Orange */
      color: white;
      margin-bottom: 30px;
    }
    .main-title h1 { font-weight: bold; color: #FFFFFF; margin: 0; }
    
    /* 2. Tab Navigation - Fixing Visibility */
    .nav-tabs { border-bottom: 2px solid #003366; }
    
    /* Inactive Tabs: Navy Blue text on white background */
    .nav-tabs > li > a {
      color: #003366 !important; 
      font-weight: bold;
      transition: 0.3s;
    }

    /* Active Tab: Growth Orange background with White text */
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:focus, 
    .nav-tabs > li.active > a:hover {
      background-color: #FF8C00 !important; /* Brand Orange */
      color: #FFFFFF !important;           /* FORCE WHITE TEXT */
      border: 1px solid #FF8C00 !important;
      border-bottom-color: transparent;
    }

    /* Hover State for Inactive Tabs */
    .nav-tabs > li > a:hover {
      background-color: #ecf0f1 !important;
      color: #FF8C00 !important;
    }

    /* 3. Action Buttons and Sidebar */
    .btn-primary {
      background-color: #FF8C00 !important;
      border-color: #E67E22 !important;
      font-weight: bold;
    }
    .well { border-left: 5px solid #FF8C00; border-top: 0; }
    h3, h4 { color: #003366; font-weight: bold; }
    
    .footer-style { 
      position: fixed; 
      bottom: 0; 
      left: 0;
      right: 0;
      width: 100%; 
      background-color: #003366; /* Brand Blue */
      border-top: 4px solid #FF8C00; /* Brand Orange */
      color: white; 
      padding: 15px 0; 
      text-align: center;
      font-size: 14px; 
      z-index: 2000; /* High z-index ensures it stays on top */
    }
    
    /* This adds extra space at the bottom of the page so the footer doesn't hide your data */
    body { padding-bottom: 70px; }
"))
  ),

  # App title and short description
  div(class = "header-container",
      fluidRow(
        column(12, align = "center",
               # Ensure logo.png is in your /www folder
               img(src = "logo.png", 
                   height = "150px", 
                   style = "background-color: white; 
             padding: 10 px; 
             border-radius: 12px; 
             box-shadow: 0px 4px 8px rgba(0,0,0,0.2); 
             margin-bottom: 15px;"),
               div(class = "main-title", h1("SPAD: Student Performance Analytics Dashboard"))
        )
      )
  ),
  
  tabsetPanel(
        
        # TAB 1: OVERVIEW & RATIONALE
        tabPanel("Overview",
                 fluidRow(
                   column(8, offset = 2,
                          div(style = "text-align: justify; line-height: 1.7; color: #34495e;",
                              br(),
                              h2("Student Performance Analytics Dashboard (SPAD)", style = "text-align: center; color: #2c3e50; font-weight: bold;"),
                              hr(),
                              
                              h3("1. The Background: A New Era of Learning Analytics"),
                              p("In the traditional classroom, we have long relied on teacher intuition and periodic assessments to gauge student progress. However, as the world enters a data-driven era, education is evolving. This project, developed as a practical exploration within the ", 
                                strong("Growth-minded Educators Academy, "), "explores how raw data can be transformed into actionable classroom intelligence."),
                              p("By analyzing over 6,000 unique student records, we have moved beyond simple spreadsheets to a system that understands the 'why' behind student grades, merging years of pedagogical experience with the precision of Educational Data Science."),
                              
                              
                              
                              h3("2. The Rationale: Solving the 'Lagging Indicator' Problem"),
                              p("The most challenging problem in the education space today is that final exam scores are lagging indicators."),
                              p("By the time a teacher sees a failing grade on a report card, the window for intervention has already closed. SPAD was developed to solve this through 'Predictive Foresight':"),
                              tags$ul(
                                tags$li(strong("From Hindsight to Insight:"), " We move from asking 'What happened?' after the exam to 'What is likely to happen?' weeks in advance, allowing for timely support."),
                                tags$li(strong("The Multi-Factor Reality:"), " Success is a complex interplay of home environment, motivation, and school resources. SPAD weights these factors accurately to give a holistic view of every student."),
                                tags$li(strong("Evidence-Based Decisions:"), " SPAD empowers school leaders and policymakers to allocate resources to the students who need them most, before they fall through the cracks.")
                              ),
                      
                              
                              
                              h3("3. Aim of the Project"),
                              p("The primary aim of SPAD is to provide a user-friendly bridge between Data Science and Pedagogy. We aim to identify key predictors of success and facilitate early, targeted educational interventions."),
                              
                              
                              
                              tags$blockquote(
                                "Data in the hands of a teacher is no longer just numbers—it is a superpower that ensures no student falls through the cracks. — Abdullateef Adedeji", 
                                style = "font-style: italic; border-left: 5px solid #2980b9; background-color: #f4f7f9; padding: 15px; margin-top: 20px;"
                              ),
                              
                             h3("4. How to Use SPAD"),
                             p("Follow these four steps to generate and interpret student analytics:"),
                             tags$ol(
                               
                               tags$li(strong("Step 1: Input Student Parameters:"), " Navigate to the 'Prediction' tab. Use the sliders and menus to enter the details of a student."),
                               tags$li(tags$b("Step 2: Generate the Analytics:"), " Click 'Generate Prediction' to process the data through our validated model."),
                               tags$li(strong("Step 3: Take Action:"), " Use the 'Educator's Action Guide' below the result for suggested interventions based on the predicted risk level.")
                             ),
                              
                              br(), br() 
                          )
                   )
                 )
        ),
      
       
        # TAB 2: Prediction
        tabPanel("Prediction",
                 sidebarLayout(
                   
                   sidebarPanel(
                     h3("Session Details", style = "color: #2c3e50; font-weight: bold;"),
                     textInput("stuName", "Student Full Name:", placeholder = "e.g., Abdullateef Adedeji"),
                     fluidRow(
                       column(6, textInput("stuSubject", "Subject Name:", placeholder = "Mathematics")),
                       column(6, textInput("stuClass", "Class/Grade Level:", placeholder = "SS3")),
                       column(6, textInput("Gend", "Gender:", placeholder = "Male"))
                     ),
                     hr(),
                   
                     tags$h3("Student Profile Parameters", style = "color: #2c3e50; font-weight: bold;"),
                     p("Adjust the settings below based on the student's current status to generate an early-warning prediction."),
                     hr(),
                     
                     # Group 1: Academic & Effort Factors
                     wellPanel(
                       tags$h4("Academic Effort", style = "color: #2980b9;"),
                       sliderInput("Attd", label = "School Attendance Percentage (%)", value = 60, min = 0, max = 100),
                       sliderInput("HrsStd", label = "Weekly Study Hours", value = 20, min = 0, max = 44),
                       sliderInput("PrevSco", label = "Previous Examination Scores", value = 65, min = 0, max = 100),
                       sliderInput("TutSes", label = "Monthly Tutoring Sessions", value = 2, min = 0, max = 8)
                     ),
                     
                     # Group 2: Home & Socio-Economic Support
                     wellPanel(
                       tags$h4("Home and Environmental Support", style = "color: #27ae60;"),
                       selectInput("ParInv", label = "Level of Parental Involvement", choices = c("Low", "Medium", "High")),
                       selectInput("Acc2Rs", label = "Access to Educational Resources", choices = c("Low", "Medium", "High")),
                       selectInput("FamInc", label = "Family Income Level", choices = c("Low", "Medium", "High")),
                       selectInput("ParEdLev", label = "Parental Education Level", choices = c("High School", "College", "Postgraduate"))
                     ),
                     
                     # Group 3: Psychological & Social Factors
                     wellPanel(
                       tags$h4("Motivation & Peers Influence", style = "color: #f39c12;"),
                       selectInput("MotLev", label = "Student Motivation Level", choices = c("High", "Medium", "Low")),
                       selectInput("PerInf", label = "Peer Influence Quality", choices = c("Positive", "Neutral", "Negative"))
                     ),
                     
                     br(),
                     actionButton("submitbutton", "Generate Analytics Report", class = "btn-primary btn-lg", style = "width: 100%; font-weight: bold;")
                   ),
                   
                   # Main Panel: High-Impact Results
                   mainPanel(
                     div(style = "padding: 20px;",
                         tags$h3("Analytics Output", style = "color: #2c3e50; font-weight: bold;"),
                         p("The scores below represent the statistically expected performance based on current data patterns."),
                         br(),
                         
                         # Stylized Prediction Box
                         div(class = "prediction-box", 
                             style = "padding: 30px; border-radius: 15px; background-color: #f8f9fa; border: 2px solid #2980b9; text-align: center;",
                             tags$h4("Predicted Exam Score", style = "margin-bottom: 10px; color: #7f8c8d;"),
                             uiOutput("txtout") # This should render a large, colored number in server.R
                         ),
                         
                         br(),
                         hr(),
                         
                         # New: Teacher Intervention Guide
                         tags$h4("Educator's Action Guide", style = "color: #2c3e50; font-weight: bold;"),
                         p("Based on this prediction, we recommend the following pedagogical focus:"),
                         uiOutput("actionGuide"), # Logic in server to suggest interventions
                         
                         # Reproduced Table
                         tags$h4("Input Summary"),
                         p("The following data was used for this analytics session:"),
                         tableOutput("inputTable"),
                         br(),
                         div(style = "background-color: #f4f7f9; padding: 15px; border-radius: 10px; font-size: 13px; color: #7f8c8d;",
                             tags$h4("Model Technical Integrity:"),
                             tags$p("This predictive engine is powered by a high-precision Linear Regression model validated through 2,000 rigorous cross-validation trials."),
                             tags$p("It analyzes the Top 10 most influential factors identified during our research at the Growth-minded Educators Academy.")
                         )
                     )
                   )
                 )
        ),
        
        # TAB 3: MODEL INFO
        tabPanel("Model Info",
                 fluidRow(
                   column(8, offset = 2,
                          div(style = "text-align: justify; line-height: 1.7; color: #34495e;",
                              br(),
                              h2("Model Integrity & Technical Documentation", style = "text-align: center; color: #2c3e50; font-weight: bold;"),
                              hr(),
                              
                              h3("1. Data Source: The Kaggle Synthetic Foundation"),
                              p("SPAD is powered by a high-quality synthetic dataset obtained from Kaggle, specifically designed to simulate the complex interplay of student performance factors."),
                              tags$ul(
                                tags$li(strong("Dataset Scope:"), " 6,000+ comprehensive records spanning academic effort, home environment, and socio-economic status."),
                                tags$li(strong("Purpose:"), " This data serves as a statistically sound foundation for demonstrating how Learning Analytics can identify early warning signs in a classroom setting."),
                                tags$li(strong("Validation:"), " Despite its synthetic nature, the model has been put through the same rigorous testing protocols used in professional educational research.")
                              ),
                              
                              h3("2. Mathematical Rigor: The 2,000-Trial Standard"),
                              p("As an EdTech specialist, I have prioritized mathematical stability so every prediction is a verified guide for your classroom."),
                              
                              tags$ul(
                                tags$li(strong("Repeated Cross-Validation:"), " The model was put through 2,000 rigorous validation trials (100-fold CV with 20 repeats)."),
                                tags$li(strong("Multicollinearity Check:"), " We utilized Variance Inflation Factor (VIF) analysis to ensure that our 10 predictors are independent, with all VIF values near 1.0, ensuring the mathematical weight of each factor is accurate.")
                              ),
                              
                              h3("3. Understanding the '87% Prediction Cap'"),
                              p("Mathematically, SPAD predicts the statistically expected outcome based on historical patterns. In our dataset, 87% represents the peak 'Baseline Potential' that data alone can reliably explain."),
                              tags$blockquote(
                                "The remaining 13% represents the 'Human Spark'—the unquantifiable impact of a teacher's inspiration, a student's sudden breakthrough, and the unique spark of growth that data cannot capture.",
                                style = "font-style: italic; border-left: 5px solid #2980b9; background-color: #f4f7f9; padding: 15px;"
                              ),
                              
                              hr(),
                              
                              h3("4. Performance Metrics"),
                              fluidRow(
                                # R-Squared
                                column(4,
                                       wellPanel(
                                         style = "border-top: 5px solid #003366; background-color: #f4f7f9; min-height: 250px;",
                                         h4("Explanatory Power (77.7%)", style = "color: #003366;"),
                                         h3("So What?", style = "color: #FF8C00; font-weight: bold;"),
                                         p("This means that ", strong("over 3/4 of a student's success"), " is driven by the 10 factors we track (like study hours and parental support)."),
                                         p("It proves that academic failure isn't random—it's predictable and, more importantly, ", strong("preventable"), ".")
                                       )
                                ),
                                
                                # MAE
                                column(4,
                                       wellPanel(
                                         style = "border-top: 5px solid #003366; background-color: #f4f7f9; min-height: 250px;",
                                         h4("Average Error (0.78)", style = "color: #003366;"),
                                         h3("So What?", style = "color: #FF8C00; font-weight: bold;"),
                                         p("This tells us our 'GPS' is highly accurate. When we predict a score, we are typically off by ", strong("less than 1%"), "."),
                                         p("You can trust these predictions to hold high-stakes conversations with parents and students about their current trajectory.")
                                       )
                                ),
                                
                                # RMSE
                                column(4,
                                       wellPanel(
                                         style = "border-top: 5px solid #003366; background-color: #f4f7f9; min-height: 250px;",
                                         h4("Reliability (1.74)", style = "color: #003366;"),
                                         h3("So What?", style = "color: #FF8C00; font-weight: bold;"),
                                         p("This number proves the model is consistent. It doesn't make 'wild guesses' or huge mistakes."),
                                         p("Whether the student is a high-achiever or at-risk, the model remains stable and reliable across the board.")
                                       )
                                )
                              ),
                              
                              h3("5. The Hierarchy of Success (Variable Importance)"),
                              p("This chart ranks variables by their predictive power, helping you know exactly where to focus your energy for maximum impact."),
                              
                              plotOutput("importancePlot"),
                              br(), br()
                          )
                   )
                 )
        ),
        
        # --- TAB 4: THE TEAM & VISION ---
        tabPanel("The Team & Vision",
                 fluidRow(
                   column(10, offset = 1,
                          div(style = "margin-top: 30px; margin-bottom: 50px;",
                              
                              # SECTION 1: ABOUT THE DEVELOPER
                              div(style = "text-align: center;",
                                  h1("ABDULLATEEF ADEDEJI", style = "color: #003366; font-weight: 900; letter-spacing: 2px;"),
                                  h4("Flipped Classroom Expert | EdTech Strategist | Data Scientist", style = "color: #FF8C00; font-weight: bold;"),
                                  hr(style = "border-top: 3px solid #003366; width: 50%; margin: auto; padding-bottom: 20px;")
                              ),
                              
                              fluidRow(
                                column(6,
                                       h3("Visionary Leadership", style = "color: #003366;"),
                                       p("Abdullateef Adedeji is a visionary educational strategist, author, and the Founder of the ", strong("Growth-minded Educators Academy."), " Widely recognized as 'The Flipped Classroom Expert,' his mission is to revolutionize African pedagogy by equipping educators with the mindset and digital tools necessary to create 21st-century learning environments."),
                                       
                                       h3("Academic Excellence", style = "color: #003366;"),
                                       p("His academic foundation is historic. He emerged as the ", strong("Best Graduating Student from TASUED"), " with a First Class degree in Mathematics, achieving a ", strong("CGPA of 4.91"), "—the highest recorded in the university’s history.")
                                ),
                                column(6,
                                       h3("Professional Impact", style = "color: #003366;"),
                                       p("Abdullateef has trained over 1,000 teachers across Nigeria, Ghana, Cameroon, and the USA. His expertise has earned him features on major platforms including ", strong("Kaftan TV’s DayBreak Africa"), " and ", strong("News Central TV’s Village Square Africa.")),
                                       
                                       h3("Current Research & Service", style = "color: #003366;"),
                                       p("Currently, he is a researcher at ", strong("ICAMMDA"), " (Federal University Oye Ekiti). He simultaneously serves as the ", strong("Overall CDS President for Oye Local Government"), " and President of the DL4ALL group.")
                                )
                              ),
                              
                              br(),
                              hr(),
                              br(),
                              
                              # SECTION 2: ACKNOWLEDGEMENTS (Refined Hierarchy)
                              div(style = "padding: 30px; background-color: #f8f9fa; border-left: 8px solid #FF8C00; border-radius: 10px;",
                                  h2("Gratitude & Acknowledgements", style = "color: #003366; font-weight: bold;"),
                                  p("The development of SPAD is a testament to the power of structured mentorship. I wish to express my deepest appreciation to:", style = "font-size: 16px;"),
                                  
                                  tags$ul(style = "font-size: 16px; line-height: 1.8;",
                                          tags$li(strong("Professor E.A. Bakare:"), " The Leadership of ICAMMDA, for providing the prestigious platform to serve as a Graduate Research Assistant and contribute to high-level mathematical modeling."),
                                          
                                          tags$li(strong("Mr. Olasupo Idowu Isaac:"), " My dedicated Supervisor, whose technical oversight and commitment to excellence have shaped my approach to data analytics."),
                                          
                                          tags$li(strong("The ICAMMDA Team:"), " I am profoundly grateful to the Post-Doctoral Scientist, Administrators, and the vibrant community of PhD and Master Scholars. Your collective expertise and intellectual rigor have created an environment where innovation thrives."),
                                          
                                          tags$li(strong("Mr. Dolapo Bakare:"), " For the foundational introduction to Machine Learning in R. This spark of knowledge served as the technical cornerstone for the SPAD predictive framework."),
                                          
                                          tags$li(strong("My Fellow Interns:"), " For the shared hours of research, the intellectual debates, and the camaraderie that made this journey truly rewarding.")
                                  )
                              ),
                              
                              br(),
                              div(style = "text-align: center; margin-top: 30px;",
                                  a(href = "https://www.linkedin.com/in/abdullateefadedeji", target = "_blank", 
                                    style = "background-color: #003366; color: white; padding: 15px 30px; text-decoration: none; border-radius: 30px; font-weight: bold; border: 2px solid #FF8C00;",
                                    "Connect with the Flipped Classroom Expert")
                              )
                          )
                   )
                 )
        ),
  ),
# Footer
div(class = "footer-style",
    HTML('Developed by <a href="https://www.linkedin.com/in/abdullateefadedeji" target="_blank" style="color:white; text-decoration:none;"><b>Abdullateef Adedeji</b></a> — The Flipped Classroom Expert')
)
)



##########################################
# SERVER
####################################

server <- function(input, output, session) {
    
    # Reactive block for prediction and logging
    prediction_data <- eventReactive(input$submitbutton, {
      
      # Construct DF with exact factor levels
      df <- data.frame(
        HrsStd = as.numeric(input$HrsStd),
        Attd = as.numeric(input$Attd),
        PrevSco = as.numeric(input$PrevSco),
        TutSes = as.numeric(input$TutSes),
        ParInv = factor(input$ParInv, levels = c("Low", "Medium", "High")),
        Acc2Rs = factor(input$Acc2Rs, levels = c("Low", "Medium", "High")),
        FamInc = factor(input$FamInc, levels = c("Low", "Medium", "High")),
        ParEdLev = factor(input$ParEdLev, levels = c("High School", "College", "Postgraduate")),
        MotLev = factor(input$MotLev, levels = c("Low", "Medium", "High")),
        PerInf = factor(input$PerInf, levels = c("Negative", "Neutral", "Positive"))
      )
      
      pred <- round(predict(model, df), 2)
      
      # Log to Google Sheets
      log_entry <- data.frame(
        Timestamp = as.character(Sys.time()),
        Student = input$stuName,
        Subject = input$stuSubject,
        Class = input$stuClass,
        Gender = input$Gend,
        Prediction = pred,
        df # Appends all input variables
      )
      sheet_append(sheet_id, log_entry)
      
      return(list(score = pred, inputs = df))
    })
    
    # Render Stylized Score
    output$txtout <- renderUI({
      if (input$submitbutton == 0) return(NULL)
      val <- prediction_data()$score
      
      # 5-Level Color Logic
      res_color <- ifelse(val < 45, "#e74c3c", # Red
                          ifelse(val < 55, "#e67e22", # Orange
                                 ifelse(val < 65, "#f1c40f", # Yellow
                                        ifelse(val < 75, "#2ecc71", # Light Green
                                               "#27ae60")))) # Dark Green
      
      div(style = paste0("padding: 20px; border-radius: 15px; background-color: #f8f9fa; border: 3px solid ", res_color, "; text-align: center;"),
          h4(paste(input$stuName, "|", input$stuSubject, "|", input$stuClass), style = "color: #7f8c8d;"),
          h1(paste0(val, "%"), style = paste0("font-size: 65px; color: ", res_color, "; font-weight: bold; margin: 0;")),
          p("Predicted Examination Performance", style = "font-weight: bold; color: #34495e;")
      )
    })
    
    # Render Educator's Guide (5 Levels)
    output$actionGuide <- renderUI({
      if (input$submitbutton == 0) return(NULL)
      val <- prediction_data()$score
      
      statement <- ifelse(val < 45, "CRITICAL: Immediate intensive intervention and parental consultation required.",
                          ifelse(val < 55, "AT-RISK: Targeted support needed to bridge foundational gaps.",
                                 ifelse(val < 65, "MODERATE: Steady monitoring and additional tutorials recommended for stability.",
                                        ifelse(val < 75, "ON-TRACK: Continue current support and encourage consistent study habits.",
                                               "EXCELLENCE: Provide enrichment tasks to foster leadership and advanced growth."))))
      
      wellPanel(style = "background-color: #fdfefe; border-left: 6px solid #2c3e50;",
                tags$b(statement))
    })
    
    # Render the Reproduced Table
    output$inputTable <- renderTable({
      if (input$submitbutton == 0) return(NULL)
      
      # Get the raw data
      input_df <- prediction_data()$inputs
      
      # Create a mapping of short names to full names
      display_df <- data.frame(
        "Parameter" = c("Weekly Study Hours", "Attendance Percentage", "Previous Exam Scores", 
                        "Monthly Tutoring Sessions", "Parental Involvement", "Access to Resources", 
                        "Family Income", "Parental Education", "Motivation Level", "Peer Influence"),
        "Value" = as.character(t(input_df))
      )
      
      display_df
    }, striped = TRUE, bordered = TRUE, align = 'l')
  
  
  
    
  ## 5. Variable Importance Plot for Model Info
    output$importancePlot <- renderPlot({
      # We use caret's varImp function specifically on the model object
      # If using 'lm' without caret, we can extract the absolute t-statistics manually
      
      tryCatch({
        # Method 1: Standard caret importance
        importance <- caret::varImp(model1, scale = FALSE)
        rownames(importance) <- c("Weekly Study Hours", "Attendance %", "Previous Scores", 
                           "Tutoring Sessions", "Parental Involvement", "Access to Resources", 
                           "Family Income", "Parental Education", "Motivation Level", "Peer Influence")
        plot(importance, 
             main = "Statistical Drivers of Student Success",
             col = "#2c3e50",
             xlab = "Predictive Strength")
      }, error = function(e) {
        # Method 2: Fallback to manual calculation if caret object isn't fully preserved
        # This calculates importance based on the absolute value of the model coefficients
        coeffs <- summary(model1)$coefficients
        # Exclude the Intercept
        imp_data <- as.data.frame(coeffs[-1, , drop = FALSE]) 
        rownames(imp_data) <- c("Attendance %", "Weekly Study Hours", "Previous Scores", 
                                "Parental Involvement", "Access to Resources", "Tutoring Sessions",
                           "Family Income", "Parental Education", "Motivation Level", "Peer Influence")
        imp_data$Feature <- rownames(imp_data)
        
        ggplot(imp_data, aes(x = reorder(Feature, abs(`t value`)), y = abs(`t value`))) +
          geom_bar(stat = "identity", fill = "#2980b9") +
          coord_flip() +
          labs(title = "The Hierarchy of Success: Top Drivers",
               x = "Variables",
               y = "Statistical Significance (Absolute t-Statistic)") +
          theme_minimal()
      })
    })
}
####################################
# RUN THE APP
####################################
shinyApp(ui = ui, server = server)
