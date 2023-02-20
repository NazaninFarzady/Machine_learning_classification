library(shiny)
library(shinyjs)
library("tidyverse")
library("data.table")
library(shinycustomloader)
library("shinythemes")
library("shinyBS")
library("shinycssloaders")
library(shinyWidgets)


# UserInterface Begins here......
shinyUI(fluidPage( theme = shinytheme("flatly"),
  tagList( useShinyjs(),
  navbarPage(title = "SLEEPLY", id = "navbar",
             collapsible = TRUE,
             position = "fixed-top",
             windowTitle = "SLEEPLY APP",
             footer = includeHTML("www/include_footer.html"),
             header =  tags$style(
               ".navbar-right {
                       float: right !important;
                       }",
               "body {padding-top: 75px;}"),
                              
    tabPanel("HOME",icon = icon("home"), value = "homepage",                          
             fluidRow(
              
               HTML("<center> <h3>Sleep/wake classifier App for Sleep Quality Analysis</h3> </center>")),
             fluidRow(
               style = "height:250px;",
               HTML('<center><img src="sleepcycle_regular_sleep.png"></center>'),
               HTML('<center><p>Image reference:  <a href = "https://biohackstack.com/posts/sleep-cycle/">biohackstack</a></p></center>')
             ),
             
             fluidRow(
               
               style = "height:200px;"),
             
    # WHAT
            fluidRow(
             column(3),
             column(6,
               HTML("<br><br><center> <h2>What you'll find here</h2> </center><br>"),
               HTML("<p>
                    <h4>An interactive tool for sleep/wake detection and basic data exploration. The app automates the process of sleep/wake prediction using 
                    pre-trained machine learning models such as logistic regression (LR), random forest (RF), K-nearest neigbour (KNN), and support vector machine(SVM-gaussian kernel) on the Analysis page. 
                    The plugged in models are trained with parameters like active index, heart rate (hr), clock proxy, and breathe rate. 
                    Therefore, datasets which contains one or more of these mentioned parameters generates a reliable analysis results, 
                    however, any type of data with different type of variable is best analysed on the Exploration page.</h4></p>")
               ),
             column(3)
               ),
          
            fluidRow( style = "height:50px;"),
    
          
    fluidRow( style = "height:50px;"),
    
    # PAGE BREAK
    tags$hr(),
   
    
    
# INSTRUCTIONAL SECTION
          fluidRow(
            HTML("<br><br><center> <h1>How to Use the App in 3 Steps.</h1> </center>
                                            <br>")
           ),
    
          fluidRow(
           column(3),
           column(2,
             div(class="panel panel-default", 
                 div(class="panel-body",  width = "600px",align = "center",
                     div(tags$img(src = "one.svg",width = "50px", height = "50px")),
                     div(p(h5("Upload data in the file-input field provided on the Home page"))),
                     div(helpText(h6( "NOTE: If no data is upload yet, the UI for the Analysis and Exploration page will remain blank")))
                     
                    )
                 )
             ),
           column(2,
             div(class="panel panel-default",
                 div(class="panel-body",  width = "600px", align = "center",
                     div(tags$img(src = "two.svg", width = "50px", height = "50px")),
                     div(h5("Select a time-related column from the data then Tick the", tags$b("DATE/TIME"), "converter to change the data type of that variable to POSIXct (date/time format). Ignore this option if your data does not contain a time-related variable")),
                     div(helpText(h6( "NOTE: Converting a time-related column is only neccesary for effective analysis on the Exploration page")))
                     )
                 )
             ),
           column(2,
             div(class="panel panel-default",
                 div(class="panel-body",  width = "600px",align = "center",
                     div(tags$img(src = "three.svg",width = "50px", height = "50px")),
                     div( h5( "Explore your dataset with the dynamic table and different charts on the Exploration page. On the Analysis page, select preferred predictors and choose a label variable, then submit to generate a summary report."
                       ))
                     )
                 )
             ),
           column(12,
                  tags$div(align = "center", 
                           actionLink("LinkInstruction", "Click here to read more on the Instruction Page")
                  )
           ),
          column(3)
        ),
      fluidRow(
  
  style = "height:50px;"),

# PAGE BREAK
tags$hr(),

      fluidRow(HTML("<br><br><center> <h1>Get Started</h1> </center><br>")),
      fluidRow(
       column(3),
       column(6,
         tags$div(align = "center", 
                  actionButton("LinkDatapage", "Get Started")
                  )
        ),
       column(3)
       ),
     fluidRow(style = "height:25px;")
              
),# HomeTabpanel ENDs
    
      
      tabPanel("DATA",icon = icon("table"), value = "datapage",
       sidebarLayout(
        sidebarPanel(div(align="center",h4("Upload Document")),
    # Horizontal line ----
    tags$hr(),
    
    fileInput("file", "Choose a CSV File"),
    
    # Horizontal line ----
    tags$hr(),
          
    # Input: Checkbox if file has header ----
    div(id = "dataersatz",
    checkboxInput(inputId = "header", label =  "Header",value =  TRUE),
    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", TRUE),
    
    # Horizontal line ----
    tags$hr(),
                     
      # Input: Select separator ----
    radioButtons(inputId = "sep", label = "Separator",choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",", inline = TRUE)
     ),   
      # Horizontal line ----
    tags$hr(),
                     
      # Input: Select number of rows to display ----
    radioButtons(inputId = "disp", label = "Data Display", choices = c(Head = "head", All = "all"), selected = "head", inline = TRUE),
    
    tags$hr(),
    a(id = "toggleAdvanced", "Click here to convert timestamp related data", href = "#"),
    hidden(div(id ="asdateformat",checkboxInput("dateformat","DATE/TIME CONVERTER"),
    uiOutput("Todateformat")))
    
  ), #sidebarPanel for home ends
         
         mainPanel(
                                  
           uiOutput("tb")
                    ) #mainPanel for home ends
                  ) #sidebarLayout for home ends
                ),# dataTabpanel ENDs
                 
       tabPanel("EXPLORATION", icon = icon("area-chart"), value = "explorepage", uiOutput("explore")), # ~ExplorationTabpanelENDs
       tabPanel("ANALYSIS", icon = icon("list-ul"),value = "analysis", uiOutput("analysis")), # ~AnalysisTabPanel ENDs
       tabPanel("INSTRUCTION", icon = icon("question-circle"),value = "instructionpage",
                fluidRow( div(style = "background-color:#d9e6f2; padding:0px; height:100%;",
                  shiny::HTML("<br><br><center> 
                                            <h1>User Guide</h1> 
                                            </center>
                                            "), #,style = "height:250px;"
                  tags$div( align = "center",
                            icon("bar-chart", class = "fa-4x"),
                            #div( align = "center"#,
                            h3("Performing tasks on the Sleeply app"),
                            helpText(h5("This page provides information on how users can perform specific tasks on this application"))
                            
                              
                ),
                
                
                fluidRow( div(style = "background-color:#d9e6f2; padding:0px; height:100%;", 
                  column(6, div(class = "panel panel-default", 
                         div(class="panel-body",
                        tags$div( align = "left",
                        tags$strong(h4("1.	Upload a file – Task on the Data Page"))
                        ),
                        tags$p("Uploading a data file to the application is the first and prerequisite task
                               a user must perform to achieve other tasks. The web application's exploration
                               page and analysis page remain empty (displays on the Rshiny logo) when no data
                               is uploaded, then displays a set of input-widgets  and subtabs once a dataset is
                               uploaded. Follow the steps below to upload data:"),
                        tags$ul(
                          tags$li(h5('Step 1: Go to the data page by clicking on the "get started" button on the home page or directly select the data page tab')),
                          tags$li(h5('Step 2: Click the "Browse" button under the "Choose a file" option')),
                          tags$li(h5("Step 3: Select any CSV file of choice from your location system to upload the file")),
                          div( style = "background-color:#d9e6f2;", class = "panel panel-default", align = "center",
                                  h6('Note: Data has successfully been uploaded once three subtabs (Data, Description, and About file)
                                      appear on the data page\'s main panel. Subtabs and corresponding input-widgets for both the
                                      exploration and analysis page appear as well.'))
                        ), br(), br(),
                        
                        tags$div( align = "left",
                                  tags$strong(h4("2. Visualize the data – Task on the Exploration Page"))
                        ),
                        tags$p('On the exploration page, users can perform quick and basic data visualization tasks.
                               There are two subtabs on the Exploration Page\'s that appear, "Dynamic Table" and "Plot".'),
                        tags$ul(
                          tags$li(h5('Select and deselect variables via the "Data Column Names" option on the exploration page\'s sidebar panel to add and drop columns to the dynamic table. ')),
                          tags$li(h5('Customize the number of entries in view with the "Show 10 entries" widget on the table\'s top-left.')),
                          tags$li(h5('Search through the table using the text field labeled "Search", placed at the table\'s top-right. 
                                     The search bar allows users to type in texts as a pattern for the table to filter out all possible 
                                     entries of the data that matches the search')),
                          div( style = "background-color:#d9e6f2;", class = "panel panel-default", align = "center",
                               h6('Note: The changes made to the dynamic table does not affect the actual dataset.')),
                          tags$li(h5('The "Plot" subtab of the exploration displays a predefined list of plots: a line plot, scatterplot, and histogram;
                                     when you open the "Plot" subtab, the scatterplot the default chart in view. Users can customize and switch between 
                                     plots using the options provided under the "Plot Aesthetics" and "Select Plot Type"'))
                        ), br(),
                        
                        tags$div( align = "left",
                                  tags$strong(h4("3. Switch between Plots"))
                        ),
                        #tags$p('•	Go to the "Plot" subtab of the exploration page.'),
                        tags$ul(
                          tags$li(h5('Go to the "Plot" subtab of the exploration page')),
                          tags$li(h5('Select the desired plot type from the defined list of charts via the "Select Plot Type" drop-down menu widget')),
                          tags$li(h5('Click on the "Change Plot" button to change the chart in view to the selected one and call its corresponding 
                                     aesthetics as some widgets are specific to a plot')),
                          
                          div( style = "background-color:#d9e6f2;", class = "panel panel-default", align = "center",
                               h6('Note: Click on the "Reset" button to return all widgets on the exploration page to their default state.'))
                          
                        ))
                        
                  ) 
                         ),
                 # column(8) # end of div panel), # end of column 8
                  column(6,div(class = "panel panel-default",
                         div(class="panel-body",
                               tags$div( align = "left",
                                         tags$strong(h4("4.	Prediction Analysis – Task on the Analysis Page"))
                               ),
                               tags$p('Users can perform a prediction task on the uploaded data and immediately get the evaluation results on this page.
                                      The analysis page has two subtabs, the "Output" tab, which displays the confusion matrix and statistics results on
                                      the analysis. The "Report" shows specific outcomes such as accuracy, specificity, sensitivity, and area under the 
                                      curve (AUC)  in tabular and plots.'),
                               tags$ul(
                                 tags$li(h5('Step 1: Select one or more predictors (independent variables) from the "Select Input Variable(s)" drop-down widget')),
                                 tags$li(h5('Step 2: Select a label (dependent variable) from the " Select Label Variable" widget')),
                                 tags$li(h5('Step 3: Choose a model from the list of predefined ML models in the "Select Model" option')),
                                 tags$li(h5('Step 4: Step 4: Pick a K-value from the " Choose a Value for K" slider widget. If the model selected is either 
                                            "K-Nearest Neighbor" or "K-Nearest Neighbor: WD')),
                                 tags$li(h5('Step 5: Click the "Submit" button to run the analysis and results. This may take a few minutes, 
                                            depending on the model and size of the uploaded data')),
                                 
                                 div( style = "background-color:#d9e6f2;", class = "panel panel-default", align = "center",
                               # helpText(
                                          h6('Note: In Step 2, it is required to select a label or dependent variable that contains exactly two unique values. 
                                             For instance, if the unique value(s) in variables A, B, C, D, E, and F are as follows: A = (0, 1), B = (x, y), 
                                             C  = (yes, no),  D = (1), E = (x, y, z), F = (yes, no, maybe). A warning text shows up if variable D, E, or F 
                                             is selected because D has one unique value, while E and F have more than two unique values. Otherwise, 
                                             the warning text becomes invisible if variable A, B, or C is selected, as all contain exactly two unique values. Moreover, 
                                             the submit button will be disabled if some conditions as mentioned above are not met while interacting with the input-widgets. 
                                             Please follow the instruction of the warning messages to enable the "Submit" button.')
                                         # )
                                )
                                 ), br(),
                               tags$div( align = "left",
                                         tags$strong(h4("5. Download Analysis Report"))
                               ),
                               tags$p('After successfully running a prediction task, the results show up on the "Output" and "Report" subtabs. The users can download 
                                      the report in an Html or word document format. The following set of activities are required to download the analysis results:'),
                               tags$ul(
                                 tags$li(h5('Step 1: Go to the "Report" subtab of the analysis page')),
                                 tags$li(h5('Step 2: Navigate to the panel at the end of the page')),
                                 tags$li(h5('Step 3: Choose a preferred file format, that is, "HMTL" or "Word"')),
                                 tags$li(h5('Step 4: Click on the "Download" button to generate and save the report in the local machine')),
                                 br(),hr(),
                                 #HTML('<center><p>Image reference:  <a href = "https://biohackstack.com/posts/sleep-cycle/">biohackstack</a></p></center>')
                                 HTML('<div class = "panel panel-default" style = "background-color:#19334d";><h5 style = "color: #ffffff;"><center><p>General Note: Information given on this page only highlights possible activities to
                                      achieve specific tasks. For detailed information on how to use and customize the Sleeply
                                      app, follow this <a href = "https://drive.google.com/file/d/14O16AENDdkHsTAGaZegNgfwjrSSFby8k/view?usp=sharing">link</a></p></center></h5></div>')
                               ), br(), br(), br(), br()
                             )
                         )
                )))))
                ), #end of instruction page

       tabPanel("ABOUT",icon = icon("info-circle"), value = "aboutpage", 
                fluidRow(
                  shiny::HTML("<br><br><center> 
                                            <h1>About Sleeply App</h1> 
                                            <h4>The Initiative Behind the app.</h4>
                                            </center>
                                            <br>
                                            <br>"),
                  style = "height:250px;"
                  ),


                fluidRow(
                  column(2),
                  column(8,
                         # Panel for Background on Data
                         div(class="panel panel-default",
                             div(class="panel-body",    
                                 tags$div( align = "center",
                                           icon("question-circle", class = "fa-4x"),
                                           div(align = "center",
                                                h5("Executive Summary")
                                           )
                                 ),
                                 tags$p(h6(
                                   'We built the Sleeply application (app) to deploy and present the machine learning (ML) models from our main study,
                                   "Sleep and wake classification using data-driven from a smart-band: presented in rshiny web application". A joint study
                                    between the Department of Psychology and the Department of Informatics of the University of Zurich.'
                                   
                                 )),
                                 tags$p(h6("
                                      The Sleep is an accessible and time-saving tool primarily developed for psychologists to automate classification analysis
                                      on any choice data with  no prior knowlegde on programming. With this tool,  users can upload data and visually explore it 
                                      with various charts listed in the application. Users can also select preferred input parameters (the independent variables)
                                      available from the uploaded dataset and analyze the data with the different ML models. Finally, the users can view the prediction results 
                                      in tables and plots and ultimately download the analyses results.     
                                           ")),
                                 tags$p(h6("The trained ML models defined within this application are:")),
                                  tags$ul(
                                   tags$li(h6("Logistic Regression (LR)")),
                                   tags$li(h6("Random Forest (RF)")),
                                   tags$li(h6("K-Nearest Neighbor (KNN)")),
                                   tags$li(h6("Support Vector Machine with the Gaussian kernel (SVMR)")),
                                   helpText(h6("Each model also includes its downsample variant"))
                             )
                             )
                         ) # end of div panel
                  ), # end of column 8
                  column(2)
                ),br(), br(),br(),br(),br(), 
                # TEAM BIO
                fluidRow(
                  column(3),
                  column(6,
                         tags$div( align = "center",
                                   icon("users", class = "fa-4x"),
                                   div( align = "center", 
                                        h4("About the team")
                                   )
                         ),
                         #HTML("<br><br><center> <h3>The People</h3> </center><br>"),
                         HTML("<h6>The Sleeply App is an initiative that stemmed from the Swiss menopausal project organised by the Department of Psycology, 
                         in collaboration with the Department of Informatics of the University of Zurich. Below is a brief information about the project team and supervisors!</h6>")
                         #,HTML("<br><br><center> <h5>About the team</h5> </center><br>")
                  ),
                  column(3)
                ),
                
                fluidRow(
                  
                  style = "height:10px;"),
                
                fluidRow(
                  column(3),
                  
                  # RUike
                  column(2,
                         div(class="panel panel-default", 
                             div(class="panel-body",  width = "600px",
                                 align = "center",
                                 div(
                                   tags$img(src = "Ruike.jpg", 
                                            width = "100px", height = "150px")
                                 ),
                                 div(
                                   tags$h5("Ruike Wang"),
                                   tags$h6( tags$i("Information  Analyst & Machine Learning Engineer")),
                                   a(tags$h6( tags$i("ruike.wang@uzh.ch")))
                                 ),
                                 div(
                                   "Currently pursuing her  masters degree in Information Systems at the University of Zurich"
                                 )
                             )
                         )
                  ),
                  # Olajoke
                  column(2,
                         div(class="panel panel-default", style = "height: 365px",
                             div(class="panel-body",  width = "600px", 
                                 align = "center",
                                 div(
                                   tags$img(src = "Attachment1.jpeg", 
                                            width = "100px", height = "150px")
                                 ),
                                 div(
                                   tags$h5("Olajoke Oladipo"),
                                   tags$h6( tags$i("Information Analyst, Project Ordinator & Programmer")),
                                   a(tags$h6( tags$i("olajoke.oladipo@uzh.ch")))
                                 ),
                                 div(
                                   "A master Student at the University of Zurich, majoring in Information Systems."
                                 )
                             )
                         )
                  ),
                  # Nazanin
                  column(2,
                         div(class="panel panel-default", style = "height: 365px",
                             div(class="panel-body",  width = "600px", 
                                 align = "center",
                                 div(
                                   tags$img(src = "Nazanin.jpg", 
                                            width = "100px", height = "150px")),
                                 div(
                                   tags$h5("Nazanin Farzady"),
                                   tags$h6( tags$i("Data Sciencist & Writer")),
                                   a(tags$h6( tags$i("nazanin.farzady@uzh.ch")))
                                 ),
                                 div(
                                   "Studied Data Science at University of Zurich"
                                 )
                             )
                         )
                  ),
                  column(3)
                  
                ),
                # SUPERVISORS BIO
                fluidRow(
                  column(3),
                  column(6,
                         shiny::HTML("<br><br><center> <h4><strong>The Project Supervisors</strong></h4> </center><br>")

                  ),
                  column(3)
                ),
                
                fluidRow(
                  
                  style = "height:50px;"),
                
                fluidRow(
                  column(3),
                  
                  # Danielle
                  column(3,
                         div(class="panel panel-default", 
                             div(class="panel-body",  width = "400px",
                                 align = "center",
                                 div(
                                   tags$img(src = "dellaglio.png", 
                                            width = "100px", height = "100px")
                                 ),
                                 div(
                                   tags$h5("Dr. Daniele Dell'Aglio"),
                                   tags$h6( tags$i("Postdoc at Department of Informatics"))
                                 ),
                                 div(
                                   "A Postdoc at DDIS since September 2016. 
                                   His research activities focus on stream processing, 
                                   in particular on stream reasoning, 
                                   i.e. the application of inference techniques 
                                   to data streams, and web stream processing, 
                                   i.e. management of streams in the web setting. 
                                   He holds a Ph.D. from Politecnico di Milano, 
                                   during which he designed a formal reference model 
                                   to capture the behaviour of existing Stream Reasoning solutions."
                                 )
                             )
                         )
                  ),
                  # Jessica
                  column(3,
                         div(class="panel panel-default", style = "height:380px;", 
                             div(class="panel-body",  width = "400px", 
                                 align = "center",
                                 div(
                                   tags$img(src = "jessicagrub.jpg.jpg", 
                                            width = "100px", height = "100px")
                                 ),
                                 div(
                                   tags$h5(" Jessica Grub, MSc "),
                                   tags$h6( tags$i("Project Owner"))
                                 ),
                                 div(
                                   "PhD student/ scientific assistant,University of Zurich 
                                   Department of Psychology Clinical Psychology and Psychotherapy."
                                 )
                             )
                         )
                  ),
  
                  column(3)
                  
                ),
                
                fluidRow(style = "height:150px;")
        ) # AboutTabpanel ends
                 
   ) #navbarpage ends
  )# taglist ends
 ) #fluidpage ends
) #shinyUI ends







