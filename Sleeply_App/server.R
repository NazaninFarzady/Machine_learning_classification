library("plotly")
library("e1071")
library("tidyverse")
library("caTools")
library("caret")
library("randomForest")
library("pROC")
library("class")
library("shinyjs")
options(shiny.maxRequestSize = 5000*1024^2) # increase default file size from 5MB to 5GB.

source("helper.R")



#### Server Function Begins
shinyServer(function(input, output, session) {
  

### Commands to link two pages  
    observeEvent(input$LinkDatapage, {
    updateTabsetPanel(session, "navbar", "datapage")
      
  })

  observeEvent(input$LinkInstruction, {
    updateTabsetPanel(session, "navbar", "instructionpage")
  })

  
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  dat <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
      data <- read.table(file = file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)

  }) 
  
  
  # For date/time convertion function
  output$Todateformat <- renderUI({
    if(is.null(dat())){return()}
    selectInput("selectedColumn", "select the time related Column", names(dat()), names(dat()[[1]]))
  })
  
  observe({
    if(is.null(dat())){return()}
    onclick("toggleAdvanced", shinyjs::show(id = "asdateformat", anim = TRUE))
    onclick("toggleAdvanced", confirmSweetAlert(session = session, inputId = "warning4",title = "Warning..!",
                                               text = "Be sure your data contains a date/time related column",
                                               btn_labels = "Got it",
                                               type = "warning", closeOnClickOutside = TRUE), TRUE) 
  })

  data <- reactive({
    if(is.null(dat())){return()}
    data <- dat()
    if(input$dateformat == FALSE) {
        return(data)
      }
      else {
        tryCatch({
          data[input$selectedColumn] <- as.POSIXct(data[,input$selectedColumn])
        },
        error = function(err){
          confirmSweetAlert(session = session, inputId = "errmessage",title = "Error..!",
                            text = paste0("Selected column does not have date/time attribute :",err$message),
                            btn_labels = "Try Again",
                            btn_colors = "red",
                            type = "error", closeOnClickOutside = TRUE)
           
          })
        return(data)
      }
  })
       
       
     

  
 # disable dataersatz if input$dateformat is TRUE
observe({
    if(is.null(data())){return()}
    if(input$dateformat == TRUE){
      disable("dataersatz")
    } else {
      enable("dataersatz")
    }
})
  
  
  
  
## This reactive output contain properties information of the dataset and displays it in a table format
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
# This reactive output contains the summary of the dataset and prints the structure
  output$summary <- renderPrint({
    if(is.null(data())){return()}
    summary(data())
  })
  
# This reactive output contains and prrnts the structure of the dataset 
  output$struct <- renderPrint({
    if(is.null(data())){return()}
    str(data())
  })
  
# This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return()}
    if(input$disp == "head") {
      return(head(data()))
    }
    else {
      return(data())
    }
  })
  
# The following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      div(align = "center",
          tags$img(src="shiny-logo.png", height=400, width=400),
          h4("Upload a data to get started")
          )
    else
      tabsetPanel(
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Description",
                           tags$label(h4('Data Summary')), 
                           verbatimTextOutput("summary"),
                           tags$hr(),
                           tags$label(h4("Data Structure")),
                           verbatimTextOutput("struct")),
                  tabPanel("About file", tableOutput("filedf"))
                  ) # tabsetPanel ends
})


##############################################  
#### Server function for exploration page #### 
#############################################

  
  # To Generate a Dynamic Table 
  output$mytable1 <-  DT::renderDataTable({
    DT::datatable(data()[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
  })
  

# plot1 = scatter plot
  
  output$plot1 <- renderPlot({
  #output$plot1 <- renderPlotly ({
     p <- ggplot(data(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    

    print(p)
    
})
  
# plot2 = histogram plot
  output$plot2 <- renderPlot({
    
    p <- ggplot(data(),aes_string(x = input$x)) + geom_histogram(binwidth = input$binwidth, alpha = input$alpha, color = "white") 
    
    #if(input$fill != "None")
    #p <- ggplot(data(),aes_string(x = input$x)) + geom_histogram(binwidth = input$binwidth, alpha = input$alpha, fill = input$fill, color = "white")
    
    if (input$group != 'None')
      p <- p + aes_string(fill=input$group) 
      
    if(input$density)
    p <- p + geom_density(color = "red", lwd = 0.9, alpha = 0.5) #, alpha = 0.1
    p <- p + aes(y = ..density..) 
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
  
    
    print(p)
    
}) 
  
# plot3 = line plot  
  
   output$plot3 <- renderPlot({
   #output$plot3 <- renderPlotly ({
    p <- ggplot(data(), aes_string(x=input$x, y=input$y)) + geom_line() 
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    print(p)
    
  }) 
  
  ################## END of plots functions for Exploration page ######################
  
  ################## UI for Exploration page ######################
  
  output$explore <- renderUI({
    
    if(is.null(data()))
      fluidRow( 
        tags$div(align = "center",
                 tags$img(src="shiny-logo.png", height=400, width=400),
                 h4("Upload a data from the Data Page to get started")
        ),
        fluidRow(style = "height:120px;")
      )
    else
      sidebarLayout(
        sidebarPanel(div(align = "center",h4("Exploration Control Panel")),
                     tags$hr(),
                     div(id = "resetplot",
                     pickerInput("show_vars","Data Column Names:",
                                        names(data()), selected = names(data()),multiple = TRUE, options = list(`selected-text-format` = "count > 3",size = 5)
                                        ),
          
                     h6(helpText("Add/remove data column name to filter the table by column ")),
                     
                     tags$hr(),
                     selectInput("plot_type", "Select Plot Type", #<--
                                 c("Scatterplot", "Histogram", "Line Plot")),
                     div(align ="center",div(style = "display: inline-block;",actionButton("submit_plot", label = "Change Plot", class = "btn-primary")),
                     div(style = "display: inline-block;",actionButton("reset_plot", label = "Reset Widgets", class = "btn-primary"))),
                     
                     tags$hr(),
                     div(align ="center", h4("Plot Aesthetics")),
                     tags$hr(),
                     div(id = "scat_specific",
                         div(style = "display: inline-block;" ,checkboxInput('jitter', 'Jitter')),
                         div(style = "display: inline-block;" ,checkboxInput('smooth', 'Smooth'))),
                     selectInput('x', 'Input X-axis', names(data())),
                     div(id = "scat_line",selectInput('y', 'Input Y-axis', names(data()), names(data())[[2]]),
                     selectInput('color', 'Color', c('None', names(data())))), 
                     
                     hidden(div( id = "hist_specific",
                       selectInput('group', 'Group', c('None', names(data()))),
                    
                     sliderInput(inputId = "alpha", label = "Transparency",
                                 min = 0, max = 1, value = 0.8),
                     
                     #selectInput('fill', 'Select HCOLOR', choices = c('None', blue = "#75AADB", teal = "#108A99", "tomato")),
                     sliderInput(inputId = "binwidth", label = "Select Bin-width\'s Value",
                                 min = 0, max = 50, value = 3, step = 0.5),
                     checkboxInput('density', 'Overlay Density Plot') 
                    )),
                     tags$hr(),
                    selectInput('facet_row', 'Facet Row',
                                c(None='.', names(data()))),
                    selectInput('facet_col', 'Facet Column',
                                c(None='.', names(data()))))
        ), #sidebarPanel for exploration ends
        
        mainPanel(tabsetPanel(id = "tabset1",
          tabPanel("Dynamic Table", DT::dataTableOutput("mytable1")),
          tabPanel("Plot", 
                        div(align ="center",id = "plotpage",div( id = "scat",
                        tags$label(h4("Scatter Plot")),br(),br(),
                        withSpinner(plotOutput("plot1")),#plotlyOutput("plot1"))
                                    br(),br(), downloadButton('Download_Scatter')),
                   
                   hidden(div(id = "histo",
                       tags$label(h4("Histogram")),br(),br(), 
                       withSpinner(plotOutput("plot2")),
                       br(),br(),downloadButton('Download_Hist'))),
                   
                   hidden(div(id = "line",
                      tags$label(h4("Line plot")), br(),br(),
                      withSpinner(plotOutput("plot3")),
                      br(),br(),downloadButton('Download_line'))))
                      #withSpinner(plotlyOutput("plot3"))))
                  ) # end of tabPanel for Plot     
      )# tabesetPanel for exlporation ends
    ) #mainPanel for Exploration ends
) #sidebarLayout for Analysis ends
      
})
  
########################################  observeEvents for exploration page #################3
observeEvent(input$reset_plot,{
     reset("resetplot")
   })

  
observeEvent(input$submit_plot,{
    if (input$plot_type == "Histogram"){
      shinyjs::show("histo");shinyjs::show("hist_specific");hide("scat");hide("scat_specific");hide("line");hide("scat_line")
    } else if (input$plot_type == "Line Plot") {
      shinyjs::show("line");shinyjs::show("scat_line");hide("scat");hide("histo");hide("hist_specific");hide("scat_specific")
    } else {
      shinyjs::show("scat");shinyjs::show("scat_specific");hide("histo");hide("line");hide("hist_specific")
    }
})

####################################### 
## Server function for Analysis page ##
#######################################

    output$analysis <- renderUI({
      if(is.null(data()))
        fluidRow( 
          tags$div(align = "center",
                 tags$img(src="shiny-logo.png", height=400, width=400),
                 h4("Upload a data from the Data Page to get started")
          ),
        fluidRow(style = "height:120px;")
      )
   
      else
        sidebarLayout(
          sidebarPanel(div(align = "center",h4("Analysis Control Panel")), hidden(div(id = "refresh_btn",align ="center", actionLink("refresh", "Refresh"))),
                       # Horizontal line ----
                      div(id = "analysis_cpanel",tags$hr(),
                       hidden(div( id = "warning1", style = "color:red;", h6("WARNING!!! select at least one input variable"))),
                       selectInput('predictor', 'Select Input Variable(s):',  names(data()), selected = names(data())[[1]], multiple = TRUE),
                       hidden(div( id = "warning2", style = "color:red;", h6("WARNING!!! Label variable is not similar to preset variable. Please change variable"))),
                       selectInput('label', 'Select Label Variable', names(data())),
                       h5(helpText("User can only select a single Variable to predict")),
                       # Horizontal line ----
                       tags$hr(),

                      #----------------------------------------------------------------------------------------------#
                      # Control the model names 
                      selectizeInput("model", "Select a model:",
                                    list( # short exp: LR is for internal reference within the server while the name that appears on UI is Logistic Regression
                                "Without Downsampling" = c("Logisitic Regression" = "LR",
                                                           "K-Nearest Neighbor" = "KNN",
                                                           "SVM Guassian" = "SVM",
                                                           "Random Forest" = "RF"
                                                          ),
                              "With Downsampling" = c("Logisitic Regression : WD" = "down_LR",
                                                      "K-Nearest Neighbor : WD" = "down_KNN",
                                                      "SVM Guassian : WD" = "down_SVM",         
                                                      "Random Forest : WD" = "down_RF"
                                                      )
                                          ) # end of list
                                    ),
                      #----------------------------------------------------------------------------------------------#
                                            
                       tags$hr(),
                       hidden(div( id = "Kvalue",sliderInput(inputId = "k", label = "choose a Value for K:",
                                   min = 0, max = 100, step = 4,value = 20)))
                      ),
                       # Horizontal line ----
                       tags$hr(),
                       # Horizontal line ----
                       tags$hr(),
                       div(align ="center", actionButton("submit", "Submit", class = "btn-primary")),
                      tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                      tags$br(),tags$br(),tags$br(),tags$br()
                      
          ), #sidebarPanel for Analysis ends
         
           mainPanel(
            # create Tabset for Analysis page
            tabsetPanel(type = "tabs", 
                        tabPanel("Output",
                                 tags$label(h3('Status/Output')),
                                 fluidRow(column(1),
                                   column(10,
                                          div(class="panel panel-default",
                                              div(class="panel-body",
                                                  withSpinner(verbatimTextOutput("roc"))
                                              )
                                          )),
                                   column(1)
                                   )
                                 ), # end of Status/Output 
                                 #withSpinner(tableOutput("ConfusionMat")))),
                        
                      #  tabPanel("Roc Plot", #verbatimTextOutput("text")#tags$label(h4("Interactive ROC curve")),
                                 #,#div(id="spinner" ,withSpinner(plotlyOutput("roc2"))), #),
                       tabPanel("Report",
                                 fluidRow(
                                   div(align="center", 
                                            h4("Overview report on prediction"),
                                            br())
                                   #style = "height:250px;"
                                 ),
                                 fluidRow(
                                   column(6,
                                          div(class="panel panel-default",
                                              div(class="panel-body",
                                              tags$div(align = "center",h5("Tabular format of the Confusion Matrix")),
                                              withSpinner(tableOutput("ConfusionMat")),
                                              hr(),
                                              tags$div(align = "center",h5("ROC Curve")),
                                              withSpinner(plotOutput("roc1"))
                                              )
                                          )),
                                   column(6,
                                          div(class="panel panel-default",
                                              div(class="panel-body",  
                                                  tags$div( align = "center",h5("Evaluation Results"),
                                                  withSpinner(tableOutput("result")),
                                                  hr(),
                                                  tags$div(align = "center",h5("Results Represented in a barplot")),
                                                  withSpinner(plotOutput("r"))
                                                  )
                                              )
                                          )),
                                   column(12,
                                          hidden(div(class="panel panel-default", id = "Report_generator",
                                              div(class="panel-body",  
                                                  tags$div( align = "center",h5("Download report on prediction"),
                                                            radioButtons('format', 'Document format', c('HTML','Word'),inline = TRUE),
                                                            downloadButton('downloadReport'))
                                                  #,withSpinner(verbatimTextOutput("text"))
                                                  
                                              )
                                          )))
                                 )
                                 )# end of report Tabpanel
                        ) # end of main panel's tabset
          ) #mainPanel for Analysis ends
        ) #sidebarLayout for Analysis ends
    })



observe({
  if(is.null(data())){return()}
  if (is.null(input$predictor) || input$predictor == "") {
    disable("submit")
    shinyjs::show("warning1")
  } else if (length(unique(data()[,input$label])) != 2 ){
    disable("submit")
    shinyjs::show("warning2")
  } else if ( input$label %in% input$predictor == TRUE){
    disable("submit")
    confirmSweetAlert(session = session, inputId = "warning4",title = "Warning..!",
                      text = "you have selected same variable for both INPUT and LABEL options...",
                      btn_labels = "Ok",
                      type = "warning", closeOnClickOutside = TRUE) 
    
  } else if (input$model == "KNN" || input$model == "down_KNN"){
    shinyjs::show("Kvalue")
  } else {
    enable("submit")
    hide("warning2")
    hide("warning1")
    hide("Kvalue")
  }
}) # end of observe function



##############################################  
#### Perform and generate analysis reults #### 
#############################################


prediction <- eventReactive(input$submit,{
  
  #rename loaded data for easier manipulation in the analysis section
  df <- data()
  
  # factor the label variable
  classifier <- factor(df[,input$label])
 
####################To trigger the modal message below ################################ 
  trigger <- intersect(names(df),fixed_pred) 

  if (length(trigger) == 0)
    showModal(modalDialog("Warning!!! The result from the analysis won`t be accurate. The variables in the uploaded data did not match any variable this ML model trained with. To get better results please upload a similar dataset"))

##################################################

    for (x in fixed_pred) {
      if (x %in% input$predictor == TRUE){
         df[x] <- as.data.frame(scale(df[x]))
         #df[x] <- df[x]
      } else {
        df[x] <- 0
      }
      ndf <- df[, (names(df) %in% fixed_pred)]
    }
    
     df <-  data.frame(ndf,classifier)
  
  output$text <-  renderPrint({
   str(df)
  })

  ###################################
  ### Fetch Predefined ML models ###
  ##################################
  
  model <- reactive({ 
             
             model <- switch(input$model, 
                             LR = readRDS("models/LR.rds"),
                             KNN = knn(df_train[, (names(df_train) %ni% input$label)],  
                                       df[,(names(df) %ni% input$label)],  
                                       cl = df_train$classifier, k= input$k),
                             SVM = readRDS("models/SVMG.rds"),
                             RF = readRDS("models/RF.rds"), 
                             down_LR = readRDS("models/down_LR.rds"),#down_LR.rds
                             down_KNN = knn(downsampleDF[, (names(downsampleDF) %ni% input$label)],  
                                        df[,(names(df) %ni% input$label)],  
                                       cl = downsampleDF$classifier, k= input$k),
                             down_SVM = readRDS("models/down_SVMG.rds"),
                             down_RF = readRDS("models/down_RF.rds")
                             )

})

##################################################
  
if (input$model == "LR" || input$model == "down_LR"){
    
    pred_lr <- predict(model(), newdata = df, type = "response") # To predict log regression model on our testing dataset
    pred.results <- ifelse(pred_lr > 0.5, 1, 0) # to calculate from the predicted values
    prediction <- factor(pred.results, levels=c(0, 1)) # re factor if not error may error
    
  } else if (input$model == "KNN" || input$model == "down_KNN") {
    
    prediction <- model()
    
  } else if (input$model == "SVM"|| input$model == "down_SVM"){
    
    prediction <-  predict(model(), newdata = df[, -ncol(df)])
  
  } else {
    
    prediction <- predict(object = model(), newdata = df,type ="class")    
    
  }
  
  
  
}) 
    
########## Make prediction ###################
 
confmat <- reactive({
   confusionMatrix(prediction(), factor(data()[,input$label], levels=c(0, 1)), positive = "1", mode = "everything")
 })

results <- reactive({
  #confusionMatrix(prediction(), factor(data()[,input$label], levels=c(0, 1)), positive = "1", mode = "everything")
  confmat <- confmat()
  Acc <- as.data.frame(confmat$overall)
  rest <- as.data.frame(confmat$byClass) 
  
  
  # get specific results from the confusion matrix
  Accurracy <- Acc["Accuracy",]
  Sensitiviy <- rest["Sensitivity",]
  Specificity <- rest["Specificity",]
  
  # get the Area under curve value
  AUC <- auc(data()[,input$label], as.numeric(prediction())) # Logistic Regression
  
  Values <- c(Accurracy,Sensitiviy,Specificity,AUC)
  Evaluation <- c("Accurracy","Sensitiviy","Specificity","AUC")
  Results <- data.frame(Evaluation,Values)
  Results
  
})

#############################
    
    output$result <- renderTable({
      #confusionMatrix(prediction(), factor(data()[,input$label], levels=c(0, 1)), positive = "1", mode = "everything")
      results()
      
    })

    output$ConfusionMat <- renderTable({
      #confusionMatrix(prediction(), factor(data()[,input$label], levels=c(0, 1)), positive = "1", mode = "everything")
      confmat <- confmat() 
      ConfusionMat <- confmat$table   #as.data.frame(result$table)
      
    })
    
    output$r <- renderPlot({
      #confusionMatrix(prediction(), factor(data()[,input$label], levels=c(0, 1)), positive = "1", mode = "everything")
      ggplot(results(), aes(x = Evaluation, y = Values, fill = Evaluation)) +
        geom_col() + theme(legend.position = "none")
      
    })
    
    output$roc <- renderPrint({
      #confusionMatrix(prediction(), factor(data()[,input$label], levels=c(0, 1)), positive = "1", mode = "everything")
      confmat()
    })
    
output$roc1 <- renderPlot({
      par(pty = "s") 
      roc(data()[,input$label], as.numeric(prediction()),
         plot = TRUE, legacy.axes=TRUE, percent=TRUE,
         xlab="False Positive Percentage", ylab="True Postive Percentage",
         col="#263e63", lwd=2, print.auc=TRUE,print.auc.x=60, full.auc = TRUE, #F64040
         auc.polygon = TRUE, auc.polygon.col = "#263e6322")
})
   
output$roc2 <- renderPlotly({
      roc.rf <- roc(data()[,input$label], as.numeric(prediction())) # Random Forest
      gg <- ggroc(roc.rf, size = 1, colour = "red",legacy.axes = TRUE) + # finally, plot graph using ggroc()
        theme_classic() +
        labs(x = "Fraction of wake scored as sleep(FPR)", y = "Fraction of sleep scored as sleep(TPR)") #+geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey")
      
      print(gg) 
})


# observe function to show Report generator Properties, disable analysis control panel
observe({
  if(is.null(data())){return()}
  if(!is.null(prediction())||prediction() != ""){
    shinyjs::show("Report_generator")
    hide("submit")
    shinyjs::show("refresh_btn")
    disable("analysis_cpanel")
  } #else {
  #   enable("analysis_cpanel")
  #   hide("reset_report")
  # }
})

observeEvent(input$refresh,{
  enable("analysis_cpanel")
  reset("analysis_cpanel")
  shinyjs::show("submit")
  #removeUI("#analysis")
  
})

    
    
    
#downloadfunction for report
     output$downloadReport <- downloadHandler(

      filename = function() {
        paste0('Report_for','_',input$model,sep = '.', 
               switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        params <- list(data= data(), pred= prediction(), label = input$label, model = input$model, predictors= input$predictor)
        library(rmarkdown)
        out <- withProgress(
          render('report.Rmd', 
                      params = params,
                      switch(input$format,HTML = html_document(), Word = word_document())
                      ), message = "Generating Report..."
        )
        file.rename(out, file)
      }
    )  
  
}) # ends function and server
