#
# 
####Updating to NavbarPage######

###Libraries###
library(shiny)
library(tidyverse)
library(data.table)
library(sortable)
library(shinyjs)
library(psych)
library(DBI)
library(odbc)

###Function that saves your data to SQL#####

saveData <- function(data,username) {
  ##Connect to the database
  connect <- dbConnect(odbc(),Driver = 'SQL Server',
                       Server = 'DESKTOP-O9TGTM6', Database= 'Relabase',
                       Trusted_Connection='yes')
  #create new table
  dbWriteTable(connect,username,data)
  #disconnect from database
  dbDisconnect(connect)
} 



################Server#################

server <- function(input, output) {

#####Page 1: Features of Dataset######  
  
  output$features <- renderText({
    paste0("Hello ", input$username, "! ",
           "You have indicated that this dataset reports on ",input$dyadic, " partner(s)",
           "and ", input$longitudinal, " timepoint(s) per relationship",
           "recruited from ", input$datadates[1], " to ", input$datadates[2])
  })

####Read in the data
  uploadeddata <- eventReactive(
    c(input$uploadeddata),
    {
      rawdata <- read_csv(input$uploadeddata$datapath)
      rawdata
    }
  )

  
######Page 3: Subset from search list to select chosen items#######  
  #####Variable 1######
  
  subsetChooseList1 <- reactive({
    items1 <- names(uploadeddata())
    pattern <- input$subsetChooseListText1
    if (nchar(pattern) < 1) {
      return(items1)
    }
    items1[
      grepl(
        x = items1,
        pattern = input$subsetChooseListText1,
        ignore.case = TRUE
      )
    ]
  })
  
  output$selection_list1 <- renderUI({
    labels <- subsetChooseList1()
    # remove already chosen items
    labels <- labels[!(
      labels %in% input$rank_scale1
    )]
    rank_list(
      text = "Select and drag your items",
      labels = labels,
      input_id = "rank_all",
      options = sortable_options(group = "mygroup1", multiDrag = TRUE)
    )
  })
  
  ##Creates a list of chosen items
  chosen_items1 <- reactive({
    input$rank_scale1
  })

  
  
  #####Variable 2######
  
  subsetChooseList2 <- reactive({
    items2 <- names(uploadeddata())
    pattern <- input$subsetChooseListText2
    if (nchar(pattern) < 1) {
      return(items2)
    }
    items2[
      grepl(
        x = items2,
        pattern = input$subsetChooseListText2,
        ignore.case = TRUE
      )
    ]
  })
  
  output$selection_list2 <- renderUI({
    labels <- subsetChooseList2()
    # remove already chosen items
    labels <- labels[!(
      labels %in% input$rank_scale2
    )]
    rank_list(
      text = "Select and drag your items",
      labels = labels,
      input_id = "rank_all",
      options = sortable_options(group = "mygroup2", multiDrag = TRUE)
    )
  })
  
  ##Creates a list of chosen items
  chosen_items2 <- reactive({
    input$rank_scale2
  })

  
  
####Page 4: Calculate variable composites (including reverse-scored items)########  
  #####Variable 1########
  
  output$reverse_button1 <- renderUI({
    return(checkboxGroupInput("reverse_items1", "Are any of the commitment items designed to be reverse-scored?
                       Please check all that apply",c(chosen_items1()))
    )
  })
  
  coding_scheme1 <- reactive({
    input$coding_scheme1
  })
  
  final_scaleitems1 <- reactive({
    selectedsub1 <- uploadeddata() %>% select(chosen_items1())
    if(input$reverse_coded1==0){finalsub1 <- selectedsub1} 
    else if(input$reverse_coded1==2){finalsub1 <- selectedsub1}
    
    ###reverse items if needed
    else if (input$reverse_coded1 == 1){
      nottoreverse1 <- selectedsub1 %>% select(!input$reverse_items1)
      toreverse1 <- selectedsub1 %>% select(input$reverse_items1)
      reversed1 <- abs(toreverse1 - as.numeric(input$coding_scheme1))
      finalsub1 <- cbind(nottoreverse1,reversed1)
    }  
  })  

  
  #####Variable 2########
  
  output$reverse_button2 <- renderUI({
    return(checkboxGroupInput("reverse_items2", "Are any of the satisfaction items designed to be reverse-scored?
                       Please check all that apply",c(chosen_items2()))
    )
  })
  
  coding_scheme2 <- reactive({
    input$coding_scheme2
  })
  
  final_scaleitems2 <- reactive({
    selectedsub2 <- uploadeddata() %>% select(chosen_items2())
    if(input$reverse_coded2==0){finalsub2 <- selectedsub2} 
    else if(input$reverse_coded2==2){finalsub2 <- selectedsub2}
    
    ###reverse items if needed
    else if (input$reverse_coded2 == 1){
      nottoreverse2 <- selectedsub2 %>% select(!input$reverse_items2)
      toreverse2 <- selectedsub2 %>% select(input$reverse_items2)
      reversed2 <- abs(toreverse2 - as.numeric(input$coding_scheme2))
      finalsub2 <- cbind(nottoreverse2,reversed2)
    }  
  })

  
  ####Return composite scales, means, and descriptives#######
  
  ####Variable 1####
  compositevar1 <- reactive({
    rowMeans(final_scaleitems1(),na.rm=TRUE)
  })
  
  output$show_scale1 <- renderTable({
    scale1 <- cbind(compositevar1(),final_scaleitems1())
    colnames(scale1)[1] = "commit_mean"
    head(scale1)
  })
  
  output$descriptives1 <- renderPrint({
    describe(compositevar1())
  })
  
  ####Variable 2####
  compositevar2 <- reactive({
    rowMeans(final_scaleitems2(),na.rm=TRUE)
  })
  
  output$show_scale2 <- renderTable({
    scale2 <- cbind(compositevar2(),final_scaleitems2())
    colnames(scale2)[1] = "sat_mean"
    head(scale2)
    
  })
  
  output$descriptives2 <- renderPrint({
    describe(compositevar2())
  })
  
  
  ####Generate final, uploadable table#####
  
  cleantable <- reactive({
    cleaned <- cbind(compositevar1(),compositevar2(),final_scaleitems1(),final_scaleitems2())
    colnames(cleaned)[1] = "commit_mean"
    colnames(cleaned)[2] = "sat_mean"
    as.data.frame(cleaned)
  })
  
  output$csvtable <- downloadHandler(
    filename = function () {
      paste(input$username, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(cleantable(), file, row.names=FALSE)
    }
  )
  
  ####Writing the data to SQL#####
  data_output <- observeEvent(input$submitButton,
                              saveData(cleantable(),input$username))
  
  
  
##End of App##
}


