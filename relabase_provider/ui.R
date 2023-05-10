#
# 
####Updating to NavbarPage######

library(shiny)
library(tidyverse)
library(data.table)
library(sortable)
library(shinyjs)
library(psych)
library(DBI)
library(odbc)


#################UI############################

ui <- navbarPage("Relabase",
                 theme=bslib::bs_theme(bootswatch="flatly"),
    
#####Page 1: Upload Your Data and Describe its Features####
  tabPanel("Provide Your Dataset",
       sidebarLayout(
          sidebarPanel(
                 textInput("username", "What is your username?"),
                 numericInput("longitudinal", "How many timepoints does your
                                             dataset have?",value=1),
                 numericInput("dyadic", "How many partners per relationship participated in your study? (E.g., in
                                        a dyadic study, two partners participate)", value=1),
                 dateRangeInput("datadates", "When were these data collected?"),
                 fileInput("uploadeddata","Upload dataset",accept=".csv"),
               ),
          
          mainPanel(
            textOutput("features")
          )
            )  
          ),

####Page 2: Select relevant measure####
tabPanel("Select Variables",
         
         ###Come back to this and make a longer list of measures
         
         checkboxGroupInput("all_measures", "Which of the following measure(s) do you have in your dataset? Please check all that apply",
                            c("Commitment" = "Commit",
                              "Satisfaction" = "Sat",
                              "Investment" = "Inv"),
         )
),  



############Page 3: Select relevant items###############

navbarMenu("Select Your Items",
           
           ####Variable 1#####
           
           tabPanel("Commitment",
                    sidebarLayout(
                      sidebarPanel(        
                        #input text to subset variable names
                        textInput(
                          inputId = "subsetChooseListText1",
                          label = "Search for your items",
                          value = "c"
                        ),
                        uiOutput("selection_list1", style="flex:1 0 200px;"),
                        
                      ),
                      
                      mainPanel(
                        
                        #Drag-and-Drop boxes
                        h5("Please identify the items representing commitment, including only one version of each item."),
                        rank_list(
                          text = "Drop commitment items here",
                          labels = list(),
                          input_id = "rank_scale1",
                          options = sortable_options(group = "mygroup1",multiDrag = TRUE)
                        )
                      )
                      
                    )
           ),
           
           ####Variable 2#####
           
           tabPanel("Satisfaction",
                    sidebarLayout(
                      sidebarPanel(        
                        #input text to subset variable names
                        textInput(
                          inputId = "subsetChooseListText2",
                          label = "Search for your items",
                          value = "c"
                        ),
                        uiOutput("selection_list2", style="flex:1 0 200px;"),
                        
                      ),
                      
                      mainPanel(
                        
                        #Drag-and-Drop boxes
                        h5("Please identify the items representing commitment, including only one version of each item."),
                        rank_list(
                          text = "Drop satisfaction items here",
                          labels = list(),
                          input_id = "rank_scale2",
                          options = sortable_options(group = "mygroup2",multiDrag = TRUE)
                        )
                      )
                      
                    )
           )
),

############Page 4: Code Your Variable###############

navbarMenu("Code Your Variable",
           
           ####Variable1####
           tabPanel("Commitment",
                    sidebarLayout(
                      sidebarPanel(
                        
                        ###Provide more details about the measure####    
                        radioButtons("coding_scheme1", label=("What size scale did you use for your commitment
                                         measure?"),
                                     choices=c("1-5 Scale"=6,"1-6 Scale"=7,"1-7 Scale"=8,"1-9 Scale"=10, "Other"=0),
                                     selected=8),
                        
                        #indicate which items need to be reverse-scored
                        uiOutput("reverse_button1"),
                        
                        radioButtons("reverse_coded1", label=("Have the reverse-scored items already been recoded?"),
                                     choices=c("Yes, they are already recoded"=0,"No, Relabase should recode them"=1,
                                               "NA. This scale has no reverse-coded items"=2),
                                     selected=0)
                      ),
                      
                      #####Display measure information#####   
                      mainPanel(      
                        h3("Variable Summary"),
                        h4("Coded Items"),
                        tableOutput("show_scale1"),
                        h4("Descriptive summary"),
                        verbatimTextOutput("descriptives1"),
                        
                        
                        ##Trying to get rank_lists to line up nicely
                        uiOutput("invis_bucket")
                      )
                    )
           ),
           
           
           ####Variable2####
           tabPanel("Satisfaction",
                    sidebarLayout(
                      sidebarPanel(
                        
                        ###Provide more details about the measure####    
                        radioButtons("coding_scheme2", label=("What size scale did you use for your satisfaction
                                         measure?"),
                                     choices=c("1-5 Scale"=6,"1-6 Scale"=7,"1-7 Scale"=8,"1-9 Scale"=10, "Other"=0),
                                     selected=8),
                        
                        #indicate which items need to be reverse-scored
                        uiOutput("reverse_button2"),
                        
                        radioButtons("reverse_coded2", label=("Have the reverse-scored items already been recoded?"),
                                     choices=c("Yes, they are already recoded"=0,"No, Relabase should recode them"=1,
                                               "NA. This scale has no reverse-coded items"=2),
                                     selected=0)
                      ),
                      
                      #####Display measure information#####   
                      mainPanel(      
                        h3("Variable Summary"),
                        h4("Coded Items"),
                        tableOutput("show_scale2"),
                        h4("Descriptive summary"),
                        verbatimTextOutput("descriptives2"),
                        
                        
                      )
                    )
           )
),



###########Page 5: Submit Data to Repository#########

tabPanel("Submit Your Data",
         h3("Your cleaned and coded data file is available for download!"),
         downloadButton("csvtable","Download your file"),
         
         h3("This button will submit your dataset to the repository.
         Are you ready to submit?"),
         actionButton("submitButton","Submit"),
)

      

##End of App##
)



