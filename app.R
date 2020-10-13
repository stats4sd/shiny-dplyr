load("miniImdb.Rdata")
init="miniImdb"
#filter, select, mutate, group_by+summarise

library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(shinyAce)

ui <- fluidPage(

    # Application title
    titlePanel("Title"),


    fluidRow(column(width = 3,
       radioButtons("verb","Select dplyr function",choices=c("filter","select","arrange","mutate","group_by","summarise","slice"))),
       column(width = 8,
              
              aceEditor(
                outputId = "ace",
                # to access content of `selectionId` in server.R use `ace_selection`
                # i.e., the outputId is prepended to the selectionId for use
                # with Shiny modules
                selectionId = "selection",
                value = init, fontSize=16,height="200px"
              ),actionButton("confirm","Confirm edits"),
              sliderInput("Slider","Run code up to line:",min=1,max=1,value=1,step = 1))),
       
       fluidRow(
        column(  width = 3,  
            ###FILTER###
            conditionalPanel(condition="input.verb == 'filter'",
            selectInput("col",
                        "Select column to filter on",
                        colnames(miniImdb)),
            radioButtons("Condition","Condition",choices = c("EQUAL TO (==)"="==",
                                                            "NOT EQUAL TO (!=)"="!=",
                                                            "GREATER THAN (>)"=">",
                                                            "LESS THAN (<)"="<")),
            textInput("VALUE","Value",value = ""
                      ),
            actionButton("Filter","Add to code")
            ),
            ###SELECT###
            conditionalPanel(condition="input.verb == 'select'",
                             selectInput("selects",
                                         "Select columns to select",
                                         colnames(miniImdb),multiple = TRUE)
                             ,
                             actionButton("Select","Add to code")),
            ###GROUP_BY###
            conditionalPanel(condition="input.verb == 'group_by'",
                             selectInput("groups",
                                         "Select columns to group by",
                                         colnames(miniImdb),multiple = TRUE)
                             ,
                             actionButton("GroupBy","Add to code")
                             ) ,
            
            ###ARRANGE###
            conditionalPanel(condition="input.verb == 'arrange'",
                             selectInput("Arrangement",
                                         "Select columns to arrange by",
                                         colnames(miniImdb),multiple = TRUE),
                             conditionalPanel(condition="input.Arrangement != '' ",
                             selectInput("Reverse",
                                         "Which columns should be sorted in descending order?",
                                         colnames(miniImdb),multiple = TRUE)),
                             actionButton("Arrange","Add to code")
            ) 
            ,
            ###SUMMARISE###
            conditionalPanel(condition="input.verb == 'summarise'",
                             selectInput("summaries",
                                         "Select column to summarise",
                                         colnames(miniImdb)),
                             selectInput("stats",
                                         "Select summary statistics",
                                         choices=c("Number of observations n()"="length",
                                                   "Mean mean()"="mean",
                                                   "Median median()"="median",
                                                   "Minimum min()"="min",
                                                   "Maximum max()"="max",
                                                   "Standard deviation sd()"="sd"),multiple = TRUE),
                             actionButton("Summarise","Add to code")),
            ###SLICE###
            conditionalPanel(condition="input.verb == 'slice'",
                             sliderInput("slices",
                                         "Enter range of rows to slice",min=1,max=20,value=c(1,5),step = 1),
                             
                             actionButton("Slice","Add to code")
            ) ,
            
            ###MUTATE###
            conditionalPanel(condition="input.verb == 'mutate'",
                             textInput("newname",
                                         "Enter name for column","newcolumn"),
                             textInput("formula",
                                       "Enter formula for column","1"),
                             actionButton("Mutate","Add to code")
            ) 
            ,
            
            ##RESET##
            actionButton("Undo","Undo last line"),
            actionButton("Redo","Redo last line"),
            actionButton("Reset","Reset all (cannot be undone)")
        ),

     
       
    column(width=8,tableOutput("data")
             )
       )
    
        )


# Define server logic required to draw a histogram
server <- function(input, output,session) {

    ##Set up initial calues of code and data
  
    code<-reactiveValues()
    code$code<-c("miniImdb",rep("",1000))
    code$tidycode<-"miniImdb"
    code$count<-1
    data<-reactiveValues()
    data$data<-miniImdb
    
    #Reset all back to original state if button pressed
    
    observeEvent(input$Reset,{
      code$code<-c("miniImdb",rep("",1000))
      code$tidycode<-"miniImdb"
      code$count<-1
        data$data<-miniImdb
        updateSelectInput(session = session,"col",choices=colnames(miniImdb))
        updateSelectInput(session = session,"selects",choices=colnames(miniImdb))
        updateSelectInput(session = session,"summaries",choices=colnames(miniImdb))
        updateSelectInput(session = session,"groups",choices=colnames(miniImdb))
        updateSelectInput(session = session,"Arrangement",choices=colnames(miniImdb))
        
        })
    
    
    
    observeEvent(input$Slider,{
    updateSelectInput(session = session,"col",choices=colnames(data$data))
    updateSelectInput(session = session,"selects",choices=colnames(data$data))
    updateSelectInput(session = session,"summaries",choices=colnames(data$data))
    updateSelectInput(session = session,"Arrangement",choices=colnames(data$data))
    })
    
    observeEvent(input$Undo,{
      if(code$count>1){
      code$count<-code$count-1
      data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))

      }
    })
    observeEvent(input$Redo,{
      if(code$code[code$count+1]!=""){
        code$count<-code$count+1
        data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
   
      }
    })
    
    #updateSlider
    observeEvent(code$count,{
      updateSliderInput(session=session,"Slider",max=code$count)
    })
    
    
    ###FILTER###
    
    observeEvent(input$Filter,{
      code$count<-code$count+1

       if(class(miniImdb[,input$col])=="numeric"|class(miniImdb[,input$col])=="integer"){
        code$code[code$count]<-paste0("filter(",input$col,input$Condition,input$VALUE,")")
        }
        else{
          code$code[code$count]<-paste0("filter(",input$col,input$Condition,"'",input$VALUE,"'",")")
     
        }
        data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
            }
                 )
    
    ###SELECT###
    
    observeEvent(input$Select,{
      code$count<-code$count+1
     
      code$code[code$count]<-paste0("select(",paste(input$selects,collapse=","),")") 
       
            data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
            
            
            
    }
    )
    
    ###SUMMARISE###
    
    observeEvent(input$Summarise,{
      code$count<-code$count+1
      
      s1<-input$stats
      s1[s1=="length"]<-"n"
        code$code[code$count]<-paste0("summarise(",paste(paste(s1,"=",input$stats,"(",input$summaries,")"),collapse=","),")")  

        data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
   
    }
    )

    ###GROUP_BY###    
    
    observeEvent(input$GroupBy,{
      code$count<-code$count+1
        code$code[code$count]<-paste0("group_by(",paste(input$groups,collapse=","),")")  
   
        data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
      }
    )
    
    ###ARRANGE###
    
    
    observeEvent(input$Arrangement,{
      updateSelectInput(session,"Reverse",choices = input$Arrangement)
    })
    
    observeEvent(input$Arrange,{
      code$count<-code$count+1
      
      Arrangement<-ifelse(!input$Arrangement%in%input$Reverse,
                          input$Arrangement,
                          paste0("desc(",input$Arrangement,")"))
      
        code$code[code$count]<-paste0("arrange(",paste(Arrangement,collapse=","),")")
     
        data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
    }
    )
    
    ###Slice###
    
    observeEvent(input$Slice,{
      code$count<-code$count+1
      code$code[code$count]<-paste0("slice(",input$slices[1],":",input$slices[2],")")
      
      data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
    }
    )
    
    #MUTATE
    observeEvent(input$Mutate,{
      code$count<-code$count+1
      code$code[code$count]<-paste0("mutate(",input$newname,"=",input$formula,")")
      
      data$data<-eval(parse(text=paste(code$code[1:code$count],collapse=" %>% ")))
    }
    )
    
    
    
    #Output code
    output$code <- renderTable({
        tc<-code$code[1:code$count]
        tc[-length(tc)]<-paste(tc[-length(tc)]," %>%")
        data.frame(Code=tc)
           })
 
 
    
observeEvent(code$count,{
   
      code$tc<-code$code[1:code$count]
      code$tc[-length(code$tc)]<-paste(code$tc[-length(code$tc)]," %>%")
      code$tc<-paste(code$tc,collapse="\n")

      updateAceEditor(session,editorId = "ace",value = code$tc )
    })

observeEvent(input$confirm,{
  x<-trimws(gsub("%>%","",unlist(strsplit(input$ace,"\n"))))
  code$code[1:length(x)]<-x
  code$count<-length(x)
})

    
    #Output data
    output$data <- renderTable({
      
    eval(parse(text=paste(code$code[1:as.numeric(input$Slider)],collapse=" %>% ")))
     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
