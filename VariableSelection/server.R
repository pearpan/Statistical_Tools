#source('global.R')
options(shiny.maxRequestSize=1024^3)
shinyServer(function(input, output){
  
  #logo
  output$logo <- renderImage({
    filename <- 'www/lottery_girls.jpg'
    list(src = filename,
         width = 272,
         height = 153,
         alt = "Statistical Testing Lab")
    
  }, deleteFile = FALSE)
  
  #data preview_test
  output$preview <- renderTable({
    inFile <- input$datafile
    if (is.null(inFile)){
      return(NULL)
    }else{
      read.csv(inFile$datapath, header = TRUE,sep = input$sep, quote = input$quote,nrow=10)
    }   
  })
  
  Dataset <- reactive({
    inFile <- input$datafile
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath, header = TRUE,sep = input$sep, quote = input$quote,nrow=10)
  })
  
  output$response <- renderUI({    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    cols <- names(Dataset())
    selectInput("dependent.var", "Select a response variable:",choices=cols, selected=cols, multiple=F)     
  })
  
  output$variables <- renderUI({    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    cols <- names(Dataset())
    selectInput("independent.variables", "Select all the variables of interests:",choices=cols, multiple=T)     
  })
  
  output$selected <- renderTable({
    independent.variables <- input$independent.variables
    dependent.var <- input$dependent.var
    if (is.null(independent.variables) || length(independent.variables)==0) return(NULL)
    if (is.null(dependent.var) || length(dependent.var)==0) return(NULL)
    #attributes <- data.frame(Dataset()[, independent.variables])
    #y <- Dataset()[,dependent.var]
    Dataset()[,independent.variables,drop=FALSE]
  })
  
  
})
