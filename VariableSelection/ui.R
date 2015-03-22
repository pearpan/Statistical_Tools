shinyUI(fluidPage(
  
  # Application title
  titlePanel("Welcome to DI Variable Selection Lab (Beta 1.0)"),
  
  # Sidebar with a slider input for number of observations
  sidebarLayout(
    
    sidebarPanel( 
      fileInput('datafile', 'upload the data file',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv',
                  '.txt'
                )
      ),
      
      #tags$hr(),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t',
                     Space=' '),
                   selected=','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   selected='"'),
      uiOutput("response"),
      uiOutput("variables"),
      br(),
      checkboxInput('weights', 'Over sampling rare events?', FALSE),
      actionButton("calculate", "Click to Calculate"),
      helpText("You must click the above button to start the variable selection process!")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Preview", 
                 h4('data preview:'),
                 tableOutput('preview'), 
                 h4('you picked following variables:'),
                 tableOutput('selected')),
        tabPanel("Summary", 
                 tableOutput("summary_table"),
                 plotOutput("summary_plot")
                 )
        
      )          
    )
  )
))#End of UI
