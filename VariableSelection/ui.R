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
                  '.tsv'
                )
      ),
      
      #tags$hr(),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   selected=','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   selected='"'),
      uiOutput("response"),
      uiOutput("variables"),
      br(),
      actionButton("calculate", "Click to Calculate"),
      helpText("You must click the above button to get the testing result, otherwise you will only see the sample output of default settings!")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Preview", 
                 h4('data preview:'),
                 tableOutput('preview'), 
                 h4('you picked following variables:'),
                 tableOutput('selected')),
        tabPanel("Summary", verbatimTextOutput("summary"))
        
      )    
      
    )
  )
))#End of UI
