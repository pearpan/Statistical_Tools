shinyUI(fluidPage(
  
  # Application title
  titlePanel("Welcome to DI statistical testing Lab (beta 1.0)"),
  
  # Sidebar with a slider input for number of observations
  sidebarLayout(
    
    sidebarPanel(
      selectInput("TestType","Type of test",
                  c("two sample proportion test","two sample t-test")),
      conditionalPanel(#only show this panel if proportional test is selected)
        condition = "input.TestType == 'two sample proportion test'",
        numericInput("n1", "sample size of test group:", 10000, min = 20),
        numericInput("n2", "sample size of control group:", 10000,min = 20),
        numericInput("value1", "test group converstion rate (input a value from 0 to 1):",0.11,min=0,max=1),
        numericInput("value2", "control group conversion rate (input a value from 0 to 1):",0.1,min=0,max=1)
      ),
      conditionalPanel(#only show this panel if two sample t-test is selected)
        condition = "input.TestType == 'two sample t-test'",
        fileInput('testfile', 'upload test group data',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )#,
                  #helpText("Only 20 top lines are shown in the page")
        ),
        
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      ),
      sliderInput("significanceLevel", "Threshold of Significance Level:", 
                  min=0, max=0.3, value=0.05, step = 0.01),
      sliderInput("samplePower", "Power:", 
                  min=0.6, max=1, value=0.8, step = 0.05),
      selectInput("valueToCalculate","Value to calculate",
                  choices = c('significant level(two-sided)','significant level(one-sided)','power','sample size')),
      br(),
      actionButton("calculate", "Calculate",icon='Click to Calculate'),
      helpText("You must click the above button to get the testing result, otherwise you will only see the sample output of default settings!")
      
    ),
    
    mainPanel(
      h2("traditional statistical test"),
      h3("Conclusion and details"),
      verbatimTextOutput("traditional_test"),
      h2("Winning probability"),
      verbatimTextOutput("win_prob"),
      tableOutput('contents'),
      imageOutput("logo")
    )
  )
))#End of UI
