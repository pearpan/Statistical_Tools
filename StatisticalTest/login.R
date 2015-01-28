write("","blank.html")
require(shiny)
addResourcePath("login",tools:::file_path_as_absolute("https://github.com/imvu/data-insights/raw/master/jobs/Statistical_Tool/StatisticalTest.zip"))
runApp(list(
  ui=bootstrapPage(
    tags$form(action="#",target="loginframe",
              tags$input(id="username",type="text",placeholder="Username"),
              tags$input(id="password",type="password",placeholder="Password"),
              tags$input(id="loginButton",class="btn btn-primary",type="submit",value="Login")
    ),
    tags$iframe(name="loginframe",src="login/blank.html",style="display:none")
  ),
  server=function(input, output) {
    observe({message(
      "username ",input$username,"\n",
      "password ",input$password,"\n"
    )})
  })
)