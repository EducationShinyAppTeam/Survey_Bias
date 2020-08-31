# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(shinyDND)
library(png)
library(shinyjs)
library(shinyWidgets)
library(V8)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Survey bias"
APP_DESCP  <<- paste(
  "Description of the app",
  "use multiple lines to keep the description legible."
)
# End App Meta Data------------------------------------------------------------

# Load additional dependencies and setup functions
# source("global.R")
bank<- read.csv("easyQuestions.csv")
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# Define UI for App
ui <- list(
  
  ## Create the app page
  dashboardPage(
    skin = "red",
    ### Create the app header
    dashboardHeader(
      title = "Survey bias",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        width = 250,
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("Game", tabName = "Game", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
     tabItems(
    #### Set up the Overview Page
    tabItem(
     tabName = "Overview",
      withMathJax(),
       h1("Survey bias for BOAST Apps"), # This should be the full name.
          p("This app illustrate the different types of biases that occur in the
            wording of survey questions."),
          h2("Instructions"),
          p("On the first page, simply click below each question that contains a
            bias to see what that bias is."),
          tags$ol(
            tags$li("Pay attention! Because on the second page, you will be asked
                    to match questions with their appropriate bias."),
            tags$li("Note: you will be timed.  Each round will continue to increase 
                    in difficulty."),
            tags$li("Challenge yourself."),
            tags$li("For the last round, please note that some of the biases overlap.
                    So while one may seem fitting, it could be marked incorrect 
                    if there is a more dominant bias.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Xigang Zhang and
            Yuxin Zhang.",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/13/2020 by NJH.")
          )
        ),
        #### Set up the Explore Page
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore"),
          fluidRow(
            
      theme = "custom.css",
      box(title = "What's the big deal about Survey Bias?", status = "success", 
                solidHeader = TRUE, width = 7,
                
                "Surveys help us understand public opinion on many topics.  
          While surveys may seem easy to create, there are some common pitfalls 
          in question wording to watch out for.  Look at the questions below to see an example of each.",
                
                
                br(),
                br(),
                br(),
                strong("Deliberate Bias (one-sided wording): "), " It is hard 
                    for today's college graduates to have a bright future 
                with the way things are today in the world. Agree or Disagree.",
                br(),
                br(),
                actionButton("runif", "Remove the bias!"),
                br(),
                br(),
                tags$head(tags$style("#text_example{color: red;
                                     font-size: 14px;
                                     font-style: bold;
                                     }"
                         )
                ),
                strong(textOutput("text_example")),
                br(),
                br(),
                strong("Filtering (missing options): "), "What is your opinion 
                      of our current President?",
                br(), "a. Favorable", br(), "b. Unfavorable",
                br(),
                br(),
                actionButton("runif1", "Remove the bias!"),
                br(),
                br(),
                tags$head(tags$style("#text_example1{color: red;
                                     font-size: 14px;
                                     font-style: bold;
                                     }"
                         )
                ),
                strong(textOutput("text_example1")),
                br(), br(),
                
                strong("Anchoring : "), "Knowing that the population of the U.S.
                            is 316 million, what is the population of Canada?",
                br(), br(),
                actionButton("runif3", "Remove the bias!"),
                br(), br(),
                tags$head(tags$style("#text_example3{color: red;
                                     font-size: 14px;
                                     font-style: bold;
                                     }"
                         )
                ),
                strong(textOutput("text_example3")),
                br(), br(),
                strong("Unintentional Bias (use of loaded words): "), "Do you 
            favor or oppose an ordinance that ", em("forbids "), "surveillance 
                cameras to be placed on Beaver Ave?",
                br(), br(),
                actionButton("runif4", "Remove the bias!"),
                br(),br(),
                tags$head(tags$style("#text_example4{color: red;
                                     font-size: 14px;
                                     font-style: bold;
                                     }"
                         )
                ),
                strong(textOutput("text_example4")),
                br(), br(),
                strong("Unnecessary Complexity (double-barreled questions): "), 
                "Do you think that health care workers and military
                personnel should be the first to receive the smallpox vaccination?",
                br(), br(),
                actionButton("runif5", "Remove the bias!"),
                br(), br(),
                tags$head(tags$style("#text_example5{color: red;
                                     font-size: 14px;
                                     font-style: bold;
                                     }"
                         )
                ),
                strong(textOutput("text_example5")),
                br(), br(),
                strong("Unnecessary Complexity (Double Negatives): "), "Do you 
                disagree that obese children should not be allowed to spend a 
            lot of time watching television, playing computer games, or listening 
                to music?",
                br(), br(),
                actionButton("runif6", "Remove the bias!"),
                
                tags$head(tags$style("#text_example6{color: red;
                                     font-size: 14px;
                                     font-style: bold;
                                     }"
                         )
                ),
                strong(textOutput("text_example6")),
                
                
                useShinyjs(),
                extendShinyjs(text = jsResetCode),
                actionButton("reset_button", "Return to Overview!")
                
                
                ),
            
          box(title = "Did you Know...", status = "warning", solidHeader = TRUE, 
                width = 5, 
                img(src = 'truman.png', align= "right"),
                " For the 1948 election between Thomas Dewey and Harry Truman, 
                Gallup conducted a poll with a sample size of about 3250. Each 
                individual in the sample was interviewed in person by a professional 
                interviewer to minimize nonresponse bias, and each interviewer was 
                given a very detailed set of quotas to meet (rather than being 
                given a random sample of specific people to contact).",
                br(),
                br(),
                "For example, an interviewer could have been given 
                the following quotas: seven white males under 40 living in a 
                rural area, five black males under 40 living in an rurban area, 
                six black females under 40 living in a rural area, etc. Other 
                than meeting these quotas the ultimate choice of who was 
                interviewed was left to each interviewer.",
                           
                           br(),
                           br(),
                           
                           "Based on the results of this poll, Gallup predicted 
                a victory for Dewey, the Republican candidate. 
                The predicted breakdown of the vote was 50% for Dewey, 44% for 
                Truman, and 6% for third-party candidates Strom Thurmond and 
                Henry Wallace. The actual results of the election turned out to 
                be almost exactly reversed:50% for Truman, 45% for Dewey, 
                and 5% for third-party candidates.",
                           
                           br(),
                           br(),
                           
                           "Truman's victory was a great surprise to the nation 
                      as a whole. So convinced was the Chicago Tribune of Dewey's 
                victory that it went to press on its early edition for November 4, 
                1948 with the headline",
                           strong("Dewey defeats Truman"),
                           
                           br(),
                           br(),
                           "The Gallup Poll learned the lesson that the biases of 
        quota based polling can be alleviated by using random sampling techniques. 
       Check out", a("their site", href="http://www.gallup.com"), " to see more!"
                           
                       )
                         )
                         ),

        #### Set up an Game Page
        tabItem(
          tabName = "Game",
          withMathJax(),
          h2("Survey Bias Game"),
          fluidRow(theme = "bootstrap.css",
                   
                   tabsetPanel( id = "gamelevel",
                              
                              tabPanel(title = "Directions", value = "a",
                        "This is a three level game to test if you 
                         can recognize the types of biases described in this app. 
                         Each level will consist of 4 questions that contain a bias.  
                         Match the question with the bias that it contains.  
                         As the levels get harder, some questions will contain 
                                       multiple biases.  Only choose one!",
                                       
                                       
                                       br(), br(),
        "There is a timer that will start as soon as you begin the game.  
         For each question you get wrong, you will be deducted 2 points and each 
         question you get right, you will be awarded 2 points.  In order to move 
         from easy to medium to hard, you will need to fully finish each level.  
       At the end of the second round, the timer will stop after all answers are 
     submitted correctly.  Your score will be compiled and if it is a top score, 
        it will make the leader board.",
                                       
               br(),
               br(),
               h2("Ready?"),
                                       
   fluidRow(column(1,offset = 5,
   bsButton(inputId = "go2", label = "G O !" ,style = "danger",size = "large"))
                                       )
                              ),
                              
    ########Level Easy
    tabPanel("Level A",value = "b",
    fluidPage(theme = "bootstrap.css", #css theme
    tags$style(type='text/css', '#timer1 {background-color:#2C3E50; font-size: 20px; 
 color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'), 
    #link to your own css file
    titlePanel("Select approaite Bias to each question. "),
    fluidRow(column(3,offset = 9,textOutput("timer1"))),br(), 
    conditionalPanel("input.go != 0", #Show everything only after the GO button is clicked
    #Set up all dropdownUIs which are randomly chosen from the question bank 
                                                                  
   h3("Choose the bias for the following: "),
    uiOutput('filteringName1'), 
    fluidRow(column(width = 9,
    selectInput(inputId = 'first', label = "Bias Type", c("Select Answer",'filtering', 
                                      'deliberate', "anchoring"), width = '30%')
    ),
   column(width = 2, offset = 1,uiOutput('answer2')
                                                                                           
   )),
                                                  
                                                                           
                                                                           
                                                                           
  uiOutput('deliberateName1'), 
  fluidRow(column(width = 9,
  selectInput(inputId = 'second', label = "Bias Type", c("Select Answer",'filtering', 
                                      'deliberate', "anchoring"), width = '30%')
  ),
  column(width = 2, offset = 1,uiOutput('answer3')
                                                                                           
  )),
                                                                           
  uiOutput('anchoringName1'),
  fluidRow(column(width = 9,
  selectInput(inputId = 'third', label = "Bias Type", c("Select Answer",'filtering', 
                                      'deliberate', "anchoring"), width = '30%')
  ),
  column(width = 2, offset = 1,uiOutput('answer4')
                                                                                           
  )),
                                                                           
                                                              
  hr(),
                                                                  
                                                                
   #Submit button and pagination button
  fluidRow(
  column(1,bsButton("prev2","<<Previous", style = "danger",size = "small")),
  column(1,offset = 4, conditionalPanel("(input.filtering!='') & (input.anchoring!='') 
                                        & (input.deliberate!='')"
  ,bsButton("submitA", "Submit Answer", style = "danger",size = "small",class = "grow"))),
   column(1,offset = 5,bsButton("next1","Next>>",style = "danger", size = "small", disabled = TRUE))
   ),br(),
                                                                  
  conditionalPanel("input.submitA != 0",wellPanel(
  fluidPage(
  fluidRow(
  div(style = "position:absolute; top:8em; right:2em",bsButton("Reset","Reset",style = "danger")),
  class = "wellTransparent col-lg-8"),
  wellPanel(h4("Full score is 30 for Easy Level."),
  verbatimTextOutput("scoreA"),class = "wellTransparent col-lg-4")
  ))))                
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 )),
   tabPanel("LevelB",value = "c",
   fluidPage(theme = "bootstrap.css", #css theme
   tags$style(type='text/css', '#timer2 {background-color:#2C3E50; font-size: 20px; 
   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'), 
   #link to your own css file
    titlePanel("Select approaite Bias to each question. "),
    fluidRow(column(3,offset = 9,textOutput("timer2"))),br(), 
                                                 
    conditionalPanel("input.next1 != 0", #Show everything only after the next button is clicked
    #Set up all dragUIs which are randomly chosen from the question bank 
    h3("Choose the bias for the following: "),
    uiOutput('unnecessaryName1'), 
    fluidRow(column(width = 9,
                    selectInput(inputId = 'forth', label = "Bias Type", c("Select Answer",'unnecessary', 
                                                                          'nonbias', "unintentional"), width = '30%')
    ),
    column(width = 2, offset = 1,uiOutput('answer6')
           
    )),
    
    uiOutput('nonbiasName1'), 
    fluidRow(column(width = 9,
                    selectInput(inputId = 'fifth', label = "Bias Type", c("Select Answer",'unnecessary', 
                                                                          'nonbias', "unintentional"), width = '30%')
    ),
    column(width = 2, offset = 1,uiOutput('answer7')
           
    )),
    
    uiOutput('unintentionalName2'),
    fluidRow(column(width = 9,
                    selectInput(inputId = 'sixth', label = "Bias Type", c("Select Answer",'unnecessary', 
                                                                          'nonbias', "unintentional"), width = '30%')
    ),
    column(width = 2, offset = 1,uiOutput('answer8')
           
    )),
    
    
    hr(),
                                                                  
                                                             
   #Submit button and pagination button
 fluidRow(
   column(1,bsButton("prev1","<<Previous", style = "primary",size = "small")),
   column(1,offset = 4, conditionalPanel("(input.unnecessary!='') & (input.nonbias!='') & (input.unintentional!='')"
                                         ,bsButton("submitB", "Submit Answer", style = "primary",size = "small",class = "grow"))),
   column(1,offset = 5,bsButton("next2","Next>>",style = "primary", size = "small", disabled = TRUE))
 ),br(),
 
 conditionalPanel("input.submitB != 0",wellPanel(
   fluidPage(
     fluidRow(
       div(style = "position:absolute; top:8em; right:2em",bsButton("Reset","Reset",style = "danger")),
       class = "wellTransparent col-lg-8"),
     wellPanel(h4("Full score is 30 for Medium Level."),
               verbatimTextOutput("scoreB"),class = "wellTransparent col-lg-4")
   ))))                
   )),
  tabPanel("LevelC",value = "d",
  fluidPage(theme = "bootstrap.css", #css theme
  tags$style(type='text/css', '#timer3 {background-color:#2C3E50; font-size: 20px; 
  color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'), #link to your own css file
  titlePanel("Select approaite Bias to each question. "),
  fluidRow(column(3,offset = 9,textOutput("timer3"))),br(), 
                                                 
  conditionalPanel("input.next2 != 0", #Show everything only after the GO button is clicked
  #Set up all dragUIs which are randomly chosen from the question bank 
  h3("Choose the bias for the following: "),
  uiOutput('unintentionalName1'), 
  fluidRow(column(width = 9,
                  selectInput(inputId = 'seventh', label = "Bias Type", c("Select Answer",'filtering', 
                                                                          'unnecessary','nonbias' ,"unintentional"), width = '30%')
  ),
  column(width = 2, offset = 1,uiOutput('answer9')
         
  )),
  
  uiOutput('unnecessaryName2'), 
  fluidRow(column(width = 9,
                  selectInput(inputId = 'eighth', label = "Bias Type", c("Select Answer",'filtering', 
                                                                         'unnecessary','nonbias' ,"unintentional"), width = '30%')
  ),
  column(width = 2, offset = 1,uiOutput('answer11')
         
  )),
  
  uiOutput('filteringName2'),
  fluidRow(column(width = 9,
                  selectInput(inputId = 'ninth', label = "Bias Type", c("Select Answer",'filtering', 
                                                                        'unnecessary','nonbias' ,"unintentional"), width = '30%')
  ),
  column(width = 2, offset = 1,uiOutput('answer10')
         
  )),
  
  uiOutput('nonbiasNAME2'),
  fluidRow(column(width = 9,
                  selectInput(inputId = 'tenth', label = "Bias Type", c("Select Answer",'filtering', 
                                                                        'unnecessary','nonbias' ,"unintentional"), width = '30%')
  ),
  column(width = 2, offset = 1,uiOutput('answer12')
         
  )),
  
  
  hr(),
                                                                  
                                                                  
    #Submit button and pagination button
  fluidRow(
    column(1,bsButton("prev3","<<Previous", style = "primary",size = "small")),
    column(1,offset = 4, conditionalPanel("(input.unintentional!='') & (input.unnecessary!='') & (input.filtering!='') & (input.nonbias!='')"
    ,bsButton("submitC", "Submit Answer", style = "primary",size = "small",class = "grow")))),
  br(),
  
  
  conditionalPanel("input.submitB != 0",wellPanel(
    fluidPage(
      fluidRow(
        div(style = "position:absolute; top:8em; right:2em",bsButton("Reset","Reset",style = "danger")),
        class = "wellTransparent col-lg-8"),
      column(1,offset = 5,bsButton("finish","STOP>>", style = "danger", disabled = TRUE, size = "small")),
      wellPanel(h4("Full score is 40 for Hard Level."),
                verbatimTextOutput("scoreC"),class = "wellTransparent col-lg-4")
    ))))                
  )),
        #### Set up the References Page-REQUIRED
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
            [R Package]. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          )
        )
      )
))))))

# Define server logic
bank <- data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click buttons to see improved wordings.",
      type = "info"
    )
  })
  
  observeEvent(input$info1, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Drag pink rectangles with questions to the correct category box.",
      type = "info"
    )
  })
  #Reset Button For Main Page
  observeEvent(input$reset_button, {js$reset()}) 
  
  
  ###go button
  observeEvent(input$go1, {
    updateTabItems(session,"tabs","Explore")
  })
  
  ########Timer Info
  time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)
  
  observe({
    time$timer()
    if(isolate(time$started))
      time$inc<-isolate(time$inc)+1
  })
  
  
  observeEvent(input$go2, {time$started<-TRUE})
  observeEvent(input$submitA, {time$started <- FALSE})
  observeEvent(input$next1, {time$started <- TRUE})
  observeEvent(input$submitB, {time$started <- FALSE})
  observeEvent(input$submitC, {time$timer<-reactiveTimer(Inf)})
  
  output$timer1 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer2 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer3 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer4 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer5 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  
  ######Back and Forth Buttons
  observeEvent(input$go2,{
    updateTabsetPanel(session = session,"gamelevel", selected = "b")
  })
  
  observeEvent(input$next1,{
    updateTabsetPanel(session = session,"LevelB", selected = "c")
  })
  
  observeEvent(input$next2,{
    updateTabsetPanel(session = session,"LevelC", selected = "d")
  })
  
  observeEvent(input$prev1,{
    updateTabsetPanel(session = session,"tabMain", selected = "b")
  })
  
  observeEvent(input$prev2,{
    updateTabsetPanel(session = session,"tabMain", selected = "a")
  })
  
  observeEvent(input$prev3,{
    updateTabsetPanel(session = session,"tabMain", selected = "c")
  })
  
  
  #Main Page
  observeEvent(input$runif,{
    output$text_example <- renderText({"Do you agree or disagree that it is hard 
      for today's college graduates to have a bright future?" })
  })
  
  
  
  observeEvent(input$runif1,{
    output$text_example1<- renderText({"What is your opinion of our current President? 
      a. favorable b. unfavorable c. undecided"})
  })
  
  observeEvent(input$runif2,{
    output$myImage <- renderImage({
      output$myImage <- renderImage({
        
        image_file <- paste("www/",input$image.type,".png",sep="")
        
        return(list(
          src = image_file,
          filetype = "importanceoforder.png",
          height = 250,
          width = 500
        ))
        
      }, deleteFile = FALSE)
      
      
      
    })
  })
  
  
  observeEvent(input$runif3,{
    output$text_example3<-renderText({"What is the population of Canada?"})
  })
  
  observeEvent(input$runif4, {
    output$text_example4<-renderText({"Do you favor or oppose an ordinance that does not allow 
      surveillance cameras to be placed on Beaver Avenue?"})
    })
  
  observeEvent(input$runif5, {
  output$text_example5<-renderText({"Who should have priority in receiving the smallpox vaccination? 
      a. health care workers 
      b. military personnel
      c. both health care workers and military personnel 
      d. neither"})
    })
  observeEvent(input$runif6,{
    output$text_example6<-renderText({"Do you agree or disagree that children who 
      have a Body Mass Index (BMI) at or above the 95th percentile should spend 
less time watching television, playing computer games, and listening to music?"})
  })
  
  #Judge Correctness
  numbers <- reactiveValues( dis = c())
  
  observeEvent(input$go2,{
    numbers$important = sample(2:6,1)
    numbers$filtering = sample(7:11,1)
    numbers$deliberate = sample(12:16,1)
    numbers$anchoring = sample(17:21,1)
    numbers$unintentional = sample(22:26,1)
    numbers$unnecessary = sample(27:31,1)
    numbers$nonbias = sample(32:35,1)
  })
  
  output$importantID1 <- renderText({
    bank[numbers$important[1], 2]
  })
  
  
  output$importantName1 <- renderText({
    bank[numbers$important[1],3]
  })
  
  output$unintentionalID1<- renderText({
    bank[numbers$unintentional[1],2]
  })
  
  output$unintentionalName1<- renderText({
    bank[numbers$unintentional[1],3]
  })
  
  output$unintentionalID2<- renderText({
    bank[numbers$unintentional[1],2]
  })
  
  output$unintentionalName2<- renderText({
    bank[numbers$unintentional[1],3]
  })
  
  output$filteringID1 <- renderText({
    bank[numbers$filtering[1], 2]
  })
  
  output$filteringName1 <- renderText({
    bank[numbers$filtering[1],3]
  })
  output$filteringID2 <- renderText({
    bank[numbers$filtering[1], 2]
  })
  
  output$filteringName2 <- renderText({
    bank[numbers$filtering[1],3]
  })
  
  
  output$anchoringID1 <- renderText({
    bank[numbers$anchoring[1], 2]
  })
  
  output$anchoringName1 <- renderText({
    bank[numbers$anchoring[1],3]
  })
  
  output$deliberateID1 <- renderText({
    bank[numbers$deliberate[1], 2]
  })
  
  output$deliberateName1 <- renderText({
    bank[numbers$deliberate[1],3]
  })
  
  output$unnecessaryID1 <- renderText({
    bank[numbers$unnecessary[1], 2]
  })
  
  output$unnecessaryName1 <- renderText({
    bank[numbers$unnecessary[1],3]
  })
  
  output$unnecessaryID2 <- renderText({
    bank[numbers$unnecessary[1], 2]
  })
  
  output$unnecessaryName2 <- renderText({
    bank[numbers$unnecessary[1],3]
  })
  
  output$deliberateID2 <- renderText({
    bank[numbers$deliberate[1], 2]
  })
  
  output$deliberateName2 <- renderText({
    bank[numbers$deliberate[1],3]
  })
  
  output$anchoringID2 <- renderText({
    bank[numbers$anchoring[1], 2]
  })
  
  output$anchoringName2 <- renderText({
    bank[numbers$anchoring[1],3]
  })
  
  output$nonbiasID1 <- renderText({
    bank[numbers$nonbias[1], 2]
  })
  
  output$nonbiasName1 <- renderText({
    bank[numbers$nonbias[1],3]
  })
  
  
  output$nonbiasID2 <- renderText({
    bank[numbers$nonbias[1], 2]
  })
  
  output$nonbiasNAME2 <- renderText({
    bank[numbers$nonbias[1],3]
  })
  
  observeEvent(input$submitA,{
    updateButton(session,"submitA",disabled = TRUE)
  })
  observeEvent(input$Reset,{
    updateButton(session,"submitA",disabled = FALSE)
  })
  observeEvent(input$submitB,{
    updateButton(session,"submitB",disabled = TRUE)
  })
  observeEvent(input$ResetB,{
    updateButton(session,"submitB",disabled = FALSE)
  })
  
  

  observeEvent(input$submitA,{  
      output$answer2 <- renderUI({
        if (!is.null(input$first)){
          if (input$first == bank[numbers$filtering,"Type"]){
            img(src = "check.png",width = 20)
          }else{
            img(src = "wrong.png",width = 20)
          }
        }
      })
      output$answer3 <- renderUI({
        if (!is.null(input$second)){
          if (input$second == bank[numbers$anchoring,"Type"]){
            img(src = "check.png",width = 20)
          }else{
            img(src = "wrong.png",width = 20)
          }
        }
      })
      output$answer4 <- renderUI({
        if (!is.null(input$third)){
          if (input$third == bank[numbers$anchoring,"Type"]){
            img(src = "check.png",width = 20)
          }else{
            img(src = "wrong.png",width = 20)
          }
        }
      })
  })


      observeEvent(input$Reset,{
        output$answer4 <- renderUI({
          img(src = NULL,width = 30)
        })
        output$answer2 <- renderUI({
          img(src = NULL,width = 30)
        })
        output$answer3 <- renderUI({
          img(src = NULL,width = 30)
        })
        score1 <- 0
        score2 <- 0
        score3 <- 0
      })


      observeEvent(input$submitB,{  
        output$answer2 <- renderUI({
          if (!is.null(input$forth)){
            if (input$forth == bank[numbers$unnecessary,"Type"]){
              img(src = "check.png",width = 20)
            }else{
              img(src = "wrong.png",width = 20)
            }
          }
        })
        output$answer3 <- renderUI({
          if (!is.null(input$fifth)){
            if (input$fifth == bank[numbers$filtering,"Type"]){
              img(src = "check.png",width = 20)
            }else{
              img(src = "wrong.png",width = 20)
            }
          }
        })
        output$answer4 <- renderUI({
          if (!is.null(input$sixth)){
            if (input$sixth == bank[numbers$nonbias,"Type"]){
              img(src = "check.png",width = 20)
            }else{
              img(src = "wrong.png",width = 20)
            }
          }
        })
      })
      
      
      observeEvent(input$ResetB,{
        output$answer6 <- renderUI({
          img(src = NULL,width = 30)
        })
        output$answer7 <- renderUI({
          img(src = NULL,width = 30)
        })
        output$answer8 <- renderUI({
          img(src = NULL,width = 30)
        })
        score4 <- 0
        score5 <- 0
        score6 <- 0
      })
      
      observeEvent(input$submitC,{  
        output$answer9 <- renderUI({
          if (!is.null(input$seventh)){
            if (input$seventh == bank[numbers$unintentional,"Type"]){
              img(src = "check.png",width = 20)
            }else{
              img(src = "wrong.png",width = 20)
            }
          }
        })
        output$answer10 <- renderUI({
          if (!is.null(input$eighth)){
            if (input$eighth == bank[numbers$unnecessary,"Type"]){
              img(src = "check.png",width = 20)
            }else{
              img(src = "wrong.png",width = 20)
            }
          }
        })
        output$answer11 <- renderUI({
          if (!is.null(input$ninth)){
            if (input$ninth == bank[numbers$filtering,"Type"]){
              img(src = "check.png",width = 20)
            }else{
              img(src = "wrong.png",width = 20)
            }
          }
        })
        output$answer12 <- renderUI({
          if (!is.null(input$tenth)){
            if (input$tenth == bank[numbers$nonbias,"Type"]){
              img(src = "check.png",width = 20)
            }else{
              img(src = "wrong.png",width = 20)
            }
          }
        })
      })

  #########Scoring
  summation <- reactiveValues(summationA = c(rep(0,20)), summationB = c(rep(0,20)),summationScore = c(rep(0,20)))
  observeEvent(input$submitA,{
    score1 = c()
    score2 = c()
    score3 = c()
    
    for (i in c(input$first)){
      if (i == bank[numbers$filtering,"Type"]){
        score1 = c(score1,10)
      }else{
        score1 = c(score1,10)
      }
    }
    for (i in c(input$second)){
      if (i == bank[numbers$filtering,"Type"]){
        score2 = c(score2, 10)
        }else{
          score2 = c(score2, 10)}
    }
    for (i in c(input$third)){
      if (i == bank[numbers$filtering,"Type"]){
        score3 = c(score3, 10)
        }else{
          score3 = c(score3, 10)}
    }
    
    # summation$summationA <- c(summation$summationA, sum(c(score1,score2,score3))) 
    summation$summationA[input$submitA] <- sum(c(score1,score2,score3))
  })
  

  # observeEvent(input$submitB,{
  #   score6 = c()
  #   score7 = c()
  #   score8 = c()
  #   
  #   
  #   for (i in c(input$drp6)){
  #     if (any(i == paste("\n",bank[c(32:36),3],"\n", sep = ""))){
  #       score6 = c(score6,10)
  #     }else{
  #       score6 = c(score6,-5)
  #     }
  #   }
  #   for (i in c(input$drp7)){
  #     if (any(i == paste("\n",bank[c(22:26),3],"\n", sep = ""))){
  #       score7 = c(score7, 10)}else{
  #         score7 = c(score7, -5)}
  #   }
  #   for (i in c(input$drp8)){
  #     if (any(i == paste("\n",bank[c(27:31),3],"\n", sep = ""))){
  #       score8 = c(score8, 10)}else{
  #         score8 = c(score8, -5)}
  #   }
  #   
  #   #summation$summationB <- c(summation$summationB, sum(score5)) 
  #   summation$summationB[input$submitB] <- sum(c(score6,score7,score8))
  # })
  # 
  # observeEvent(input$submitC,{
  #   score9 = c()
  #   score10 = c()
  #   score11 = c()
  #   score12 = c()
  #   score13 = c()
  #   score14 = c()
  #   
  #   for (i in c(input$drp9)){
  #     if (any(i == paste("\n",bank[c(32:36),3],"\n", sep = ""))){
  #       score9 = c(score9,10)
  #     }else{
  #       score9 = c(score9,-5)
  #     }
  #   }
  #   for (i in c(input$drp10)){
  #     if (any(i == paste("\n",bank[c(22:26),3],"\n", sep = ""))){
  #       score10 = c(score10, 10)}else{
  #         score10 = c(score10, -5)}
  #   }
  #   for (i in c(input$drp11)){
  #     if (any(i == paste("\n",bank[c(27:31),3],"\n", sep = ""))){
  #       score11 = c(score11, 10)}else{
  #         score11 = c(score11, -5)}
  #   }
  #   for (i in c(input$drp12)){
  #     if (any(i == paste("\n",bank[c(7:11),3],"\n", sep = ""))){
  #       score12 = c(score12, 10)}else{
  #         score12 = c(score12, -5)}
  #   }
  #   for (i in c(input$drp13)){
  #     if (any(i == paste("haha ur wrong",bank[c(7:11),3],"lol", sep = ""))){
  #       score13 = c(score13, 0)}else{
  #         score13 = c(score13, 0)}
  #   }
  #   for (i in c(input$drp14)){
  #     if (any(i == paste("haha ur wrong",bank[c(7:11),3],"lol", sep = "", sep = ""))){
  #       score14 = c(score14, 0)}else{
  #         score14 = c(score14, 0)}
  #   }
  #   
  #   # summation$summationC <- c(summation$summationC, sum(c(score9,score10,score11,score12,score13,score14)) 
  #   summation$summationC[input$submitC] <- sum(c(score9,score10,score11,score12,score13,score14))
  # })
  
  values = reactiveValues(
    count = 0
  )
  observeEvent(input$submitA,{
    if(summation$summationA[input$submitA] == 30){
      updateButton(session, "next1",disabled = FALSE)
      #values$count = values$count + 30
    }
    
    
  })
  observeEvent(input$submitB,{
    if(summation$summationB[input$submitB] == 30){
      updateButton(session, "next2",disabled = FALSE)
      #values$count = values$count + 30
    }
  })
  observeEvent(input$submitC,{
    if(summation$summationC[input$submitC] == 40){
      updateButton(session, "finish",disabled = FALSE)
      #values$count = values$count + 30
    }
    else{
      updateButton(session, "finish", disabled = TRUE)}
    
  })
  
  
  output$scoreA <- renderPrint({
    cat("Score",summation$summationA[input$submitA])
  })
  
  output$scoreB <- renderPrint({
    cat("Score",summation$summationB[input$submitB])
  })
  
  output$scoreC <- renderPrint({
    cat("Score", summation$summationC[input$submitC])
  })
  
}

# Create Shiny App using BOAST App template
 boastApp(ui = ui, server = server)
# shinyApp(ui = ui, server = server) # For testing purposes only