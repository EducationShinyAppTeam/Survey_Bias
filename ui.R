library(shinydashboard)
library(shinyDND)
library(png)
library(shinyjs)
library(shinyBS)
library(V8)
library(shinyWidgets)

bank<- read.csv("easyQuestions.csv")
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

dashboardPage(
  dashboardHeader(title = "Survey Bias"),
  dashboardSidebar(
    sidebarMenu(id='tabs',
      menuItem("Overview", tabName = "readme", icon = icon("dashboard")),
      menuItem("Explore", tabName = "overview", icon = icon("wpexplorer")),
      menuItem("Game", tabName = "game", icon = icon("gamepad"))
      
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    
    tabItems(
      
      tabItem(tabName = "readme",
              theme = "custom.css",
                       tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                       br(),br(),br(),
                       h3(strong("About:")),
                       h4("The goal of this app is to illustrate the different types of biases that occur in the wording of survey questions."),
                       br(),h3(strong("Instructions:")),
                       h4(tags$li("On the first page, simply click below each question that contains a bias to see what that bias is.")),
                       h4(tags$li("Pay attention! Because on the second page, you will be asked to match questions with their appropriate bias.")),  
                       h4(tags$li("Note: you will be timed.  Each round will continue to increase in difficulty.")),
                       h4(tags$li("For the last round, please note that some of the biases overlap.  So while one may seem fitting, it could be marked incorrect if there is a more dominant bias.")),
                       
                       div(style = "text-align: center",
                           bsButton("start","Explore",icon("bolt"),style = "danger",size = "medium",class="circle grow")),
                       h3(strong("Acknowledgements:")),
                       h4("This app includes the following packages: shinydashboard, shinyDND, shinyjs, shinyBS, and V8.")
                     
              
              ),
      
      
      tabItem(tabName = "overview",
              div(style="display: inline-block;vertical-align:top;",
                  tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
              ),
              div(style="display: inline-block;vertical-align:top;",
                  circleButton("info",icon = icon("info"),status = "myClass", size = "xs")
              ),
              
              fluidRow(
                
                theme = "custom.css",
                       box(title = "What's the big deal about Survey Bias?", status = "success", solidHeader = TRUE, width = 7,
                           
                           "Surveys help us understand public opinion on many topics.  While surveys may seem easy to create, there are some common pitfalls in question wording to watch out for.  Look at the questions below to see an example of each.",
                           
                           
                           br(),
                           br(),
                           br(),
                           strong("Deliberate Bias (one-sided wording): "), " It is hard for today's college graduates to have a bright future 
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
                         strong("Filtering (missing options): "), "What is your opinion of our current President?",
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
                         
                         strong("Anchoring : "), "Knowing that the population of the U.S. is 316 million, what is the population of Canada?",
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
                         strong("Unintentional Bias (use of loaded words): "), "Do you favor or oppose an ordinance that ", em("forbids "), "surveillance cameras to be placed on Beaver Ave?",
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
                         strong("Unnecessary Complexity (double-barreled questions): "), "Do you think that health care workers and military
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
                         strong("Unnecessary Complexity (Double Negatives): "), "Do you disagree that
                         obese children should not be allowed to spend a lot of time watching television, playing computer games, or listening to music?",
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
                       
                       box(title = "Did you Know...", status = "warning", solidHeader = TRUE, width = 5, 
                           img(src = 'truman.png', align= "right"),
                           " For the 1948 election between Thomas Dewey and Harry Truman, Gallup conducted a poll with a sample size of about 
                           3250. Each individual in the sample was interviewed in person by a professional interviewer to minimize nonresponse bias, 
                           and each interviewer was given a very detailed set of quotas to meet (rather than being given a random sample of specific people to contact).",
                           br(),
                           br(),
                           "For example, an interviewer could have been given 
                           the following quotas: seven white males under 40 living in a rural area, five black males under 40 living in an rurban area, 
                           six black females under 40 living in a rural area, etc. Other than meeting these quotas the ultimate choice of who was 
                           interviewed was left to each interviewer.",
                           
                           br(),
                           br(),
                           
                           "Based on the results of this poll, Gallup predicted a victory for Dewey, the Republican candidate. 
                           The predicted breakdown of the vote was 50% for Dewey, 44% for Truman, and 6% for third-party candidates
                           Strom Thurmond and Henry Wallace. The actual results of the election turned out to be almost exactly reversed:
                           50% for Truman, 45% for Dewey, and 5% for third-party candidates.",
                           
                           br(),
                           br(),
                           
                           "Truman's victory was a great surprise to the nation as a whole. So convinced was the Chicago Tribune of Dewey's 
                           victory that it went to press on its early edition for November 4, 1948 with the headline",
                           strong("Dewey defeats Truman"),
                           
                           br(),
                           br(),
                           "The Gallup Poll learned the lesson that the biases of quota based polling can be alleviated by using random sampling techniques. 
                           Check out", a("their site", href="http://www.gallup.com"), " to see more!"
                           
                       )
                         )
                         ),
      
      
      tabItem(tabName = "game",
              div(style="display: inline-block;vertical-align:top;",
                  tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
              ),
              div(style="display: inline-block;vertical-align:top;",
                  circleButton("info1",icon = icon("info"),status = "myClass", size = "xs")
              ),
              fluidRow(theme = "bootstrap.css",
                       
                       navbarPage(title = "Game Types", id = "navMain",
                                  
                                  tabPanel(title = "Directions", value = "a",
                                           "This is a three level game to test if you can recognize the types of biases described in this app. 
                                           Each level will consist of 4 questions that contain a bias.  Match the question with the bias 
                                           that it contains.  As the levels get harder, some questions will contain 
                                           multiple biases.  Only choose one!",
                                           
                                           
                                           br(), br(),
                                           "There is a timer that will start as soon as you begin the game.  For each question you get wrong,  
                                           you will be deducted 2 points and each question you get right, you will be awarded 2 points.  In order to move from easy to medium to hard, you will need to fully
                                           finish each level.  At the end of the second round, the timer will stop after all answers are 
                                           submitted correctly.  Your score will be compiled and if it is a top score, it will make the leader board.",
                                           
                                           br(),
                                           br(),
                                           h2("Ready?"),
                                           
                                           fluidRow(column(1,offset = 5,
                                                           bsButton("go","G O !" ,style = "danger",size = "large"))
                                           )
                                  ),
                                  
                                  ########Level Easy
                                  tabPanel("Level A",value = "b",
                                           fluidPage(theme = "bootstrap.css", #css theme
                                                     tags$style(type='text/css', '#timer1 {background-color:#2C3E50; font-size: 20px; 
                                                                color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'), #link to your own css file
                                                     titlePanel("Drag the questions into the categories they belong to. "),
                                                     fluidRow(column(3,offset = 9,textOutput("timer1"))),br(), 
                                                     
                                                     conditionalPanel("input.go != 0", #Show everything only after the GO button is clicked
                                                                      #Set up all dragUIs which are randomly chosen from the question bank 
                                                                      
                                                                      fluidRow(
                                                                        dragUI(textOutput("filteringID1"),textOutput("filteringName1"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-3 drag dragelement"),
                                                                        
                                                                        dragUI(textOutput("deliberateID1"),textOutput("deliberateName1"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-3 drag dragelement"),
                                                                        
                                                                        dragUI(textOutput("anchoringID1"),textOutput("anchoringName1"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-3 drag dragelement"),
                                                                        hr()),
                                                                      
                                                                      #Set Up Drop UI
                                                                      fluidRow(
                                                                        wellPanel(dropUI("drp2", class = "dropelement"), h4("Filtering"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer2")), class = "wellTransparent col-xs-12 col-sm-12 col-md-6 col-lg-3"),
                                                                        
                                                                        
                                                                        
                                                                        wellPanel(dropUI("drp3", class = "dropelement"), h4("Anchoring"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer3")), class = "wellTransparent col-xs-12 col-sm-12 col-md-6 col-lg-3"),
                                                                        
                                                                        
                                                                        
                                                                        wellPanel(dropUI("drp4", class = "dropelement"),h4("Deliberate Bias"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer4")), class = "wellTransparent col-xs-12 col-sm-12 col-md-6 col-lg-3")),
                                                                      
                                                                      #Submit button and pagination button
                                                                      fluidRow(
                                                                        column(1,bsButton("prev2","<<Previous", style = "danger",size = "small")),
                                                                        column(1,offset = 4, conditionalPanel("(input.drp2!='') & (input.drp3!='') & (input.drp4!='')"
                                                                                                              ,bsButton("submitA", "Submit Answer", style = "danger",size = "small",class = "grow"))),
                                                                        column(1,offset = 5,bsButton("next1","Next>>",style = "danger", size = "small", disabled = TRUE))
                                                                      ),br(),
                                                                      
                                                                      
                                                                      conditionalPanel("input.submitA != 0",wellPanel(
                                                                        fluidPage(
                                                                          fluidRow(
                                                                            wellPanel(
                                                                              div(style = "position:absolute; top;50em; left:1em",h4("Please drag the wrong answers into this PENALTY box and click the CLEAR button to restart.")),
                                                                              dropUI("home1",class = "dropelement dropelementHome"),
                                                                              div(style = "position:absolute; top:8em; right:2em",bsButton("clear","CLEAR",style = "danger")),class = "wellTransparent col-lg-8"),
                                                                            wellPanel(h4("Full score is 30 for Easy Level."),
                                                                                      verbatimTextOutput("scoreA"),class = "wellTransparent col-lg-4")
                                                                          )))))                
                                                     
                                                     
                                                     
                                                     
                                                     
                                                     )),
                                  tabPanel("Level B",value = "c",
                                           fluidPage(theme = "bootstrap.css", #css theme
                                                     tags$style(type='text/css', '#timer2 {background-color:#2C3E50; font-size: 20px; 
                                                                color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'), #link to your own css file
                                                     titlePanel("Drag the questions into the categories they belong to. "),
                                                     fluidRow(column(3,offset = 9,textOutput("timer2"))),br(), 
                                                     
                                                     conditionalPanel("input.next1 != 0", #Show everything only after the next button is clicked
                                                                      #Set up all dragUIs which are randomly chosen from the question bank 
                                                                      fluidRow(
                                                                        dragUI(textOutput("unnecessaryID1"),textOutput("unnecessaryName1"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-3 drag dragelement"),
                                                                        
                                                                        
                                                                        dragUI(textOutput("nonbiasID1"),textOutput("nonbiasName1"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-3 drag dragelement"),
                                                                        
                                                                        
                                                                        dragUI(textOutput("unintentionalID2"),textOutput("unintentionalName2"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-3 drag dragelement")),
                                                                      
                                                                      
                                                                      hr(),
                                                                      
                                                                      #Set Up Drop UI
                                                                      fluidRow(
                                                                        
                                                                        wellPanel(dropUI("drp6", class = "dropelement"), h4("Unbias"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer6")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3"),
                                                                        
                                                                        
                                                                        
                                                                        wellPanel(dropUI("drp7", class = "dropelement"), h4("Unintentional Bias"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer7")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3"),
                                                                        
                                                                        
                                                                        
                                                                        wellPanel(dropUI("drp8", class = "dropelement"), h4("Unnecessary Complexity"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer8")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3")),
                                                                      
                                                                      #Submit button and pagination button
                                                                      fluidRow(
                                                                        column(1,bsButton("prev1","<<Previous", style = "primary",size = "small")),
                                                                        column(1,offset = 4, conditionalPanel("(input.drp6!='') & (input.drp7!='') & (input.drp8!='')"
                                                                                                              ,bsButton("submitB", "Submit Answer", style = "primary",size = "small",class = "grow"))),
                                                                        column(1,offset = 5,bsButton("next2","Next>>",style = "primary", size = "small", disabled = TRUE))
                                                                      ),br(),
                                                                      
                                                                      
                                                                      conditionalPanel("input.submitB != 0",wellPanel(
                                                                        fluidPage(
                                                                          fluidRow(
                                                                            wellPanel(
                                                                              div(style = "position:absolute; top;50em; left:1em",h4("Please drag the wrong answers into this PENALTY box and click the CLEAR button to restart.")),
                                                                              dropUI("home1",class = "dropelement dropelementHome"),
                                                                              div(style = "position:absolute; top:8em; right:2em",bsButton("clearB","CLEAR",style = "danger")),class = "wellTransparent col-lg-8"),
                                                                            wellPanel(h4("Full score is 30 for Medium Level."),
                                                                                      verbatimTextOutput("scoreB"),class = "wellTransparent col-lg-4")
                                                                          )))))                
                                                     
                                                     
                                                     
                                                     
                                                     
                                                     )),
                                  tabPanel("Level C",value = "d",
                                           fluidPage(theme = "bootstrap.css", #css theme
                                                     tags$style(type='text/css', '#timer3 {background-color:#2C3E50; font-size: 20px; 
                                                                color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'), #link to your own css file
                                                     titlePanel("Drag the questions into the categories they belong to. "),
                                                     fluidRow(column(3,offset = 9,textOutput("timer3"))),br(), 
                                                     
                                                     conditionalPanel("input.next2 != 0", #Show everything only after the GO button is clicked
                                                                      #Set up all dragUIs which are randomly chosen from the question bank 
                                                                      fluidRow(
                                                                        dragUI(textOutput("unintentionalID1"),textOutput("unintentionalName1"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-12 drag dragelement"),
                                                                        
                                                                        
                                                                        dragUI(textOutput("unnecessaryID2"),textOutput("unnecessaryName2"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-12 drag dragelement"),
                                                                        
                                                                        
                                                                        dragUI(textOutput("filteringID2"),textOutput("filteringName2"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-12 drag dragelement"),
                                                                        
                                                                        
                                                                        dragUI(textOutput("nonbiasID2"),textOutput("nonbiasNAME2"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-12 drag dragelement")
                                                                      ),hr(),
                                                                      
                                                                      #Set Up Drop UI
                                                                      fluidRow(
                                                                        
                                                                        wellPanel(dropUI("drp9", class = "dropelement"),h4("Nonbias"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer9")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3"),
                                                                        
                                                                        
                                                                        
                                                                       # wellPanel(dropUI("drp13", class = "dropelement"),h4("Anchoring"),
                                                                                 # div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer13")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3"),
                                                                        
                                                                        
                                                                        wellPanel(dropUI("drp11", class = "dropelement"),h4("Unnecessary Complexity"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer11")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3")
                                                                        
                                                                        
                                                                      ),
                                                                      
                                                                      fluidRow(
                                                                        wellPanel(dropUI("drp10", class = "dropelement"), h4("Unintentional Bias"),
                                                                                  div(style = "position:absolute;top: 10%;right: 2%;", htmlOutput("answer10")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3"),
                                                                       # wellPanel(dropUI("drp14", class = "dropelement"), h4("Deliberate"),
                                                                                #  div(style = "position:absolute;top: 10%;right: 2%;", htmlOutput("answer14")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3"),
                                                                        wellPanel(dropUI("drp12", class = "dropelement"),h4("Filtering"),
                                                                                  div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer12")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3")
                                                                        
                                                                      ),
                                                                      
                                                                      #Submit button and pagination button
                                                                      fluidRow(
                                                                        column(1,bsButton("prev3","<<Previous", style = "primary",size = "small")),
                                                                        column(1,offset = 4, conditionalPanel("(input.drp9!='') & (input.drp10!='') & (input.drp11!='') & (input.drp12!='')"
                                                                                                              ,bsButton("submitC", "Submit Answer", style = "primary",size = "small",class = "grow")))),
                                                                      br(),
                                                                      
                                                                      
                                                                      conditionalPanel("input.submitB != 0",wellPanel(
                                                                        fluidPage(
                                                                          fluidRow(
                                                                            wellPanel(
                                                                              div(style = "position:absolute; top;10em; left:1em",h4("Please drag the wrong answers into this PENALTY box and click the CLEAR button to restart."),
                                                                                  dropUI("home1",class = "dropelement dropelementHome")),
                                                                              div(style = "position:absolute; top:8em; right:2em",bsButton("clearC","CLEAR",style = "danger")),class = "wellTransparent col-lg-8"),
                                                                            column(1,offset = 5,bsButton("finish","STOP>>", style = "danger", disabled = TRUE, size = "small")),
                                                                            wellPanel(h4("Full score is 40 for Hard Level."),
                                                                                      verbatimTextOutput("scoreC"),class = "wellTransparent col-lg-4")
                                                                          ))))                
                                                                      
                                                                      
                                                                      
                                                                      
                                                                      
                                                     )))
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  )))
      
      
      
      
      
      )
    )
  )


