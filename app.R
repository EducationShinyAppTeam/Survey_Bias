# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(dplyr)
library(DT)


# Load additional dependencies and setup functions ----
bank <- read.csv("questionBank.csv", stringsAsFactors = FALSE, header = TRUE)
choicesA <- c("Select Answer", "filtering", "deliberate bias", "anchoring")
choicesB <- c("Select Answer", "unnecessary complexity", "unbiased", "unintentional bias")
choicesC <- c("Select Answer", "filtering", "unnecessary complexity", "unbiased", "unintentional bias")

# Set up UI ----
ui <- list(
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Survey Question Bias",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Survey_Bias")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://shinyapps.science.psu.edu/",
          icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        width = 250,
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Examples", tabName = "examples", icon = icon("book-open-reader")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Survey Question Wording Bias"),
          p("The goal of this app is to illustrate the different types of biases
            that occur in the wording of survey questions."),
          h2("Instructions"),
          p("On the first page, simply click below each question that contains a
            bias to see what that bias is."),
          tags$ul(
            tags$li("On the first page, simply click below each question that
                    contains a bias to see what that bias is."),
            tags$li("Pay attention! Because on the second page, you will be
                    asked to match questions with their appropriate bias.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app was initially designed and programmed by Ryan Manigly-Haney
            with the coding updated by Yuxin Zhang (2017), and Chenese Gray with
            input from Xigang Zhang (2020).",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 8/17/2023 by NJH.")
          )
        ),
        ### Examples Page ----
        tabItem(
          tabName = "examples",
          withMathJax(),
          h2("Types of Survey Wording Bias"),
          br(),
          tabsetPanel(
            type = "tabs",
            #### Bias Examples tab ----
            tabPanel(
              title = "Wording Bias",
              br(),
              h3("Survey Question Wording Bias is a BIG Deal"),
              p("Surveys help us understand public opinion on many topics. While
                surveys may seem easy to create, there are some common pitfalls
                in question wording to watch out for. Expand the boxes below to
                see an example of each."),
              ##### Row 1 ----
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Deliberate Bias",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    p("People who use a form of deliberate bias (also referred to
                      as One-sided Wording) often desire to gather support for a
                      specific cause or opinion. Consider the bias example below
                      then reveal the correct wording."),
                    br(),
                    p("Problematic Example:", br(),
                      "It is hard for today's college graduates to have a bright
                      future with the way things are today in the world.
                      Agree or Disagree?"
                    ),
                    br(),
                    br(),
                    bsButton(
                      inputId = "fixDeliberateBias",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(),
                    br(),
                    p(
                      class = "answertext",
                      tags$strong(textOutput("deliberateExample", inline = TRUE))
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Filtering",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    p("Filtering (or Missing Options) exists when certain choices
                      such as 'undecided' or 'don't know' are not included in the
                      list of possible answers. Consider the bias example below
                      then reveal the correct wording."),
                    br(),
                    p("Problematic Example:", br(),
                      "What is your opinion of our current President?",
                      tags$ol(
                        type = "a",
                        tags$li("Favorable"),
                        tags$li("Unfavorable")
                      )
                    ),
                    br(),
                    br(),
                    bsButton(
                      inputId = "fixFiltering",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(),
                    br(),
                    p(
                      class = "answertext",
                      tags$strong(uiOutput("filteringExample", inline = TRUE))
                    )
                  )
                )
              ),
              ##### Row 2 ----
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Anchoring",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    p("Anchoring is when questions include a reference point or
                      anchor. People tend to say close to the anchor because of
                      either having limited knowledge about the topic or being
                      distracted by the anchor. Consider the bias example below
                      then reveal the correct wording."),
                    br(),
                    p("Problematic Example:", br(),
                      "Knowing that the population of the U.S. is 316 million,
                      what is the population of Canada?"
                    ),
                    br(),
                    br(),
                    bsButton(
                      inputId = "fixAnchoring",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(),
                    br(),
                    p(
                      class = "answertext",
                      tags$strong(textOutput("anchoringExample", inline = TRUE))
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Unintentional Bias",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    p("Unintentional bias or 'The Use of Loaded Words' is when a
                      question contains words such as 'forbid', 'control', 'ban',
                      'outlaw', and 'restraint'. People do not like to be told
                      that they can't do something so they tend to answer 'oppose'
                      or 'no', regardless of what question is actually being asked.
                      Consider the bias example below then reveal the correct
                      wording."),
                    br(),
                    p("Problematic Example:", br(),
                      "Do you favor or oppose an ordinance that ",
                      tags$em("forbids"), " surveillance cameras to be placed on
                      Beaver Ave?",
                    ),
                    br(),
                    br(),
                    bsButton(
                      inputId = "fixUnintentional",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(),
                    br(),
                    p(
                      class = "answertext",
                      tags$strong(textOutput("unintentionalExample", inline = TRUE))
                    )
                  )
                )
              ),
              ##### Row 3 ----
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Unnecessarily Complex: Double-barreled",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    p("One way a question can be unnecessarily complex is if it
                      is composed of two or more separate issues or topics. We
                      call this type of complex question a ",
                      tags$em("Double-barrled Question"), ". Consider the bias
                      example below then reveal the improved wording."),
                    br(),
                    p("Problematic Example:", br(),
                      "Do you think that health care workers and military personnel
                      should be the first to receive the COVID-19 vaccination?"
                    ),
                    br(),
                    br(),
                    bsButton(
                      inputId = "fixDoubleBarrel",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(),
                    br(),
                    p(
                      class = "answertext",
                      tags$strong(uiOutput("doubleBarrelExample", inline = TRUE))
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Unnecessarily Complex: Double Negatives",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    p("Another way in which questions can be unnecessarily complex
                      is through the use of double negatives. Double negative bias
                      occurs when two negative words are used in one sentence.
                      Many respondents will not understand what the question is
                      really asking. Consider the bias example below then reveal
                      the correct wording."),
                    br(),
                    p("Problematic Example:", br(),
                      "Do you disagree that obese children should not be allowed
                      to spend a lot of time watching television, playing computer
                      games, or listening to music?",
                    ),
                    br(),
                    br(),
                    bsButton(
                      inputId = "fixDoubleNeg",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(),
                    br(),
                    p(
                      class = "answertext",
                      tags$strong(textOutput("doubleNegExample", inline = TRUE))
                    )
                  )
                )
              )
            ),
            #### Did you know tab ----
            tabPanel(
              title = "Did you know...",
              br(),
              p("For the 1948 election between Thomas Dewey and Harry Truman,
                Gallup conducted a poll with a sample size of about 3250. Each
                individual in the sample was interviewed in person by a
                professional interviewer to minimize nonresponse bias, and each
                interviewer was given a very detailed set of quotas to meet
                (rather than being given a random sample of specific people to
                contact)."
              ),
              p("For example, an interviewer could have been given the following
                quotas: seven white males under 40 living in a rural area, five
                black males under 40 living in an rurban area, six black females
                under 40 living in a rural area, etc. Other than meeting these
                quotas the ultimate choice of who was interviewed was left to
                each interviewer."
              ),
              p("Based on the results of this poll, Gallup predicted a victory
                for Dewey, the Republican candidate. The predicted breakdown of
                the vote was 50% for Dewey, 44% for Truman, and 6% for third-party
                candidates Strom Thurmond and Henry Wallace. The actual results
                of the election turned out to be almost exactly reversed: 50% for
                Truman, 45% for Dewey, and 5% for third-party candidates."
              ),
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = 'truman.jpg',
                  height = '50%',
                  width = '50%',
                  alt = "Truman holds paper with headline he lost when he won"
                ),
                tags$figcaption("Truman holding paper falsely declaring his
                                defeat; photo by B. Rollins.")
              ),
              br(),
              p("Truman's victory was a great surprise to the nation as a whole.
                So convinced was the Chicago Tribune of Dewey's victory that it
                went to press on its early edition for November 4, 1948 with the
                headline", strong("Dewey defeats Truman.")
              ),
              p("The Gallup Poll learned the lesson that the biases of quota
                based polling can be alleviated by using random sampling
                techniques. Check out ",
                tags$a(href = "http://www.gallup.com", class = "bodylinks",
                       "the Gallup Website"), " to learn more."
              )
            )
          )
        ),
        ### Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Survey Bias Game"),
          tabsetPanel(
            id = "gameLevels",
            type = "hidden",
            #### Directions Tab ----
            tabPanel(
              title = "Directions",
              br(),
              h3("Directions"),
              p("There are three levels to this game. In each level, you'll need
                to review proposed survey questions for wording biases (if any).
                You'll have two attempts for each survey question."),
              p("Click the Start button when you're ready to begin."),
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "go2",
                  label = "Start",
                  style = "default",
                  size = "large",
                  icon = icon("bolt")
                )
              )
            ),
            #### Level A ----
            tabPanel(
              title = "Level A",
              br(),
              h3("Level A"),
              p("Select the appropriate bias for each survey question."),
              h4("Survey Question 1"),
              uiOutput("questionA1"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qA1",
                    label = "Bias Type",
                    choices = choicesA
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansA1")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackA1")
                )
              ),
              h4("Survey Question 2"),
              uiOutput("questionA2"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qA2",
                    label = "Bias Type",
                    choices = choicesA
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansA2")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackA2")
                )
              ),
              h4("Survey Question 3"),
              uiOutput("questionA3"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qA3",
                    label = "Bias Type",
                    choices = choicesA
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansA3")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackA3")
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "prevA",
                    label = "Previous",
                    style = "default",
                    size = "large",
                    icon = icon("backward")
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "submitA",
                      label = "Submit",
                      style = "default",
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  bsButton(
                    inputId = "nextA",
                    label = "Next",
                    style = "default",
                    size = "large",
                    icon = icon("forward"),
                    disabled = TRUE
                  )
                )
              )
            ),
            #### Level B ----
            tabPanel(
              title = "Level B",
              br(),
              h3("Level B"),
              p("Select the appropriate bias for each survey question."),
              h4("Survey Question 1"),
              uiOutput("questionB1"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qB1",
                    label = "Bias Type",
                    choices = choicesB
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansB1")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackB1")
                )
              ),
              h4("Survey Question 2"),
              uiOutput("questionB2"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qB2",
                    label = "Bias Type",
                    choices = choicesB
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansB2")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackB2")
                )
              ),
              h4("Survey Question 3"),
              uiOutput("questionB3"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qB3",
                    label = "Bias Type",
                    choices = choicesB
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansB3")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackB3")
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "prevB",
                    label = "Previous",
                    style = "default",
                    size = "large",
                    icon = icon("backward")
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "submitB",
                      label = "Submit",
                      style = "default",
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  bsButton(
                    inputId = "nextB",
                    label = "Next",
                    style = "default",
                    size = "large",
                    icon = icon("forward"),
                    disabled = TRUE
                  )
                )
              )
            ),
            #### Level C ----
            tabPanel(
              title = "Level C",
              br(),
              h3("Level C"),
              p("Select the appropriate bias for each survey question."),
              h4("Survey Question 1"),
              uiOutput("questionC1"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qC1",
                    label = "Bias Type",
                    choices = choicesC
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansC1")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackC1")
                )
              ),
              h4("Survey Question 2"),
              uiOutput("questionC2"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qC2",
                    label = "Bias Type",
                    choices = choicesC
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansC2")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackC2")
                )
              ),
              h4("Survey Question 3"),
              uiOutput("questionC3"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qC3",
                    label = "Bias Type",
                    choices = choicesC
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansC3")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackC3")
                )
              ),
              h4("Survey Question 4"),
              uiOutput("questionC4"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "qC4",
                    label = "Bias Type",
                    choices = choicesC
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  br(),
                  uiOutput("ansC4")
                ),
                column(
                  width = 6,
                  offset = 0,
                  br(),
                  uiOutput("feedbackC4")
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "prevC",
                    label = "Previous",
                    style = "default",
                    size = "large",
                    icon = icon("backward")
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "submitC",
                      label = "Submit",
                      style = "default",
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  bsButton(
                    inputId = "nextC",
                    label = "Next",
                    style = "default",
                    size = "large",
                    icon = icon("forward"),
                    disabled = TRUE
                  )
                )
              )
            ),
            #### Final Tab ----
            tabPanel(
              title = "Final Scores",
              h3("Final Scores"),
              p("Congratulations on finishing the game!"),
              br(),
              DT::dataTableOutput(outputId = "finalScores"),
              br(),
              fluidRow(
                column(
                  width = 2,
                  offset = 0,
                  bsButton(
                    inputId = "prevScores",
                    label = "Previous",
                    icon = icon("backward"),
                    size = "large"
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  bsButton(
                    inputId = "newGame",
                    label = "Play Again",
                    icon = icon("retweet"),
                    size = "large"
                  )
                )
              )
            )
          )
        ),
        ### References Page----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v0.61.1). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield., N. J. (2023). boastUtils: BOAST utilities.
            (v0.1.11.2). [R Package]. Available from
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create dashboards
            with 'Shiny'. (v0.7.2). [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2022).
            shiny: Web application framework for R. (v1.7.4). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023). shinyWidgets: Custom
            inputs widgets for shiny. (v0.7.6). [R Package]. Availble from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Rollins, B. (1948). Picture Harry S. Truman."
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v1.1.2). [R Package].
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2023). DT: A wrapper of the
            JavaScript library 'DataTables'. (v0.28). [R Package]. Available 
            from https://CRAN.R-project.org/package=DT"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  ## Define player trackers ----
  playerScores <- reactiveValues(
    intLevelA = 0,
    intLevelB = 0,
    intLevelC = 0,
    finalLevelA = 0,
    finalLevelB = 0,
    finalLevelC = 0,
    aAttempts = 0,
    bAttempts = 0,
    cAttempts = 0
  )
  
  playerBank <- reactiveVal(NULL, label = "Player Question Bank")

  ## Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      message <- switch(
        EXPR = input$pages,
        examples = "Click buttons to see improved wordings.",
        game = "Test your understanding by identifying the type of bias in the
        displayed survey question. You have two attempts for each level.",
        "Use the app to explore different ways in which bias can show up in the
        wording of survey questions."
      )
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = message,
        type = "info"
      )
    }
  )

  ## Go button ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "examples"
      )
    }
  )

  ## Examples Page Buttons ----
  ### Deliberate bias ----
  observeEvent(
    eventExpr = input$fixDeliberateBias,
    handlerExpr = {
      output$deliberateExample <- renderText({
        "Do you agree or disagree that it is hard for today's college graduates
        to have a bright future?"
      })
    }
  )

  ### Filtering bias ----
  observeEvent(
    eventExpr = input$fixFiltering,
    handlerExpr = {
      output$filteringExample <- renderUI({
        p("What is your opinion of our current President?",
          tags$ol(
            type = "a",
            tags$li("Favorable"),
            tags$li("Unfavorable"),
            tags$li("Undecided")
          )
        )
      })
    }
  )

  ### Anchoring bias ----
  observeEvent(
    eventExpr = input$fixAnchoring,
    handlerExpr = {
      output$anchoringExample <- renderText({
        "What is the population of Canada?"
      })
    }
  )

  ### Unintentional bias ----
  observeEvent(
    eventExpr = input$fixUnintentional,
    handlerExpr = {
      output$unintentionalExample <- renderText({
        "Do you favor or oppose an ordinance that does not allow surveillance
        cameras to be placed on Beaver Avenue?"
      })
    }
  )

  ### Double barrel ----
  observeEvent(
    eventExpr = input$fixDoubleBarrel,
    handlerExpr = {
      output$doubleBarrelExample <- renderUI({
        p("Who should have priority in receiving the COVID-19 vaccination?",
          tags$ol(
            type = "a",
            tags$li("Health care workers"),
            tags$li("Military personnel"),
            tags$li("Both health care workers and military personnel"),
            tags$li("Neither")
          )
        )
      })
    }
  )

  ### Double negative ----
  observeEvent(
    eventExpr = input$fixDoubleNeg,
    handlerExpr = {
      output$doubleNegExample <- renderText({
        "Do you agree or disagree that children who have a Body Mass Index (BMI)
        at or above the 95th percentile should spend less time watching
        television, playing computer games, and listening to music?"
      })
    }
  )

  ## Starting the Game ----
  observeEvent(
    eventExpr = input$go2,
    handlerExpr = {
      ### Generate Questions ----
      tempBankA <- bank %>%
        filter(Type %in% c("filtering", "deliberate bias", "anchoring")) %>%
        slice_sample(n = 3)
      
      tempBankB <- bank %>%
        filter(Type %in% c("unnecessary complexity", "unbiased", "unintentional")) %>%
        slice_sample(n = 3)
      
      tempBankC <- bank %>%
        filter(Type %in% c("filtering", "unnecessary complexity", "unbiased",
                           "unintentional")) %>%
        filter(!(qID %in% c(tempBankA$QID, tempBankB$qID))) %>%
        slice_sample(n = 4)
      
      playerBank(rbind(tempBankA, tempBankB, tempBankC))
      
      ### Render Question Text ----
      output$questionA1 <- renderText({
        playerBank()[1, "Survey.Question"]
      })
      output$questionA2 <- renderText({
        playerBank()[2, "Survey.Question"]
      })
      output$questionA3 <- renderText({
        playerBank()[3, "Survey.Question"]
      })
      
      output$questionB1 <- renderText({
        playerBank()[4, "Survey.Question"]
      })
      output$questionB2 <- renderText({
        playerBank()[5, "Survey.Question"]
      })
      output$questionB3 <- renderText({
        playerBank()[6, "Survey.Question"]
      })
      
      output$questionC1 <- renderText({
        playerBank()[7, "Survey.Question"]
      })
      output$questionC2 <- renderText({
        playerBank()[8, "Survey.Question"]
      })
      output$questionC3 <- renderText({
        playerBank()[9, "Survey.Question"]
      })
      output$questionC4 <- renderText({
        playerBank()[10, "Survey.Question"]
      })
      
      ### Move to Level A ----
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level A")
    }
  )

  
  ## Scoring Level A ----
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      ### Alert for Non-answers ----
      if (any(input$qA1 == "Select Answer", input$qA2 == "Select Answer",
              input$qA3 == "Select Answer")) {
        sendSweetAlert(
          session = session,
          title = "Answer Questions",
          text = "Try identifying the type of bias for all survey questions.",
          type = "warning"
        )
      } else {
        ### Iterate Attempts ----
        playerScores$aAttempts <- playerScores$aAttempts + 1
        
        ### Question A1 ----
        if (input$qA1 == playerBank()[1, "Type"]) {
          output$ansA1 <- renderIcon("correct")
          output$feedbackA1 <- renderUI({"Congrats!"})
          if (playerScores$aAttempts <= 1) {
            playerScores$intLevelA <- playerScores$intLevelA + 1
          } else {
            playerScores$finalLevelA <- playerScores$finalLevelA + 1
          }
        } else {
          output$ansA1 <- renderIcon("incorrect")
        }
        if (playerScores$aAttempts >= 2) {
          output$feedbackA1 <- renderUI({playerBank()[1, "Feedback"]})
        }
        
        ### Question A2 ----
        if (input$qA2 == playerBank()[2, "Type"]) {
          output$ansA2 <- renderIcon("correct")
          output$feedbackA2 <- renderUI({"Congrats!"})
          if (playerScores$aAttempts <= 1) {
            playerScores$intLevelA <- playerScores$intLevelA + 1
          } else {
            playerScores$finalLevelA <- playerScores$finalLevelA + 1
          }
        } else {
          output$ansA2 <- renderIcon("incorrect")
        }
        if (playerScores$aAttempts >= 2) {
          output$feedbackA2 <- renderUI({playerBank()[2, "Feedback"]})
        }
        
        ### Question A3 ----
        if (input$qA3 == playerBank()[3, "Type"]) {
          output$ansA3 <- renderIcon("correct")
          output$feedbackA3 <- renderUI({"Congrats!"})
          if (playerScores$aAttempts <= 1) {
            playerScores$intLevelA <- playerScores$intLevelA + 1
          } else {
            playerScores$finalLevelA <- playerScores$finalLevelA + 1
          }
        } else {
          output$ansA3 <- renderIcon("incorrect")
        }
        if (playerScores$aAttempts >= 2) {
          output$feedbackA3 <- renderUI({playerBank()[3, "Feedback"]})
        }
        
        if (playerScores$intLevelA == 3) {playerScores$finalLevelA <- 3}
        ### Alter Buttons ----
        if (playerScores$finalLevelA == 3 || playerScores$aAttempts >= 2) {
          updateButton(
            session = session,
            inputId = "nextA",
            disabled = FALSE
          )
          updateButton(
            session = session,
            inputId = "submitA",
            disabled = TRUE
          )
        }
        
      }
    }
  )
  
  ### Move to Level B ----
  observeEvent(
    eventExpr = input$nextA,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level B"
      )
    }
  )
  
  ## Scoring Level B ----
  observeEvent(
    eventExpr = input$submitB,
    handlerExpr = {
      ### Alter for Non-answers ----
      if (any(input$qB1 == "Select Answer", input$qB2 == "Select Answer",
              input$qB3 == "Select Answer")) {
        sendSweetAlert(
          session = session,
          title = "Answer Questions",
          text = "Try identifying the type of bias for all survey questions.",
          type = "warning"
        )
      } else {
        ### Iterate Attempts ----
        playerScores$bAttempts <- playerScores$bAttempts + 1
        
        ### Question B1 ----
        if (input$qB1 == playerBank()[4, "Type"]) {
          output$ansB1 <- renderIcon("correct")
          output$feedbackB1 <- renderUI({"Congrats!"})
          if (playerScores$bAttempts <= 1) {
            playerScores$intLevelB <- playerScores$intLevelB + 1
          } else {
            playerScores$finalLevelB <- playerScores$finalLevelB + 1
          }
        } else {
          output$ansB1 <- renderIcon("incorrect")
        }
        if (playerScores$bAttempts >= 2) {
          output$feedbackB1 <- renderUI({playerBank()[4, "Feedback"]})
        }
        
        ### Question B2 ----
        if (input$qB2 == playerBank()[5, "Type"]) {
          output$ansB2 <- renderIcon("correct")
          output$feedbackB2 <- renderUI({"Congrats!"})
          if (playerScores$bAttempts <= 1) {
            playerScores$intLevelB <- playerScores$intLevelB + 1
          } else {
            playerScores$finalLevelB <- playerScores$finalLevelB + 1
          }
        } else {
          output$ansB2 <- renderIcon("incorrect")
        }
        if (playerScores$bAttempts >= 2) {
          output$feedbackB2 <- renderUI({playerBank()[5, "Feedback"]})
        }
        
        ### Question B3 ----
        if (input$qB3 == playerBank()[6, "Type"]) {
          output$ansB3 <- renderIcon("correct")
          output$feedbackB3 <- renderUI({"Congrats!"})
          if (playerScores$bAttempts <= 1) {
            playerScores$intLevelB <- playerScores$intLevelB + 1
          } else {
            playerScores$finalLevelB <- playerScores$finalLevelB + 1
          }
        } else {
          output$ansB3 <- renderIcon("incorrect")
        }
        if (playerScores$bAttempts >= 2) {
          output$feedbackB3 <- renderUI({playerBank()[6, "Feedback"]})
        }
        
        if (playerScores$intLevelB == 3) {playerScores$finalLevelB <- 3}
        ### Alter Buttons ----
        if (playerScores$finalLevelB == 3 | playerScores$bAttempts >= 2) {
          updateButton(
            session = session,
            inputId = "nextB",
            disabled = FALSE
          )
          updateButton(
            session = session,
            inputId = "submitB",
            disabled = TRUE
          )
        }
      }
    }
  )
  
  ### Move to Level C ----
  observeEvent(
    eventExpr = input$nextB,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level C"
      )
    }
  )
  
  ## Scoring Level C ----
  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      ### Alert for Non-Answers ----
      if (any(input$qC1 == "Select Answer", input$qC2 == "Select Answer",
              input$qC3 == "Select Answer", input$qC4 == "Select Answer")) {
        sendSweetAlert(
          session = session,
          title = "Answer Questions",
          text = "Try identifying the type of bias for all survey questions.",
          type = "warning"
        )
      } else {
        ### Iterate Attempts ----
        playerScores$cAttempts <- playerScores$cAttempts + 1
        
        ### Question C1 ----
        if (input$qC1 == playerBank()[7, "Type"]) {
          output$ansC1 <- renderIcon("correct")
          output$feedbackC1 <- renderUI({"Congrats!"})
          if (playerScores$cAttempts <= 1) {
            playerScores$intLevelC <- playerScores$intLevelC + 1
          } else {
            playerScores$finalLevelC <- playerScores$finalLevelC + 1
          }
        } else {
          output$ansC1 <- renderIcon("incorrect")
        }
        if (playerScores$cAttempts >= 2) {
          output$feedbackC1 <- renderUI({playerBank()[7, "Feedback"]})
        }
        
        ### Question C2 ----
        if (input$qC2 == playerBank()[8, "Type"]) {
          output$ansC2 <- renderIcon("correct")
          output$feedbackC2 <- renderUI({"Congrats!"})
          if (playerScores$cAttempts <= 1) {
            playerScores$intLevelC <- playerScores$intLevelC + 1
          } else {
            playerScores$finalLevelC <- playerScores$finalLevelC + 1
          }
        } else {
          output$ansC2 <- renderIcon("incorrect")
        }
        if (playerScores$cAttempts >= 2) {
          output$feedbackC2 <- renderUI({playerBank()[8, "Feedback"]})
        }
        
        ### Question C3 ----
        if (input$qC3 == playerBank()[9, "Type"]) {
          output$ansC3 <- renderIcon("correct")
          output$feedbackC3 <- renderUI({"Congrats!"})
          if (playerScores$cAttempts <= 1) {
            playerScores$intLevelC <- playerScores$intLevelC + 1
          } else {
            playerScores$finalLevelC <- playerScores$finalLevelC + 1
          }
        } else {
          output$ansC3 <- renderIcon("incorrect")
        }
        if (playerScores$cAttempts >= 2) {
          output$feedbackC3 <- renderUI({playerBank()[9, "Feedback"]})
        }
        
        ### Question C4 ----
        if (input$qC4 == playerBank()[10, "Type"]) {
          output$ansC4 <- renderIcon("correct")
          output$feedbackC4 <- renderUI({"Congrats!"})
          if (playerScores$cAttempts <= 1) {
            playerScores$intLevelC <- playerScores$intLevelC + 1
          } else {
            playerScores$finalLevelC <- playerScores$finalLevelC + 1
          }
        } else {
          output$ansC4 <- renderIcon("incorrect")
        }
        if (playerScores$cAttempts >= 2) {
          output$feedbackC4 <- renderUI({playerBank()[10, "Feedback"]})
        }
        
        if (playerScores$intLevelC == 4) {playerScores$finalLevelC <- 4}
        ### Alter Buttons ----
        if (playerScores$finalLevelC == 4 | playerScores$cAttempts >= 2) {
          updateButton(
            session = session,
            inputId = "nextC",
            disabled = FALSE
          )
          updateButton(
            session = session,
            inputId = "submitC",
            disabled = TRUE
          )
        }
      }
    }
  )
  
  ### Move to Final Tab ----
  observeEvent(
    eventExpr = input$nextC,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Final Scores"
      )
    }
  )
  
  ## Final Score Page ----
  output$finalScores <- DT::renderDataTable(
    expr = {
      data.frame(
        level = c("Level A", "Level B", "Level C"),
        initial = c(playerScores$intLevelA, playerScores$intLevelB,
                    playerScores$intLevelC),
        final = c(playerScores$finalLevelA, playerScores$finalLevelB,
                  playerScores$finalLevelC),
        attempts = c(playerScores$aAttempts, playerScores$bAttempts,
                     playerScores$cAttempts)
      )
    },
    caption = "Your Scores",
    rownames = FALSE,
    colnames = c("Level", "Initial Scores", "Final Scores", "Attempts"),
    options = list(
      responsive = TRUE,
      ordering = FALSE,
      paging = FALSE,
      lengthChange = FALSE,
      searching = FALSE,
      info = FALSE
    )
  )
  
  ## Previous Level Buttons ----
  observeEvent(
    eventExpr = input$prevA, 
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Directions"
      )
    }
  )

  observeEvent(
    eventExpr = input$prevB, 
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level A"
      )
    }
  )

  observeEvent(
    eventExpr = input$prevC, 
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level B"
      )
    }
  )
  
  observeEvent(
    eventExpr = input$prevScores,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level C"
      )
    }
  )
  
  ## Start a New Game ----
  observeEvent(
    eventExpr = input$newGame,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Directions"
      )
      playerScores$intLevelA <- 0
      playerScores$intLevelB <- 0
      playerScores$intLevelC <- 0
      playerScores$finalLevelA <- 0
      playerScores$finalLevelB <- 0
      playerScores$finalLevelC <- 0
      playerScores$aAttempts <- 0
      playerScores$bAttempts <- 0
      playerScores$cAttempts <- 0
      
      playerBank(NULL)
      
      lapply(
        X = c("qA1", "qA2", "qA3", "qB1", "qB2", "qB3", "qC1", "qC2", "qC3", "qC4"),
        FUN = function(x) {
          updateSelectInput(
            session = session,
            inputId = x,
            selected = "Select Answer"
          )
        }
      )
      
      output$ansA1 <- renderIcon()
      output$feedbackA1 <- renderUI({NULL})
      output$ansA2 <- renderIcon()
      output$feedbackA2 <- renderUI({NULL})
      output$ansA3 <- renderIcon()
      output$feedbackA3 <- renderUI({NULL})
      output$ansB1 <- renderIcon()
      output$feedbackB1 <- renderUI({NULL})
      output$ansB2 <- renderIcon()
      output$feedbackB2 <- renderUI({NULL})
      output$ansB3 <- renderIcon()
      output$feedbackB3 <- renderUI({NULL})
      output$ansC1 <- renderIcon()
      output$feedbackC1 <- renderUI({NULL})
      output$ansC2 <- renderIcon()
      output$feedbackC2 <- renderUI({NULL})
      output$ansC3 <- renderIcon()
      output$feedbackC3 <- renderUI({NULL})
      output$ansC4 <- renderIcon()
      output$feedbackC4 <- renderUI({NULL})
    }
  )

}

# Create Shiny App using BOAST App template
boastApp(ui = ui, server = server)
