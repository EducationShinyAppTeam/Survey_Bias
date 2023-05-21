# Load Packages
library(dplyr)
library(boastUtils)
library(shinyBS)
library(shinyWidgets)

# Load additional dependencies and setup functions ----
bank <- read.csv("questionBank.csv", stringsAsFactors = FALSE, header = TRUE)
choicesA <- c("Select Answer", "filtering", "deliberate bias", "anchoring")
choicesB <- c("Select Answer", "unnecessary complexity", "unbiased", "unintentional bias")
choicesC <- c("Select Answer", "filtering", "unnecessary complexity", "unbiased", "unintentional bias")

# Define UI for App
ui <- list(
  ## Create the app page
  dashboardPage(
    skin = "red",
    ### Create the app header
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
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        width = 250,
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
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
        #### Set up the Overview Page ----
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Survey Question Wording Bias"),
          p("The goal of this app is to illustrate the different types of biases
            that occur in the wording of survey questions."),
          h2("Instructions"),
          tags$ul(
            tags$li("On the first page, simply click below each question that
                    contains a bias to see what that bias is."),
            tags$li("Pay attention! Because on the second page, you will be
                    asked to match questions with their appropriate bias."),
            tags$li("Note: you will be timed.  Each round will continue to
                    increase in difficulty."),
            tags$li("For the game portion, please note that some of the biases
                    overlap.  So while one may seem fitting, it could be marked
                    incorrect if there is a more dominant bias.")
          ),
          ##### Go Button--location will depend on your goals
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
            div(class = "updated", "Last Update: 8/30/2021 by NJH.")
          )
        ),
        #### Set up the Explore Page ----
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore Types of Survey Wording Bias"),
          tabsetPanel(
            type = "tabs",
            ### Bias Examples tab ----
            tabPanel(
              title = "Wording Bias",
              br(),
              h3("Survey Question Wording Bias is a BIG Deal"),
              p("Surveys help us understand public opinion on many topics. While
                surveys may seem easy to create, there are some common pitfalls
                in question wording to watch out for. Expand the boxes below to
                see an example of each."),
              ### Row 1 ----
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
                    p("Problematic Example:",
                      br(),
                      "It is hard for today's college graduates to have a bright
                      future with the way things are today in the world.
                      Agree or Disagree?"
                    ),
                    br(), br(),
                    bsButton(
                      inputId = "fixDeliberateBias",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(), br(),
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
                    p("Problematic Example:",
                      br(),
                      "What is your opinion of our current President?",
                      tags$ol(
                        type = "a",
                        tags$li("Favorable"),
                        tags$li("Unfavorable")
                      )
                    ),
                    br(), br(),
                    bsButton(
                      inputId = "fixFiltering",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(), br(),
                    p(
                      class = "answertext",
                      tags$strong(uiOutput("filteringExample", inline = TRUE))
                    )
                  )
                )
              ),
              ### Row 2 ----
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
                    p("Problematic Example:",
                      br(),
                      "Knowing that the population of the U.S. is 316 million,
                      what is the population of Canada?"
                    ),
                    br(), br(),
                    bsButton(
                      inputId = "fixAnchoring",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(), br(),
                    p(
                      class = "answertext",
                      tags$strong(textOutput("anchoringExample", inline = TRUE))
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Unintential Bias",
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
                    p("Problematic Example:",
                      br(),
                      "Do you favor or oppose an ordinance that ",
                      tags$em("forbids"),
                      " surveillance cameras to be placed on Beaver Ave?",
                    ),
                    br(), br(),
                    bsButton(
                      inputId = "fixUnintential",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(), br(),
                    p(
                      class = "answertext",
                      tags$strong(textOutput("unintentialExample", inline = TRUE))
                    )
                  )
                )
              ),
              ### Row 3 ----
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
                    p("Problematic Example:",
                      br(),
                      "Do you think that health care workers and military personnel
                      should be the first to receive the COVID-19 vaccination?"
                    ),
                    br(), br(),
                    bsButton(
                      inputId = "fixDoubleBarrel",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(), br(),
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
                    p("Problematic Example:",
                      br(),
                      "Do you disagree that obese children should not be allowed
                      to spend a lot of time watching television, playing computer
                      games, or listening to music?",
                    ),
                    br(), br(),
                    bsButton(
                      inputId = "fixDoubleNeg",
                      label = "Remove the bias!",
                      style = "default",
                      size = "large"
                    ),
                    br(), br(),
                    p(
                      class = "answertext",
                      tags$strong(textOutput("doubleNegExample", inline = TRUE))
                    )
                  )
                )
              )
            ),
            ## Did you know tab ----
            tabPanel(
              title = "Did you know...",
              br(),
              img(
                src = 'truman.png',
                align = "right",
                height = '50%',
                width = '50%',
                alt = "Truman holds paper with headline he lost when he won"
              ),
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
              p("Truman's victory was a great surprise to the nation as a whole.
                So convinced was the Chicago Tribune of Dewey's victory that it
                went to press on its early edition for November 4, 1948 with the
                headline", strong("Dewey defeats Truman.")
              ),
              p("The Gallup Poll learned the lesson that the biases of quota
                based polling can be alleviated by using random sampling
                techniques. Check out ",
                tags$a(href = "http://www.gallup.com", "the Gallup Website"),
                " to learn more."
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              offset = 2,
              bsButton(
                inputId = "goToOverview",
                label = "Return to Overview",
                style = "default",
                size = "large"
              )
            ),
            column(
              width = 3,
              offset = 2,
              bsButton(
                inputId = "playGame",
                label = "Play Wording Bias Game",
                style = "default",
                size = "large"
              )
            )
          )
        ),
        #### Set up an Game Page ----
        tabItem(
          tabName = "Game",
          withMathJax(),
          h2("Survey Bias Game"),
          tabsetPanel(
            id = "gameLevels",
            type = "hidden",
            ## Directions Tab ----
            tabPanel(
              title = "Directions",
              p("This is a three level game to test if you can recognize the
                types of biases described in this app. The first and second level would consist of 3 questions each while
                the third level would contain 4 questions which contain a bias. Match the question with the bias
                that it contains. As the levels get harder, some questions will
                contain multiple biases; in these cases you should select the most
                prevelant bias."),
              p("There is a timer that will start as soon as you begin the game.
                For each question you get wrong, you will be deducted 2 points
                and each question you get right, you will be awarded 2 points.
                In order to move from level to level, you will need to fully
                finish each level. At the end of the second round, the timer
                will stop after all answers are submitted correctly."),
              p("Are you ready? If so, press Start!"),
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "go2",
                  label = "Start!",
                  style = "default",
                  size = "large",
                  icon = icon("bolt")
                )
              )
            ),
            ## Level A ----
            tabPanel(
              title = "Level A",
              div(
                style = "text-align: right;",
                textOutput("timerA")
              ),
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
                    size = "large"
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
                    disabled = TRUE
                  )
                )
              ),
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "reattemptA",
                  label = "Reattempt",
                  style = "default",
                  size = "large",
                  icon = icon("retweet"),
                  disabled = TRUE
                )
              ),
              br(),
              br(),
              tags$strong(textOutput("scoreA")),
              p("The maximum possible score for this level is 6 points.")
            ),
            ## Level B ----
            tabPanel(
              title = "Level B",
              div(
                style = "text-align: right",
                textOutput("timerB")
              ),
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
                    size = "large"
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
                    disabled = TRUE
                  )
                )
              ),
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "reattemptB",
                  label = "Reattempt",
                  style = "default",
                  size = "large",
                  icon = icon("retweet"),
                  disabled = TRUE
                )
              ),
              br(),
              br(),
              tags$strong(textOutput("scoreB")),
              p("The maximum possible score for this level is 6 points.")
            ),
            ## Level C ----
            tabPanel(
              title = "Level C",
              div(
                style = "text-align: right",
                textOutput("timerC")
              ),
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
                    size = "large"
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
                    disabled = TRUE
                  )
                )
              ),
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "reattemptC",
                  label = "Reattempt",
                  style = "default",
                  size = "large",
                  icon = icon("retweet"),
                  disabled = TRUE
                )
              ),
              br(),
              br(),
              tags$strong(textOutput("scoreC")),
              p("The maximum possible score for this level is 8 points.")
            ),
            ## Final Page ----
            tabPanel(
              title = "Final Scores",
              h3("Final Scores"),
              p("Congratulations on finishing the game!"),
              br(),
              textOutput("finalAScore"),
              textOutput("finalBScore"),
              textOutput("finalCScore"),
              hr(),
              textOutput("totalScore"),
              textOutput("finalTime")
            )
          )
        ),
        #### Set up the References Page----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter Bootstrap Components
                      for Shiny. R package version 0.61. Available from
                      https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST
                      Utilities. R package version 0.1.6.2. Available from
                      https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard:
                      Create Dashboards with 'Shiny'. R package version 0.7.1.
                      Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Ooms, J. (2020). V8: Embedded JavaScript and WebAssembly
                       Engine for R. R package version 3.0.2. Available from
                       https://CRAN.R-project.org/package=V8"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., Francois, R., Henry L., and Muller K. (2020). dplyr:
                      A Grammar of Data Manipulation for R. R package version 1.0.2. Available from
                      https://CRAN.R-project.org/package=dplyr"
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

  scoreLevelA <- reactiveVal(0)
  scoreLevelB <- reactiveVal(0)
  scoreLevelC <- reactiveVal(0)

  ## Define what each button does
  observeEvent(input$info, {
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

  # Reset Button For Main Page ----
  observeEvent(input$goToOverview, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "Overview"
    )
  })

  ### go button ----
  observeEvent(input$go1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "Explore"
    )
  })

  ### Play button ----
  observeEvent(input$playGame, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "Game"
    )
  })

  ## Timer Info ----
  time <- reactiveValues(inc = 0, timer = reactiveTimer(1000), started = FALSE)
  observe({
    time$timer()
    if (isolate(time$started)) {
      time$inc <- isolate(time$inc) + 1
    }
  })
  observeEvent(input$go2, {
    time$started <- TRUE
  })
  observeEvent(input$nextA, {
    time$started <- TRUE
  })
  observeEvent(input$nextB, {
    time$started <- TRUE
  })

  ## Timer Outputs ----
  output$timerA <- renderText({
    paste("You have used", time$inc, "seconds.")
  })
  output$timerB <- renderText({
    paste("You have used", time$inc, "seconds.")
  })
  output$timerC <- renderText({
    paste("You have used", time$inc, "seconds.")
  })
  output$finalTime <- renderText({
    paste("You used a total of ", time$inc, "seconds.")
  })

  ###### Back and Forth Buttons ----
  observeEvent(input$go2, {
    updateTabsetPanel(
      session = session,
      inputId = "gameLevels",
      selected = "Level A")
  })

  observeEvent(input$nextA, {
    if(scoreLevelA() == 6) {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level B")
    } else {
      sendSweetAlert(
        session = session,
        title = "Try Again",
        text = "You need to achieve the max score before moving on.",
        type = "warning"
      )
    }
  })

  observeEvent(input$nextB, {
    if(scoreLevelB() == 6) {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Level C")
    } else {
      sendSweetAlert(
        session = session,
        title = "Try Again",
        text = "You need to achieve the max score before moving on.",
        type = "warning"
      )
    }
  })

  observeEvent(input$nextC, {
    if(scoreLevelC() == 8) {
      updateTabsetPanel(
        session = session,
        inputId = "gameLevels",
        selected = "Final Scores"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "Try Again",
        text = "You need to achieve the max score before moving on.",
        type = "warning"
      )
    }
  })

  observeEvent(input$prevA, {
    updateTabsetPanel(
      session = session,
      inputId = "gameLevels",
      selected = "Directions")
  })

  observeEvent(input$prevB, {
    updateTabsetPanel(
      session = session,
      inputId = "gameLevels",
      selected = "Level A")
  })

  observeEvent(input$prevC, {
    updateTabsetPanel(
      session = session,
      inputId = "gameLevels",
      selected = "Level B")
  })

  # Exploration Page ----
  observeEvent(input$fixDeliberateBias, {
    output$deliberateExample <- renderText({
      "Do you agree or disagree that it is hard
      for today's college graduates to have a bright future?"
    })
  })

  observeEvent(input$fixFiltering, {
    output$filteringExample <- renderUI({
      p("What is your opinion of our current President?",
        tags$ol(
          type = "a",
          tags$li("Favorable"),
          tags$li("Unfavorable"),
          tags$li("Undecided")
        ))
    })
  })

  # observeEvent(input$runif2, {
  #   output$myImage <- renderImage({
  #     output$myImage <- renderImage(
  #       {
  #         image_file <- paste("www/", input$image.type, ".png", sep = "")
  #         return(list(
  #           src = image_file,
  #           filetype = "importanceoforder.png",
  #           height = 250,
  #           width = 500
  #         ))
  #       },
  #       deleteFile = FALSE
  #     )
  #   })
  # })

  observeEvent(input$fixAnchoring, {
    output$anchoringExample <- renderText({
      "What is the population of Canada?"
    })
  })

  observeEvent(input$fixUnintential, {
    output$unintentialExample <- renderText({
      "Do you favor or oppose an ordinance that does not allow
      surveillance cameras to be placed on Beaver Avenue?"
    })
  })

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

  observeEvent(input$fixDoubleNeg, {
    output$doubleNegExample <- renderText({
      "Do you agree or disagree that children who
      have a Body Mass Index (BMI) at or above the 95th percentile should spend
less time watching television, playing computer games, and listening to music?"
    })
  })

  ## Game Server Logic ----

  ### Level A Scoring/Submit Button ----
  observeEvent(input$submitA, {
    #### Stop Timer ----
    time$started <- FALSE
    #### QA1 ----
    if (!is.null(input$qA1)) {
      correct <- input$qA1 == questionBank[1, "Type"]

      if (correct) {
        scoreLevelA(scoreLevelA() + 2)
        output$ansA1 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelA(scoreLevelA() - 2)
        output$ansA1 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qA1",
        description = questionBank[1, 3],
        interactionType = "choice",
        response = input$qA1,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### QA2 ----
    if (!is.null(input$qA2)) {
      correct <- input$qA2 == questionBank[2, "Type"]

      if (correct) {
        scoreLevelA(scoreLevelA() + 2)
        output$ansA2 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelA(scoreLevelA() - 2)
        output$ansA2 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qA2",
        description = questionBank[2, 3],
        interactionType = "choice",
        response = input$qA2,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### QA3 ----
    if (!is.null(input$qA3)) {
      correct <- input$qA3 == questionBank[3, "Type"]

      if (correct) {
        scoreLevelA(scoreLevelA() + 2)
        output$ansA3 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelA(scoreLevelA() - 2)
        output$ansA3 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qA3",
        description = questionBank[3, 3],
        interactionType = "choice",
        response = input$qA3,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### Disable Submit Button ----
    updateButton(
      session = session,
      inputId = "submitA",
      disabled = TRUE
    )
    #### Enable Next button ----
    if(!is.null(scoreLevelA()) ){
      updateButton(
        session = session,
        inputId = "nextA",
        disabled = FALSE
      )
    }

    #### Enable Reattempt Button ----
    if(scoreLevelA() < 6){
      updateButton(
        session = session,
        inputId = "reattemptA",
        disabled = FALSE
      )
    }
  })

  ### Level B Scoring/Submit Button ----
  observeEvent(input$submitB, {
    #### Stop Timer ----
    time$started <- FALSE
    #### QB1 ----
    if (!is.null(input$qB1)) {
      correct <- input$qB1 == questionBank[4, "Type"]

      if (correct) {
        scoreLevelB(scoreLevelB() + 2)
        output$ansB1 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelB(scoreLevelB() - 2)
        output$ansB1 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qB1",
        description = questionBank[4, 3],
        interactionType = "choice",
        response = input$qB1,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### QB2 ----
    if (!is.null(input$qB2)) {
      correct <- input$qB2 == questionBank[5, "Type"]

      if (correct) {
        scoreLevelB(scoreLevelB() + 2)
        output$ansB2 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelB(scoreLevelB() - 2)
        output$ansB2 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qB2",
        description = questionBank[5, 3],
        interactionType = "choice",
        response = input$qB2,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### QB3 ----
    if (!is.null(input$qB3)) {
      correct <- input$qB3 == questionBank[6, "Type"]

      if (correct) {
        scoreLevelB(scoreLevelB() + 2)
        output$ansB3 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelB(scoreLevelB() - 2)
        output$ansB3 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qB3",
        description = questionBank[6, 3],
        interactionType = "choice",
        response = input$qB3,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### Disable Submit Button ----
    updateButton(
      session = session,
      inputId = "submitB",
      disabled = TRUE
    )
    #### Enable Next button ----
    if(!is.null(scoreLevelB()) ){
      updateButton(
        session = session,
        inputId = "nextB",
        disabled = FALSE
      )
    }

    #### Enable Reattempt Button ----
    if(scoreLevelB() < 6){
      updateButton(
        session = session,
        inputId = "reattemptB",
        disabled = FALSE
      )
    }
  })

  ### Level C Scoring/Submit Button ----
  observeEvent(input$submitC, {
    #### Stop Timer ----
    time$started <- FALSE
    #### QC1 ----
    if (!is.null(input$qC1)) {
      correct <- input$qC1 == questionBank[7, "Type"]

      if (correct) {
        scoreLevelC(scoreLevelC() + 2)
        output$ansC1 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelC(scoreLevelC() - 2)
        output$ansC1 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qC1",
        description = questionBank[7, 3],
        interactionType = "choice",
        response = input$qC1,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### QC2 ----
    if (!is.null(input$qC2)) {
      correct <- input$qC2 == questionBank[8, "Type"]

      if (correct) {
        scoreLevelC(scoreLevelC() + 2)
        output$ansC2 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelC(scoreLevelC() - 2)
        output$ansC2 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qC2",
        description = questionBank[8, 3],
        interactionType = "choice",
        response = input$qC2,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### QC3 ----
    if (!is.null(input$qC3)) {
      correct <- input$qC3 == questionBank[9, "Type"]

      if (correct) {
        scoreLevelC(scoreLevelC() + 2)
        output$ansC3 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelC(scoreLevelC() - 2)
        output$ansC3 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qC3",
        description = questionBank[9, 3],
        interactionType = "choice",
        response = input$qC3,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### QC4 ----
    if (!is.null(input$qC4)) {
      correct <- input$qC4 == questionBank[10, "Type"]

      if (correct) {
        scoreLevelC(scoreLevelC() + 2)
        output$ansC4 <- boastUtils::renderIcon(icon = "correct", width = 52)
      } else {
        scoreLevelC(scoreLevelC() - 2)
        output$ansC4 <- boastUtils::renderIcon(icon = "incorrect", width = 52)
      }

      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "qC4",
        description = questionBank[10, 3],
        interactionType = "choice",
        response = input$qC4,
        success = correct
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### Disable Submit Button ----
    updateButton(
      session = session,
      inputId = "submitC",
      disabled = TRUE
    )
    #### Enable Next button ----
    if(!is.null(scoreLevelC()) ){
      updateButton(
        session = session,
        inputId = "nextC",
        disabled = FALSE
      )

      stmt <- boastUtils::generateStatement(
        session,
        verb = "completed",
        object = "submitC",
        description = "Survey Bias Game",
        response = paste0(
          "Scores: [",
          scoreLevelA(), ", ",
          scoreLevelB(), ", ",
          scoreLevelC(), ", ",
          "] = ",
          scoreLevelA() + scoreLevelB() + scoreLevelC()
        )
      )

      boastUtils::storeStatement(session, stmt)
    }
    #### Enable Reattempt Button ----
    if(scoreLevelC() < 8){
      updateButton(
        session = session,
        inputId = "reattemptC",
        disabled = FALSE
      )
    }
  })

  ## Reattempt Buttons ----
  observeEvent(input$reattemptA, {
    output$ansA1 <- renderIcon()
    output$ansA2 <- renderIcon()
    output$ansA3 <- renderIcon()
    scoreLevelA(0)
    time$started <- TRUE
    updateButton(
      session = session,
      inputId = "submitA",
      disabled = FALSE
    )
  })

  observeEvent(input$reattemptB, {
    output$ansB1 <- renderIcon()
    output$ansB2 <- renderIcon()
    output$ansB3 <- renderIcon()
    scoreLevelB(0)
    time$started <- TRUE
    updateButton(
      session = session,
      inputId = "submitB",
      disabled = FALSE
    )
  })

  observeEvent(input$reattemptC, {
    output$ansC1 <- renderIcon()
    output$ansC2 <- renderIcon()
    output$ansC3 <- renderIcon()
    output$ansC4 <- renderIcon()
    scoreLevelC(0)
    time$started <- TRUE
    updateButton(
      session = session,
      inputId = "submitC",
      disabled = FALSE
    )
  })

  # Generate Questions for Display ----

  observeEvent(input$go2, {
    tempBankA <- bank %>%
      filter(Type %in% c("filtering", "deliberate bias", "anchoring")) %>%
      slice_sample(n = 3)

    tempBankB <- bank %>%
      filter(Type %in% c("unnecessary complexity", "unbiased", "unintential")) %>%
      slice_sample(n = 3)

    tempBankC <- bank %>%
      filter(Type %in% c("filtering", "unnecessary complexity", "unbiased", "unintential")) %>%
      filter(!(Var %in% c(tempBankA$Var, tempBankB$Var))) %>%
      slice_sample(n = 4)

    questionBank <<- rbind(tempBankA, tempBankB, tempBankC)

    ## Render Question Text ----
    output$questionA1 <- renderText({
      questionBank[1, "Survey.Question"]
    })
    output$questionA2 <- renderText({
      questionBank[2, "Survey.Question"]
    })
    output$questionA3 <- renderText({
      questionBank[3, "Survey.Question"]
    })

    output$questionB1 <- renderText({
      questionBank[4, "Survey.Question"]
    })
    output$questionB2 <- renderText({
      questionBank[5, "Survey.Question"]
    })
    output$questionB3 <- renderText({
      questionBank[6, "Survey.Question"]
    })

    output$questionC1 <- renderText({
      questionBank[7, "Survey.Question"]
    })
    output$questionC2 <- renderText({
      questionBank[8, "Survey.Question"]
    })
    output$questionC3 <- renderText({
      questionBank[9, "Survey.Question"]
    })
    output$questionC4 <- renderText({
      questionBank[10, "Survey.Question"]
    })
  })

  ## Score Outputs ----
  output$scoreA <- renderText({
    paste("Your score is", scoreLevelA(), ".")
  })

  output$finalAScore <- renderText({
    paste("Your score for Level A:", scoreLevelA())
  })

  output$scoreB <- renderText({
    paste("Your score is", scoreLevelB(), ".")
  })

  output$finalBScore <- renderText({
    paste("Your score for Level B:", scoreLevelB())
  })

  output$scoreC <- renderPrint({
    cat("Your score is", scoreLevelC(), ".")
  })

  output$finalCScore <- renderText({
    paste("Your score for Level C:", scoreLevelC())
  })

  output$totalScore <- renderText({
    paste("Your total score is", scoreLevelA() + scoreLevelB() + scoreLevelC(), ".")
  })
}

# Create Shiny App using BOAST App template
boastApp(ui = ui, server = server)
