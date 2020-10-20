# Load Libraries ----
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Measures of Association"
APP_DESCP <<- paste(
  "This app provides a hangman-style game format for helping students review",
  "measures of association."
)
## End App Meta Data------------------------------------------------------------

# Global Constants ----
ansOptions <- list("Select Answer", "Increased Risk", "Odds", "Odds Ratio",
                   "Probability", "Relative Risk", "Risk")

# Read in questions ----
bank <- read.csv(file = "questionbank.csv", stringsAsFactors = FALSE)

# Define the UI ----
ui <- dashboardPage(
  skin = "yellow",
  ## Header ----
  dashboardHeader(
    titleWidth = 250,
    title = "Measures of Association",
    tags$li(
      class = "dropdown",
      actionLink("info", icon("info"))
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        target = "_blank", icon("comments"),
        href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Measures_of_Association"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
    )
  ),
  ## Sidebar ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Prerequisites", tabName = "prerequisite", icon = icon("book")),
      menuItem("Game", tabName = "game", icon = icon("gamepad")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  ## Body ----
  dashboardBody(
    tabItems(
      ## First tab - Overview Tab----
      tabItem(
        tabName = "overview",
        h1("Measures of Association"),
        p("In this app, you will explore measures of association to test
         your ability to distinguish increased risk, probability, risk,
         relative risk, odds, and odds ratio."),
        br(),
        ### Instructions ----
        h2("Instructions"),
        tags$ol(
          tags$li(
            "Review the different quantities on the ",
            actionLink(
              inputId = "linkPreq",
              label = "Prerequisites",
              class = "bodylinks"
            ),
            "page."
          ),
          tags$li(
            "You'll start the game with a man relaxing on top of a tree and
          a scenario displayed."
          ),
          tags$li(
            "For each scenario, you'll need to correctly identify which type of
          measure is each number that gets displayed.
          Press the Submit button to check your work."
          ),
          tags$li(
            "If you misidentify any values, the man will fall down to
          the next branch. If you correctly identify all of the values,
          you'll have completed that scenario."
          ),
          tags$li(
            "Your goal is to complete 10 scenarios before the man falls
          to the ground. The man will fall to the ground after four mistakes."
          )
        ),
        p(
          "Note: if you misidentify all of the values for a scenario, you won't
          be able to move beyond that scenario until you correctly identify at
          least one value. Use the Re-attempt button to make a new attempt at
          identifying the values in the current scenario. Press the RESET button
          if you wish to start the game over."
        ),
        br(),
        div(
          style = "text-align:center;",
          bsButton(
            inputId = "go1",
            label = "GO!",
            icon("bolt"),
            size = "large"
          )
        ),
        ### Acknowledgements ----
        br(),
        br(),
        h2("Acknowledgements"),
        p(
          "This app was originally developed and coded by Zhiliang Zhang.
        The app was further updated by Daehoon Gwak in June 2020.
        Special thanks to Luxin Wang, Dr. Neil Hatfield, and Robert Carey
        for helping with some programming issues.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 10/13/2020 by NJH/RPC.")
        )
      ),
      ## Second tab - Prerequisite Tab ----
      tabItem(
        withMathJax(),
        tabName = "prerequisite",
        h2("Prerequisites"),
        br(),
        box(
          title = "Probability",
          p("Probability is the long-run relative frequency of a particular data
          event occurring, given our assumptions about a repeatable process.
          For example, if we were to imagine running a lottery to pick a US
          citizen, the probability of picking a female is 50.8%. Thus, 50.8% of
          the time we repeat carrying out this lottery (always starting from
          the same initial population), we will pick a female."),
          collapsible = TRUE,
          collapsed = TRUE,
          footer = "Probability is not a measure of association.",
          width = 12
        ),
        box(
          title = "Risk",
          p("Risk refers to the probability of a data event that we view as being
          negative, undesirable, or 'bad'. For example, the lifetime risk of
          developing skin cancer is about 2.6% (1/38) for White Americans.
          Thus, if we imagine a process of picking White Americans and
          observing their lifetimes, 2.6% of the time we will pick an
          individual who will develop skin cancer."),
          collapsible = TRUE,
          collapsed = TRUE,
          footer = "Risk is not a measure of association.",
          width = 12
        ),
        box(
          title = "Relative Risk",
          p("Relative Risk (RR) is the ratio of two groups' risk (probability)
          for a particular data event. We often calculate relative risk
          through the formula \\[RR=\\frac{\\text{Risk for Group 1}}
          {\\text{Risk for Group 2}}\\] For example, the relative risk of skin
          cancer for a White American is \\(2.6\\%/0.1\\% = 0.026/0.001 = 26\\)
          times as large as for Black Americans."),
          collapsible = TRUE,
          collapsed = TRUE,
          footer = "Relative Risk is a measure of association.",
          width = 12
        ),
        box(
          title = "Increased Risk",
          p("Increased Risk (IR) is a way to express relative risk as a percentage
          increase over the lower risk group. As a formula, \\[IR=
          \\left(\\text{Relative Risk}-1\\right)*100\\%\\] For example, since
          the relative risk of skin cancer for a White American is 26 times
          higher compared to Black Americans, then the increased risk is 2500%."),
          collapsible = TRUE,
          collapsed = TRUE,
          footer = "Increased Risk is a measure of association.",
          width = 12
        ),
        box(
          title = "Odds",
          p(
            "Odds is the ratio of two probabilities--the probability of a data
          event and the probability of that same data event not happening (i.e.,
          the complement or the opposite event). There are two ways that odds
          are expressed: 1) as a fraction or 2) using 'odds notation' with a colon.
          Letting \\(p=\\frac{x}{N}\\) represent the probability of a data event
          happening, \\[\\text{Odds}=\\frac{p}{1-p}=\\frac{x/N} {(N-x)/N}
          \\equiv x:(N-x)\\] We read odds notation, \\(X:Y\\), as 'X to Y odds'.",
            br(),
            "For example, the probability (risk) of a White American getting skin
          cancer is 2.6%, approximately \\(\\frac{1}{38}\\). Thus the odds of
          a White American getting skin cancer are 1:37. For Black Americans,
          the odds of getting skin cancer are 1:999."
          ),
          collapsible = TRUE,
          collapsed = TRUE,
          footer = "Odds is not a measure of association.",
          width = 12
        ),
        box(
          title = "Odds Ratio",
          p(
            "An odds ratio is the ratio of the odds for two groups of the same/
          similar data event. Suppose that the first group has a probability
          of the data event of p and the second group has a probability of r.
          Then \\[\\text{Odds Ratio}=\\frac{p}{1-p}\\bigg/\\frac{r}{1-r}\\]",
            br(),
            "For example, we found the odds of a White American (1:37) and a
          Black American (1:999) getting skin cancer. The odds ratio for these
          two groups would be \\[\\frac{1/38}{37/38}\\bigg/
          \\frac{1/1000}{999/1000}\\approx 26.667\\]"
          ),
          collapsible = TRUE,
          collapsed = TRUE,
          footer = "Odds Ratio is a measure of association.",
          width = 12
        ),
        p(tags$em("Note: "), "This app only deals with measures of association
          related to dichotomous probabilities (i.e., relative risks, increased
          risks, and odds ratios). There are other measures of association that
          are not covered by this app. For example, Pearson's Correlation,
          Spearman's Rho, and Kendall's Tau."),
        br(),
        div(
          style = "text-align:center;",
          bsButton(
            inputId = "go2",
            label = "GO!",
            icon("bolt"),
            size = "large"
          )
        )
      ),
      ## Third tab - Game Tab ----
      tabItem(
        tabName = "game",
        h2("Identifying Values"),
        p("Read through each context carefully. For the four given values,
        identify what type of value each one is."),
        h3("The Context"),
        uiOutput("question"),
        br(),
        fluidRow(
          # this column is for the selectInput
          column(
            width = 5,
            wellPanel(
              h3("Identify These Values"),
              br(),
              fluidRow(
                column(
                  width = 9,
                  selectInput(
                    inputId = "first",
                    label = "first value",
                    choices = ansOptions,
                    selectize = TRUE
                  )
                ),
                column(
                  width = 3,
                  uiOutput(
                    outputId = "mark1"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  selectInput(
                    inputId = "second",
                    label = "second value",
                    choices = ansOptions,
                    selectize = TRUE
                  )
                ),
                column(
                  width = 3,
                  uiOutput(
                    outputId = "mark2"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  selectInput(
                    inputId = "third",
                    label = "third value",
                    choices = ansOptions,
                    selectize = TRUE
                  )
                ),
                column(
                  width = 3,
                  uiOutput(
                    outputId = "mark3"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  selectInput(
                    inputId = "fourth",
                    label = "fourth value",
                    choices = ansOptions,
                    selectize = TRUE
                  )
                ),
                column(
                  width = 3,
                  uiOutput(
                    outputId = "mark4"
                  )
                )
              )
            )
          ),
          # this column is for the score tree image
          column(
            width = 7,
            uiOutput("correct", align = "center"),
            div(
              imageOutput(outputId = "scoreTree", inline = TRUE),
              align = "center"
            )
          )
        ),
        # this row is for the buttons
        fluidRow(
          column(
            width = 2,
            offset = 1,
            bsButton(
              inputId = "submit",
              label = "Submit",
              size = "large",
              disabled = FALSE
            )
          ),
          column(
            width = 2,
            offset = 1,
            bsButton(
              "reattempt",
              "Re-attempt",
              size = "large",
              disabled = TRUE
            )
          ),
          column(
            width = 2,
            offset = 1,
            bsButton(
              inputId = "nextq",
              label = "Next >>",
              size = "large",
              disabled = TRUE
            )
          ),
          br()
        )
      ),
      ### References ----
      tabItem(
        tabName = "References",
        h2("References"),
        p( # shinyBS
          class = "hangingindent",
          "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny.
          (v0.61), [R package]. Available from
          https://CRAN.R-project.org/package=shinyBS"
        ),
        p( # Boast Utilities
          class = "hangingindent",
          "Carey, R. (2019), boastUtils: BOAST Utilities. (v0.1.0),
          [R Package]. Available from
          https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p( # shinydashboard
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
          dashboards with 'Shiny'. (v0.7.1), [R Package]. Available from
          https://CRAN.R-project.org/package=shinydashboard"
        ),
        p( # shiny
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
          (2019), shiny: Web application framework for R. (v1.4.0),
          [R Package]. Available from https://CRAN.R-project.org/package=shiny"
        ),
        p( # skin cancer
          class = "hangingindent",
          " Key Statistics for Melanoma Skin Cancer (n.d.), Available from
        https://www.cancer.org/cancer/melanoma-skin-cancer/about/key-statistics.html"
        ),
        p( # shinyWidgets
          class = "hangingindent",
          "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
          (2020), shinyWidgets: Custom Inputs Widgets for Shiny
          (v0.5.2), [R package]. Available from
          https://cran.r-project.org/web/packages/shinyWidgets/index.html"
        ),
        br(),
        br(),
        br(),
        boastUtils::copyrightInfo()
      )
    )
  )
)

# Define the server ----
server <- function(session, input, output) {

  ## variables
  # gameProgress to check whether the user enter the game first time or continue
  gameProgress <- FALSE

  value <- reactiveValues(index = 15, mistake = 0, correct = 0)
  context <- reactiveVal()

  # first column of the question bank
  # use for scenarios. Make this variable class = 'list'.
  # Also, do not pull the value that was took out before
  correct_answer <- as.matrix(bank[1:60, 1])

  index_list <- reactiveValues(list = sample(1:14, 14, replace = FALSE))

  # Remove correct / incorrect icons
  removeMarks <- function() {
    output$mark1 <- renderIcon()
    output$mark2 <- renderIcon()
    output$mark3 <- renderIcon()
    output$mark4 <- renderIcon()
  }

  updateInputs <- function(index) {
    # Update the first box
    updateSelectInput(
      session = session, inputId = "first",
      label = bank[(1 + 4 * (index - 1)), 4],
      choices = ansOptions
    )
    # Update the second box
    updateSelectInput(
      session = session, inputId = "second",
      label = bank[(2 + 4 * (index - 1)), 4],
      choices = ansOptions
    )
    # Update the third box
    updateSelectInput(
      session = session, inputId = "third",
      label = bank[(3 + 4 * (index - 1)), 4],
      choices = ansOptions
    )
    # Update the fourth box
    updateSelectInput(
      session = session, inputId = "fourth",
      label = bank[(4 + 4 * (index - 1)), 4],
      choices = ansOptions
    )
  }

  # Clear inputs, stored values, and pull new questions.
  resetGame <- function() {
    index_list$list <- c(sample(1:14, 14, replace = FALSE))

    value$index <- 15

    value$box1 <- 4 * value$index - 3
    value$box2 <- 4 * value$index - 2
    value$box3 <- 4 * value$index - 1
    value$box4 <- 4 * value$index

    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "reattempt", disable = TRUE)
    updateButton(session, "nextq", disabled = TRUE)

    updateInputs(value$index)

    # Remove marks
    removeMarks()

    value$correct <- 0
    value$mistake <- 0
  }

  # Define info button ----
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = tags$ol(
        tags$li("You need to find an appropriate measure that goes with the
                numerical value."),
        tags$li("You have 4 chances to save the poor little man and you need to
                get 10 scenarios correct."),
        tags$li("Remember, if you miss any question on the scenario, then the
                man will drop down to the next branch.")
      ),
      type = "info"
    )
  })

  # Set Prereqs link ----
  observeEvent(input$linkPreq, {
    updateTabItems(session, "pages", "prerequisite")
  })

  # Define the two go buttons
  observeEvent(input$go1, {
    updateTabItems(session, "pages", "prerequisite")
  })

  observeEvent(input$go2, {
    updateTabItems(session, "pages", "game")
  })

  ## start challenge
  ################ hack(input$game) ####################
  observeEvent(input$go2, {

    # if the user clicks the 'game' first time
    # start game with the last scenario on the question bank
    if (!gameProgress) {
      value$index <- 15
      # correct_answer <- as.matrix(bank[1:60, 1])
      value$box1 <- 4 * value$index - 3 # first question on each scenario
      value$box2 <- 4 * value$index - 2
      value$box3 <- 4 * value$index - 1
      value$box4 <- 4 * value$index

      # track scores for each scenario
      output$correct <- renderUI({
        p("Number of completed scenarios: ", value$correct, " out of 10")
      })

      # the user can continue the game on when they come back
      gameProgress <<- TRUE

      # Initialize the game state using reset function
      resetGame()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pages, {
    if (input$pages == "game") {
      # if the user clicks the 'game' first time
      if (!gameProgress) {
        # start game with the last scenario on the question bank
        value$index <- 15
        # correct_answer <- as.matrix(bank[1:60,1])
        value$box1 <- 4 * value$index - 3 # this is the first question on each scenario
        value$box2 <- 4 * value$index - 2
        value$box3 <- 4 * value$index - 1
        value$box4 <- 4 * value$index

        output$correct <- renderUI({
          p("Number of completed scenarios: ", value$correct, " out of 10")
        })
        gameProgress <<- TRUE

        # Initialize the game state using reset function
        resetGame()
      }
    }
  }, ignoreInit = TRUE)

  ## scenario text
  observe(context(bank[(1 + 4 * (value$index - 1)), 5]))
  output$question <- renderUI({
    context()
  })

  # Submit Button Actions
  observeEvent(input$submit, {

    # Check if the given question was answered correctly
    q1 <- any(input$first == correct_answer[value$box1, 1])
    q2 <- any(input$second == correct_answer[value$box2, 1])
    q3 <- any(input$third == correct_answer[value$box3, 1])
    q4 <- any(input$fourth == correct_answer[value$box4, 1])

    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session, "submit", disabled = TRUE)
    updateButton(session, "reattempt", disabled = FALSE)

    ## score system & check marks ##
    output$mark1 <- renderIcon(
      icon = ifelse(input$first == correct_answer[value$box1, 1],
                    "correct",
                    "incorrect"),
      width = 45
    )
    output$mark2 <- renderIcon(
      icon = ifelse(input$second == correct_answer[value$box2, 1],
                    "correct",
                    "incorrect"),
      width = 45
    )
    output$mark3 <- renderIcon(
      icon = ifelse(input$third == correct_answer[value$box3, 1],
                    "correct",
                    "incorrect"),
      width = 45
    )
    output$mark4 <- renderIcon(
      icon = ifelse(input$fourth == correct_answer[value$box4, 1],
                    "correct",
                    "incorrect"),
      width = 45
    )

    ## Counting Mistakes
    success <- FALSE
    if (!q1 || !q2 || !q3 || !q4) {
      value$mistake <- value$mistake + 1

      # When player makes enough mistakes to fall out of the tree, they must reset the game.
      if (value$mistake == 4) {
        updateButton(session, "reattempt", disabled = TRUE)
        updateButton(session, "submit", disabled = TRUE)
        updateButton(session, "nextq", disabled = TRUE)

        msg <- "Game over! Please try again."

        ### Store xAPI statement ----
        stmt <- boastUtils::generateStatement(
          session,
          verb = "failed",
          object = "shiny-tab-game",
          description = "Identify these values",
          response = msg,
          success = FALSE
        )

        boastUtils::storeStatement(session, stmt)

        confirmSweetAlert(
          session = session,
          inputId = "end",
          title = "Game Over!",
          type = "error",
          text = msg,
          btn_labels = "Retry",
          btn_colors = "orange"
        )
      } else if (!q1 && !q2 && !q3 && !q4) {

        # When the user misses all questions, the next button is disabled
        updateButton(session, "nextq", disabled = TRUE)
      } else {
        # Player can proceed with some values incorrect.
        success <- TRUE
      }
    } else if (q1 && q2 && q3 && q4) {
      success <- TRUE
      value$correct <- value$correct + 1

      # Disable reattempt button if all questions are answered correctly.
      updateButton(session, "reattempt", disabled = TRUE)

      # When the user gets to the end, alert message shows up and reset the game
      if (value$correct == 10) {

        msg <- "You have completed this challenge!"

        confirmSweetAlert(
          session = session,
          inputId = "end",
          title = "Well Done!",
          type = "success",
          text = msg,
          btn_labels = "Game Over",
          btn_colors = "orange"
        )

        ### Store xAPI statement ----
        stmt <- boastUtils::generateStatement(
          session,
          verb = "completed",
          object = "shiny-tab-game",
          description = msg,
          completion = TRUE
        )

        boastUtils::storeStatement(session, stmt)
      }
    }

    ### Store xAPI statement ----
    stmt <- boastUtils::generateStatement(
      session,
      verb = "answered",
      object = "shiny-tab-game",
      description = paste0(
        "Identify these values: ",
        bank[value$box1,4], ", ",
        bank[value$box2,4], ", ",
        bank[value$box3,4], ", ",
        bank[value$box4,4], "."
      ),
      interactionType = "choice",
      success = success,
      response = paste(
        input$first,
        input$second,
        input$third,
        input$fourth,
        sep = ", "
      ),
      extensions = list(
        ref = "https://educationshinyappteam.github.io/BOAST/xapi/result/extensions/context",
        value = jsonlite::toJSON(list(
          context = context(),
          matched = c(q1, q2, q3, q4),
          correct = value$correct,
          mistakes = value$mistake
        ), auto_unbox = TRUE)
      )
    )

    boastUtils::storeStatement(session, stmt)
  })

  # Next Button Actions
  observeEvent(input$nextq, {
    updateButton(session, inputId = "submit", disabled = FALSE)
    updateButton(session, inputId = "nextq", disabled = TRUE)
    updateButton(session, inputId = "reattempt", disabled = TRUE)

    # number of scenario is now the first value of the list
    value$index <- index_list$list[1]

    # remove the first value, so that next question starts with next value$index
    # this keeps going until there are only 2 elements left
    # in the list in the maximum possible case.
    index_list$list <- index_list$list[-1]

    # value$box# should be updated
    value$box1 <- 1 + 4 * (value$index - 1)
    value$box2 <- 2 + 4 * (value$index - 1)
    value$box3 <- 3 + 4 * (value$index - 1)
    value$box4 <- 4 + 4 * (value$index - 1)

    updateInputs(value$index)

    # Remove marks
    removeMarks()
  })

  #### Reattempt button
  observeEvent(input$reattempt, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "reattempt", disable = TRUE)

    updateInputs(value$index)

    # Remove marks
    removeMarks()
  })

  # Game over alert
  observeEvent(input$end, {
    resetGame()
  })

  ##### Draw the Hangman Game #####
  output$scoreTree <- renderImage({

    alt <- c(
      "This is first tree which shows you are fine.",
      "This is the second tree which shows you lose one life.",
      "This is the third tree which shows you only have two lives.",
      "This is the fourth tree which shows you only have one life.",
      "This is the last tree which shows you are dead."
    )
    
    index <- value$mistake + 1
    
    filename <- normalizePath(
      file.path(
        './www/',
        paste0('Cell0', index, '.jpg')
      )
    )
    
    list(
      src = filename,
      alt = alt[index]
    )
  }, deleteFile = FALSE)
}

# Boast Call ---
boastUtils::boastApp(ui = ui, server = server)
