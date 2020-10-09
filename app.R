# Load Libraries ----
library(shinydashboard)
library(shiny)
library(shinyBS)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Measures of Association"
APP_DESCP  <<- paste(
  "This app provides a hangman-style game format for helping students review",
  "measures of association."
)
## End App Meta Data------------------------------------------------------------

# Read in questions ----
bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

# Define the UI ----
ui <- list(
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
        tags$a(target = "_blank", icon("comments"),
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
      tags$div(class = "sidebar-logo",
               boastUtils::psu_eberly_logo("reversed"))
    ),
    ## Body ----
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
      ),
      tabItems(
        ##First tab - Overview Tab----
        tabItem(
          tabName = "overview",
          h1("Measures of Association"),
          p("In this app, you will explore measures of association to test
           your ability to distinguish increased risk, probability, risk,
           relative risk, odds, and odds ratio."),
          br(),
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
            style = "text-align:center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              icon("bolt"),
              size = "large"
            )
          ),
          # Acknowledgements
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was originally developed and coded by Zhiliang Zhang.
          The app was further updated by Daehoon Gwak in June 2020.
          Special thanks to Luxin Wang and professor Neil Hatfield
          for helping with some programming issues.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/18/2020 by DG.")
          )
        ),
        ##Second tab - Prerequisite Tab ----
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
            footer= "Probability is not a measure of association.",
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
            footer= "Risk is not a measure of association.",
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
            footer= "Relative Risk is a measure of association.",
            width = 12
          ),
          box(
            title = "Increased Risk",
            p("Increased Risk (IR) is a relative risk expressed as a percentage
            increase over the lower risk group. As a formula \\[IR=
            \\left(\\text{Relative Risk}-1\\right)*100\\%\\] For example, since
            the relative risk of skin cancer for a White American is 26 times
            higher compared to Black Americans, then the increased risk is 2500%."),
            collapsible = TRUE,
            collapsed = TRUE,
            footer= "Increased Risk is a measure of association.",
            width = 12
          ),
          box(
            title = "Odds",
            p("Odds is the ratio of two probabilities--the probability of a data
            event and the probability of that data event not happening (i.e.,
            the complement or the opposite event). There are two ways that odds
            are expressed: as a fraction or using 'odds notation' with a colon.
            Letting \\(p=\\frac{x}{N}\\) represents the probability of a data event
            happening, \\[\\text{Odds}=\\frac{p}{1-p}=\\frac{x/N} {(N-x)/N}
            \\equiv X:(N-X)\\] We read odds notation, \\(X:Y\\) as 'X to Y'.",
              br(),
              "For example, the probability (risk) of a White American getting skin
            cancer is 2.6%, approximately \\(\\frac{1}{38}\\). Thus the odds of
            a White American getting skin cancer are 1:37. For Black Americans,
            the odds of getting skin cancer are 1:999."),
            collapsible = TRUE,
            collapsed = TRUE,
            footer= "Odds is not a measure of association.",
            width = 12
          ),
          box(
            title = "Odds Ratio",
            p("An odds ratio is the ratio of the odds for two groups of the 
            same/similar data event. Suppose that the first group has a probability
            of the data event of p and the second group has a probability of r.
            Then \\[\\text{Odds Ratio}=\\frac{p}{1-p}\\bigg/\\frac{r}{1-r}\\]",
              br(),
              "For example, we found the odds of a White American (1:37) and a
            Black American (1:999) getting skin cancer. The odds ratio for these
            two groups would be \\[\\frac{1/38}{37/38}\\bigg/
            \\frac{1/1000}{999/1000}\\approx 26.667\\]"),
            collapsible = TRUE,
            collapsed = TRUE,
            footer= "Odds Ratio is a measure of association.",
            width = 12
          ), 
          p(tags$em("Note:"), " This app covers measures of association 
            regarding dichotomous outcomes. There are other measures of 
            association that are not covered by this app. For example, 
            Pearson's Correlation, Spearman's Rho, and Kendall's Tau."), 
          br(),
          div(
            style = "text-align:center",
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
                h3('Identify These Values'),
                br(),
                fluidRow(
                  column(
                    width = 9,
                    selectInput(
                      inputId = 'first',
                      label = 'first value',
                      choices = list(
                        'Select Answer',
                        'Increased Risk',
                        'Odds',
                        'Odds Ratio',
                        'Probability',
                        'Relative Risk',
                        'Risk'
                      ),
                      selectize = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    uiOutput(
                      outputId = 'mark1'
                      #inline = TRUE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 9,
                    selectInput(
                      inputId = 'second',
                      label = "second value",
                      choices = list(
                        'Select Answer',
                        'Increased Risk',
                        'Odds',
                        'Odds Ratio',
                        'Probability',
                        'Relative Risk',
                        'Risk'
                      ),
                      selectize = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    uiOutput(
                      outputId = "mark2",
                      #inline = TRUE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 9,
                    selectInput(
                      inputId = 'third',
                      label = 'third value',
                      choices = list(
                        'Select Answer',
                        'Increased Risk',
                        'Odds',
                        'Odds Ratio',
                        'Probability',
                        'Relative Risk',
                        'Risk'
                      ),
                      selectize = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    uiOutput(
                      outputId = 'mark3',
                      #inline = TRUE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 9,
                    selectInput(
                      inputId = 'fourth',
                      label = 'fourth value',
                      choices = list(
                        'Select Answer',
                        'Increased Risk',
                        'Odds',
                        'Odds Ratio',
                        'Probability',
                        'Relative Risk',
                        'Risk'
                      ),
                      selectize = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    uiOutput(
                      outputId = 'mark4'
                      #inline = TRUE
                    )
                  )
                )
              )
            ),
            # this column is for the score tree image
            column(width = 7,
                   uiOutput("correct", align = 'center'),
                   div(
                     imageOutput(outputId = "scoreTree", inline = TRUE),
                     align = 'center')
            )
          ),
          # this row is for the buttons
          fluidRow(
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = 'submit',
                label = "Submit",
                size = "large",
                disabled = FALSE
              )
            ),
            column(
              width = 2,
              offset = 1,
              bsButton(
                'reattempt',
                'Re-attempt',
                size = 'large',
                disabled = TRUE
              )
            ),
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = 'nextq',
                label = "Next >>",
                size = "large",
                disabled = TRUE
              )
            ),
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = 'reset',
                label = "RESET",
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
          p(     #shinyBS
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny.
            (v0.61), [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(     #Boast Utilities
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities. (v0.1.0),
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(     #shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1), [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(     #shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R. (v1.4.0),
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(     #skin cancer
            class = "hangingindent",
            " Key Statistics for Melanoma Skin Cancer (n.d.), Available from
          https://www.cancer.org/cancer/melanoma-skin-cancer/about/key-statistics.html"
          ),
          p(     #shinyWidgets
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
)

#Define the server ----
server <- function(session, input, output) {
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

  # Define the two go2 buttons
  observeEvent(input$go1, {
    updateTabItems(session, "pages", "prerequisite")
  })

  observeEvent(input$go2, {
    updateTabItems(session, "pages", "game")
  })

  ## variables
  # gameProgress to check whether the user enter the game first time or continue
  gameProgress <- FALSE

  value <- reactiveValues(index =  15, mistake = 0, correct = 0)
  correct_answer <- as.matrix(bank[1:60,1]) # first column of the question bank
  # use for scenarios. Make this variable class = 'list'.
  # Also, do not pull the value that was took out before
  index_list <- reactiveValues(list=sample(1:14,14,replace=FALSE))

  ## start challenge
  ################ hack(input$game) ####################
  observeEvent(input$go2,{
    if (!gameProgress) { # if the user clicks the 'game' first time
      value$index <- 15 # start game with the last scenario on the question bank
      # correct_answer <- as.matrix(bank[1:60, 1])
      value$box1 = 4 * value$index - 3 # first question on each scenario
      value$box2 = 4 * value$index - 2
      value$box3 = 4 * value$index - 1
      value$box4 = 4 * value$index

      output$correct <- renderUI({ # track scores for each scenario
        p("Number of completed scenarios: " , value$correct, " out of 10")
      })
      # the user can continue the game on when they come back
      gameProgress <<- TRUE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pages, {
    if (input$pages == "game") {
      if (!gameProgress) { #if the user clicks the 'game' first time
        value$index <- 15 # start game with the last scenario on the question bank
        # correct_answer <- as.matrix(bank[1:60,1])
        value$box1= 4*value$index-3 # this is the first question on each scenario
        value$box2= 4*value$index-2
        value$box3= 4*value$index-1
        value$box4= 4*value$index

        output$correct <- renderUI({
          p("Number of completed scenarios: " , value$correct, " out of 10")
        })
        gameProgress <<- TRUE
      }
    }
  }, ignoreInit = TRUE)


  ## scenario text
  output$question <- renderUI({
    bank[(1 + 4 * (value$index - 1)), 5]
  })

  # Put the values to be identified as labels
  observe({
    # Update the first box
    updateSelectInput(
      session = session,
      inputId = "first",
      label = bank[(1 + 4 * (value$index - 1)), 4], # which scenario do you want?
      choices = list('Select Answer', 'Increased Risk', 'Odds', 'Odds Ratio',
                     'Probability', 'Relative Risk', 'Risk'
      )
    )
    # Update the second box
    updateSelectInput(
      session = session,
      inputId = "second",
      label = bank[(2 + 4 * (value$index - 1)), 4],
      choices = list('Select Answer', 'Increased Risk', 'Odds', 'Odds Ratio',
                     'Probability', 'Relative Risk', 'Risk'
      )
    )
    # Update the third box
    updateSelectInput(
      session = session,
      inputId = "third",
      label = bank[(3 + 4 * (value$index - 1)), 4],
      choices = list('Select Answer', 'Increased Risk', 'Odds', 'Odds Ratio',
                     'Probability', 'Relative Risk', 'Risk'
      )
    )
    # Update the fourth box
    updateSelectInput(
      session = session,
      inputId = "fourth",
      label = bank[(4 + 4 * (value$index - 1)), 4],
      choices = list('Select Answer', 'Increased Risk', 'Odds', 'Odds Ratio',
                     'Probability', 'Relative Risk', 'Risk'
      )
    )
  })

  # Submit Button Actions
  observeEvent(input$submit, {
    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session, "submit", disabled = TRUE)
    updateButton(session, "reattempt", disabled = FALSE)

    ## score system & check marks ##
    output$mark1 <- renderUI({
      # check out if the selected value is correct in the correct_answer matrix
      if (any(input$first == correct_answer[value$box1,1])){
        img(
          src = 'check.png',
          alt = 'Correct',
          width = 60
        )
      }
      else{
        img(
          src = 'cross.png',
          alt = 'Incorrect',
          width = 60
        )
      }
    })
    output$mark2 <- renderUI({
      if (any(input$second == correct_answer[value$box2,1])){
        img(
          src = 'check.png',
          alt = 'Correct',
          width = 60
        )
      }
      else{
        img(
          src = 'cross.png',
          alt = 'InCorrect',
          width = 60
        )
      }
    })
    output$mark3 <- renderUI({
      if (any(input$third == correct_answer[value$box3,1])){
        img(
          src = 'check.png',
          alt = 'Correct',
          width = 60
        )
      }
      else{
        img(
          src = 'cross.png',
          alt = 'Incorrect',
          width = 60
        )
      }
    })
    output$mark4 <- renderUI({
      if (any(input$fourth == correct_answer[value$box4,1])){
        img(
          src = 'check.png',
          alt = 'Correct',
          width = 60
        )
      }
      else{
        img(
          src = 'cross.png',
          alt = 'InCorrect',
          width = 60
        )
      }
    })

    ## Counting Mistakes
    if(any(input$first != correct_answer[value$box1,1])||
       any(input$second != correct_answer[value$box2,1])||
       any(input$third != correct_answer[value$box3,1])||
       any(input$fourth != correct_answer[value$box4,1]))
    {
      value$mistake <- value$mistake + 1
    }
    # when user gets all scenarios wrong, then the user has to reset the game.
    if(value$mistake == 4){
      updateButton(session, "reattempt", disabled = TRUE)
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }

    ## Counting Correct answers
    if(any(input$first == correct_answer[value$box1,1])&&
       any(input$second == correct_answer[value$box2,1])&&
       any(input$third == correct_answer[value$box3,1])&&
       any(input$fourth == correct_answer[value$box4,1]))
    {
      value$correct <- value$correct + 1
      updateButton(session, "reattempt", disabled = TRUE) #disable Reattempt button
    }
    # when the user misses all questions, then next button is disabled
    if(any(input$first != correct_answer[value$box1,1])&&
       any(input$second != correct_answer[value$box2,1])&&
       any(input$third != correct_answer[value$box3,1])&&
       any(input$fourth != correct_answer[value$box4,1]))
    {
      updateButton(session, "nextq", disabled = TRUE)
    }
    # when the user gets to the end, alert message shows up and reset the game
    if(value$correct == 10){
      confirmSweetAlert(
        session = session,
        inputId = "end",
        title = "Well Done!",
        type = "success",
        text = p("You have completed this challenge! Thank you for saving
                 this poor little man!"),
        btn_labels = "Game Over",
        btn_colors = "orange"
      )
    }
  })

  # Next Button Actions
  observeEvent(input$nextq, {
    updateButton(session, inputId = 'submit', disabled = FALSE)
    updateButton(session, inputId = 'nextq', disabled = TRUE)
    updateButton(session, inputId = 'reattempt', disabled = TRUE)

    # number of scenario is now the first value of the list
    value$index <- index_list$list[1]
    # remove the first value, so that next question starts with next value$index
    index_list$list = index_list$list[-1]
    # this keeps going until there are only 2 elements left
    # in the list in the maximum possible case.

    # value$box# should be updated
    value$box1= 1 + 4 * (value$index - 1)
    value$box2= 2 + 4 * (value$index - 1)
    value$box3= 3 + 4 * (value$index - 1)
    value$box4= 4 + 4 * (value$index - 1)

    output$mark1 <- renderUI({
      img(src = NULL)
    })
    output$mark2 <- renderUI({
      img(src = NULL)
    })
    output$mark3 <- renderUI({
      img(src = NULL)
    })
    output$mark4 <- renderUI({
      img(src = NULL)
    })
  })

  #### Reattempt button
  observeEvent(input$reattempt,{
    updateButton(session, 'submit', disabled = FALSE)
    updateButton(session, 'reattempt',disable =TRUE)

    # Update the first box
    updateSelectInput(session = session, inputId = 'first',
                      label = bank[(1 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the second box
    updateSelectInput(session = session, inputId = 'second',
                      label = bank[(2 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the third box
    updateSelectInput(session = session, inputId = 'third',
                      label = bank[(3 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the fourth box
    updateSelectInput(session = session, inputId = 'fourth',
                      label = bank[(4 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )

    # remove marks
    output$mark1 <- renderUI({
      img(src = NULL)
    })
    output$mark2 <- renderUI({
      img(src = NULL)
    })
    output$mark3 <- renderUI({
      img(src = NULL)
    })
    output$mark4 <- renderUI({
      img(src = NULL)
    })
  })

  #### Reset button
  observeEvent(input$reset,{
    # reset the index_list with another randomly generated fourteen numbers
    index_list$list<-c(sample(1:14,14,replace=FALSE))
    value$index <- 15
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index

    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "reattempt",disable =TRUE)
    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "reset", disabled = TRUE)

    # Update the first box
    updateSelectInput(session = session, inputId = 'first',
                      label = bank[(1 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the second box
    updateSelectInput(session = session, inputId = 'second',
                      label = bank[(2 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the third box
    updateSelectInput(session = session, inputId = 'third',
                      label = bank[(3 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the fourth box
    updateSelectInput(session = session, inputId = 'fourth',
                      label = bank[(4 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )

    # remove marks
    output$mark1 <- renderUI({
      img(src = NULL)
    })
    output$mark2 <- renderUI({
      img(src = NULL)
    })
    output$mark3 <- renderUI({
      img(src = NULL)
    })
    output$mark4 <- renderUI({
      img(src = NULL)
    })
    value$correct <- 0
    value$mistake <- 0
  })

  #when the game is over, alert message shows up
  observeEvent(input$end, {
    index_list$list<-c(sample(1:14,14,replace=FALSE))
    value$index <- 15
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index

    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "reattempt",disable =TRUE)
    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "reset", disabled = TRUE)

    # Update the first box
    updateSelectInput(session = session, inputId = 'first',
                      label = bank[(1 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the second box
    updateSelectInput(session = session, inputId = 'second',
                      label = bank[(2 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the third box
    updateSelectInput(session = session, inputId = 'third',
                      label = bank[(3 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )
    # Update the fourth box
    updateSelectInput(session = session, inputId = 'fourth',
                      label = bank[(4 + 4 * (value$index - 1)), 4],
                      choices = list('Select Answer', 'Increased Risk',
                                     'Odds', 'Odds Ratio', 'Probability',
                                     'Relative Risk', 'Risk')
    )

    output$mark1 <- renderUI({
      img(src = NULL)
    })
    output$mark2 <- renderUI({
      img(src = NULL)
    })
    output$mark3 <- renderUI({
      img(src = NULL)
    })
    output$mark4 <- renderUI({
      img(src = NULL)
    })
    value$correct <- 0
    value$mistake <- 0
  })

  #### Rlocker issue. Please deal with this - Bob or Neil. Thank you! ####
  # Gets current page address from the current session
  # getCurrentAddress <- function(session){
  #   return(paste0(
  #     session$clientData$url_protocol, "//",
  #     session$clientData$url_hostname,
  #     session$clientData$url_pathname, ":",
  #     session$clientData$url_port,
  #     session$clientData$url_search
  #   ))
  # }
  # observeEvent(input$submit,{
  #   value$box1= 4*value$index-3
  #   value$box2= 4*value$index-2
  #   value$box3= 4*value$index-1
  #   value$box4= 4*value$index
  #   statement <- rlocker::createStatement(
  #     list(
  #       verb = list(
  #         display = "answered"
  #       ),
  #       object = list(
  #         id = paste0(getCurrentAddress(session), "#", value$index),
  #         name = paste('Question', value$index, ":", bank[value$index*4,5]),
  #         description = paste(bank[value$box1,4], bank[value$box2,4],
  #                             bank[value$box3,4], bank[value$box4,4], sep =";")
  #       ),
  #       result = list(
  #         success = (any(input$first == correct_answer[value$box1,1])&&
  #                      any(input$second == correct_answer[value$box2,1])&&
  #                      any(input$third == correct_answer[value$box3,1])&&
  #                      any(input$fourth == correct_answer[value$box4,1])),
  #         completion = (any(input$first != 'Select Answer')&&
  #                         any(input$second != 'Select Answer')&&
  #                         any(input$third != 'Select Answer')&&
  #                         any(input$fourth != 'Select Answer')),
  #         response = paste(input$first, input$second, input$third,
  #                          input$fourth, correct_answer[value$box1,1],
  #                          correct_answer[value$box2,1],
  #                          correct_answer[value$box3,1],
  #                          correct_answer[value$box4,1], sep = ";"),
  #         duration = value$correct/10
  #         #the total questions number to reach win is 10, the duration is
  #         #the percentage of correct answers/total 10
  #       )
  #     )
  #   )
  #   # Store statement in locker and return status
  #   status <- rlocker::store(session, statement)
  # })

  ##### Draw the Hangman Game#####
  output$scoreTree <- renderImage({
    ## Background
    if(value$mistake == 0){
      return(list(
        src = "www/Cell01.jpg",
        contentType = "image/jpg",
        alt = "This is first tree which shows you are fine."
      ))
    }
    ## Head
    else if(value$mistake == 1 ) {
      return(list(
        src = "www/Cell02.jpg",
        contentType = "image/jpg",
        alt = "This is the second tree which shows you lose one life."
      ))
    }
    ## Arms
    else if(value$mistake == 2) {
      return(list(
        src = "www/Cell03.jpg",
        contentType = "image/jpg",
        alt = "This is the third tree which shows you only have two lives."
      ))
    }
    ## Body
    else if(value$mistake == 3 ) {
      return(list(
        src = "www/Cell04.jpg",
        contentType = "image/jpg",
        alt = "This is the fourth tree which shows you only have one life."
      ))
    }
    ## Legs
    else if(value$mistake == 4) {
      return(list(
        src = "www/Cell05.jpg",
        contentType = "image/jpg",
        alt = "This is the last tree which shows you are dead."
      ))
    }
  }, deleteFile=FALSE)
}

# Boast Call ---
boastUtils::boastApp(ui = ui, server = server)
