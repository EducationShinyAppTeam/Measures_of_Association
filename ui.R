library(shinydashboard)
library(shiny)
library(shinyBS)
library(boastUtils)

#Let us begin
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    titleWidth = 250,
    title = "Measures of Association",
    tags$li(
      class = "dropdown",
      actionLink("info", icon("info"))
    ),
    tags$li(
      class = "dropdown",
      tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
    )
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
    menuItem("Prerequisites", tabName = "prerequisite", icon = icon("book")),
    menuItem("Game", tabName = "game", icon = icon("gamepad")),
    menuItem("References", tabName = "References", icon = icon("leanpub"))
               ),
    tags$div(class = "sidebar-logo",
             boastUtils::psu_eberly_logo("reversed"))
    ),
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
        #Acknowledgements
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
            For example, if we were to imagine running a lottery to picka US
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
            developing skin cancer is about 2.6% (1/38) for white Americans.
            Thus, we if imagine a process of picking white Americans and
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
            cancer for a white American is \\(2.6\\%/0.1\\% = 0.026/0.001 = 26\\)
            times as large as Black Americans."),
          collapsible = TRUE,
          collapsed = TRUE,
          footer= "Relative Risk is a measure of association.",
          width = 12
        ),
        box(
          title = "Increased Risk",
          p("Increased Risk (IR) is relative risk expressed as a percentage
            increase over the lower risk group. As a formula \\[IR=
            \\left(\\text{Relative Risk}-1\\right)*100\\%\\] For example, since
            the relative risk of skin cancer for a white American is 26 times
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
            Letting \\(p=\\frac{x}{N}\\) represent the probability of a data event
            happening, \\[\\text{Odds}=\\frac{p}{1-p}=\\frac{x/N} {(N-x)/N}
            \\equiv X:(N-X)\\] We read odds notation, \\(X:Y\\) as 'X to Y'.",
            br(),
            "For example, the probability (risk) of a white American getting skin
            cancer is 2.6%, approximately \\(\\frac{1}{38}\\). Thus the odds of
            a white American getting skin cancer are 1:37. For Black Americans,
            the odds of getting skin cancer are 1:999."),
          collapsible = TRUE,
          collapsed = TRUE,
          footer= "Odds is not a measure of association.",
          width = 12
        ),
        box(
          title = "Odds Ratio",
          p("An odds ratio is the ratio of the odds for two groups of the same/
            similar data event. Suppose that the first group has a probability
            of the data event of p and the second group has a probability of r.
            Then \\[\\text{Odds Ratio}=\\frac{p}{1-p}\\bigg/\\frac{r}{1-r}\\]",
            br(),
            "For example, we found the odds of a white American (1:37) and a
            Black American (1:999) getting skin cancer. The odds ratio for these
            two groups would be \\[\\frac{1/38}{37/38}\\bigg/
            \\frac{1/1000}{999/1000}\\approx 26.667\\]"),
          collapsible = TRUE,
          collapsed = TRUE,
          footer= "Odds Ratio is a measure of association.",
          width = 12
        ),
        p(tags$em("Note:"), " there are other measures of association that are
          not covered by this app. For example, Pearson's Correlation, Spearman's
          Rho, and Kendall's Tau."),
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
        p("Read through each context carefuly. For the four given values,
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
