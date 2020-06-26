library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets) #for 'sendSweetAlert' function
library(boastUtils)
#Let`s begin
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    titleWidth = 250,
    title = "Measures of Association",
    tags$li(
      class = "dropdown",
      tags$a(href = "https://shinyapps.science.psu.edu/",
             icon("home"))
    )
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
    menuItem("Prerequisites", tabName = "prerequisite", icon = icon("book")),
    menuItem("Game", tabName = "Hangman", icon = icon("gamepad")),
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
      ##First tab - Overview Tab
      tabItem(
        tabName = "overview",
        h1("Measures of Association"),
        p(
          "In this app, you will explore measures of association for two by two tables 
          and test your ability to distinguish probability, risk, relative risk, odds and odds ratio."),
        br(),
        h2("Instructions"),
        tags$ol(
          tags$li(
            "Review the different quantities on the ", actionLink("link_to_preq", "Prerequisites"), "page."
          ),
          tags$li(
            "You'll start the game with the man relaxing on top of the tree.
             If you miss a question, he will fall to the next branch.
             You need to get ten scenarios right before he falls all the way to the ground."
                 ),
          tags$li(
            "For each scenario provided, choose the measure that goes with the numerical value."
                 ),
          tags$li(
            "If you get every question for the scenario correct then that counts toward your total of ten needed.
             If you miss any, then the man will drop down to the next branch."
                 ),
          tags$li(
            "If you miss all questions on the scenario, you cannot proceed to the next scenario and
             you have to click 'Re-attempt' button to try again that scenario."
                 ),
          tags$li(
            "If you miss all chances, the man falls to the ground, and you can click 'RESET' button
            to play again. Remember, you have 4 chances!"
          )
          ),
        br(),
        div(
          style = "text-align:center",
          bsButton(
            inputId = "nextbutton",
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
          "This app was developed and coded by Zhiliang Zhang and further updated by Daehoon Gwak.
          Special thanks to Luxin Wang for helping with some programming issues.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/26/2020 by DHG.")
         )
        ),
      ##Second tab - Prerequiste Tab
      tabItem(
        withMathJax(),
        tabName = "prerequisite",
        h2("Prerequisites"),
        br(),
        tags$ol(
          tags$li("Probability"),
          p("Probability is the long-run relative frequency of a particular data even occurring, 
            given our assumptions about a repeatable process."),
          p("For example, if we were to imagine running a lottery to pick a US citizen,
            the probability of pecking a female is 50.8%. 
            Thus, 50.8% of the time we repeat carrying out this lottery 
            (always starting from the same initial population), we will pick a female. 
            (Note: we won't pick the exact same person each time.)"),
          br(),
          tags$li("Risk"),
          p("Risk refers to the probability of a data event that we view as being negative, undesirable, or 'bad'."),
          p("For example, the lifetime risk of developing skin cancer is about 2.6% (1/38) for white Americans. 
            Thus, we if imagine a process of picking white Americans and observing their lifetimes, 
            2.6% of the time we will pick an individual who will develop skin cancer."),
          br(),
          tags$li("Relative Risk"),
          p("Relative Risk (RR) is the ratio of two groups' risk (probability) for a particular data event. 
            We often calculate relative risk through the formula 
            \\[RR=\\frac{\\text{Risk for Group 1}}{\\text{Risk for Group 2}}\\]"),
          p("For example, the relative risk of skin cancer for a white American is 
            \\(2.6\\%/0.1\\% = 0.0026/0.001 = 26\\) times as large as Black Americans."),
          br(),
          tags$li("Odds & Odds Ratio"),
          p("Odds, often expressed as a ratio called an Odds Ratio, compares the probability a data event 
            happen with the probability of the same event not happen (i.e., the complement)."),
          p("The odds ratio of a white American getting skin cancer is \\(\\frac{2.6}{100-2.6}\\approx 0.0267\\). 
             Thus, the probability (risk) of a white American getting skin cancer is 0.0267 times 
            as large as the probability of them not getting skin cancer. 
            Put another way, we can say that the odds of a white American getting skin cancer is 1 to 37. 
            The odds of a black American getting skin cancer are 1 to 999.")
        ),
        br(),
        div(
          style = "text-align:center",
          bsButton(
            inputId = "go",
            label = "GO!",
            icon("bolt"),
            size = "large"
          )
        )
      ),
      ## Third tab - Game Tab
      tabItem(
        tabName = "Hangman",
        h2("Choose the Correct Measure of Association"),
        uiOutput("question"),
          # Four small questions to choose appropriate answer
            fluidRow(
              column(4,
                     wellPanel(
            p("Choose appropriate answers", style = 'text-align:center'),
            br(),
            uiOutput('box1'),
            div(selectInput(
              inputId = 'first',
              "",
              c(
                "Select Answer",
                'Relative Risk',
                'Risk',
                'Odds',
                'Odds Ratio',
                "Probability"
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark1'), style = 'position:absolute;top:21%;right:8%;'),
            br(),
            uiOutput('box2'),
            div(selectInput(
              'second',
              "",
              c(
                "Select Answer",
                'Relative Risk',
                'Risk',
                'Odds',
                'Odds Ratio',
                "Probability"
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark2'), style = 'position:absolute;top:41%;right:8%;'),
            br(),
            uiOutput('box3'),
            div(selectInput(
              'third',
              "",
              c(
                "Select Answer",
                'Relative Risk',
                'Risk',
                'Odds',
                'Odds Ratio',
                "Probability"
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark3'), style = 'position:absolute;top:60%;right:8%;'),
            br(),
            uiOutput('box4'),
            div(selectInput(
              'fourth',
              "",
              c(
                "Select Answer",
                'Relative Risk',
                'Risk',
                'Odds',
                'Odds Ratio',
                "Probability"
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark4'), style = 'position:absolute;top:80%;right:8%;'),
            br(),
          )),
          column(8, 
            uiOutput("correct", align = 'center'),
              div(uiOutput("scoreTree"), align = 'center')
            )
          ),
            fluidRow(
              column(1,
              bsButton(
                inputId = 'submit',
                label = "Submit",
                size = "large",
                style = "warning",
                disabled = FALSE
              )
            ),
            column(1, offset = 4,
                   bsButton(
                     'restart',
                     'Re-attempt',
                     size = 'large',
                     style = "danger",
                     disabled = TRUE
                     )
            ),
            column(1, offset = 4,
              bsButton(
                inputId = 'nextq',
                label = "Next >>",
                size = "large",
                disabled = TRUE
              ),
              bsButton(
                inputId = 'reset',
                label = "RESET",
                size = "large",
                disabled = TRUE
              )
            ),br()
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
        )
      )
    )
  )
)
