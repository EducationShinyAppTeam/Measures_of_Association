library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
#Let us begin
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    titleWidth = 250,
    title = "Measures of Association",
    tags$li(class = "dropdown",
            actionLink("info",icon("info",class = "myClass"))),
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
        p("In this app, you will explore measures of association to test 
           your ability to distinguish increased risk, probability, risk, 
           relative risk, odds, and odds ratio."),
        br(),
        h2("Instructions"),
        tags$ol(
          tags$li(
            "Review the different quantities on the ", 
            actionLink("link_to_preq", "Prerequisites"), "page."
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
                 ),
          tags$li(
            "Note: if you misidentify all of the values for a scenario, 
            you won't be able to move beyond that scenario 
            until you correctly identify at least one value. 
            Use the Re-attempt button to make a new attempt at identifying 
            the values in the current scenario. 
            Press the RESET button if you wish to start the game over."
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
          "This app was originally developed and coded by Zhiliang Zhang. 
          The app was further updated by Daehoon Gwak in June 2020.
          Special thanks to Luxin Wang for helping with some programming issues.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/29/2020 by DG.")
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
          p("Probability is the long-run relative frequency of a particular 
          data event occurring, given our assumptions about a repeatable process."),
          p("For example, if we were to imagine running a lottery to pick 
          a US citizen, the probability of picking a female is 50.8%. 
          Thus, 50.8% of the time we repeat carrying out this lottery 
          (always starting from the same initial population), 
            we will pick a female."),
          br(),
          tags$li("Risk"),
          p("Risk refers to the probability of a data event that 
            we view as being negative, undesirable, or 'bad'."),
          p("For example, the lifetime risk of developing skin cancer 
          is about 2.6% (1/38) for white Americans. Thus, we if imagine 
          a process of picking white Americans and observing their lifetimes, 
            2.6% of the time we will pick an individual who will develop 
            skin cancer."),
          br(),
          tags$li("Relative Risk"),
          p("Relative Risk (RR) is the ratio of two groups' risk (probability) 
          for a particular data event. We often calculate relative risk through 
          the formula \\[RR=\\frac{\\text{Risk for Group 1}}{\\text{Risk 
            for Group 2}}\\]"),
          p("For example, the relative risk of skin cancer for a white American 
            is \\(2.6\\%/0.1\\% = 0.0026/0.001 = 26\\) times as large 
            as Black Americans."),
          br(),
          tags$li("Increased Risk"),
          p("Increased Risk (IR) is relative risk expressed as a percentage 
            increase over the lower risk group."),
          p("As a formula \\[IR=\\text{(Relative Risk-1)}*100%\\]
            For example, if the relative risk of skin cancer for 
            a white American is 1.4 times higher compared to Black Americans, 
            then the increased risk is 40%."),
          br(),
          tags$li("Odds"),
          p("Odds is the ratio of two probabilities--the probability of 
          a data event and the probability of that data event not happening 
            (i.e., the complement or the opposite event). 
            There are two ways that odds are expressed: 
            as a fraction or using 'odds notation' with a colon. 
            Letting \\(p=\\frac{x}{N}\\) represent the probability of 
            a data event happening \\[\\text{Odds}=\\frac{p}{1-p}=\\frac{x/N}
            {(N-x)/N}\\equiv X:(N-X)\\] We read odds notation, \\(X:Y\\) 
            as 'X to Y'."),
          p("For example, if a White American gets 1 out of every 5 skin 
            cancer cases, its odds of getting skin cancer are 1 to 4 
            (expressed as 1:4)."),
          br(),
          tags$li("Odds Ratio"),
          p("An odds ratio is the ratio of the odds for two groups of 
          the same/similar data event. Suppose that the first group has a 
          probability of the data event of p and the second group has a 
          probability of r. Then \\[\\text{Odds Ratio}=\\frac{p}{1-p}\\
            bigg/\\frac{r}{1-r}\\]."),
          p("For example, if the odds that a White American has 
          a skin cancer in the next five years is 1 to 4 and the odds that 
          a Black American has a skin caner in the next five years is 1 to 20, 
            then the odds ratio is 5 = 1/4 divided by 1/20.")
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
            #uiOutput('box1', style = 'position:absolute;right:10%;'),
            div(selectInput(
              inputId = 'first',
              label = uiOutput('box1'),
              c(
                'Select Answer',
                'Increase Risk',
                'Odds',
                'Odds Ratio',
                'Probability',
                'Relative Risk',
                'Risk'
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark1'), style = 'position:absolute;top:20%;right:8%;'),
            br(),
            #uiOutput('box2'),
            div(selectInput(
              inputId = 'second',
              label = uiOutput('box2'),
              c(
                'Select Answer',
                'Increase Risk',
                'Odds',
                'Odds Ratio',
                'Probability',
                'Relative Risk',
                'Risk'
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark2'), style = 'position:absolute;top:40%;right:8%;'),
            br(),
            #uiOutput('box3'),
            div(selectInput(
              inputId = 'third',
              label = uiOutput('box3'),
              c(
                'Select Answer',
                'Increase Risk',
                'Odds',
                'Odds Ratio',
                'Probability',
                'Relative Risk',
                'Risk'
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark3'), style = 'position:absolute;top:58%;right:8%;'),
            br(),
            #uiOutput('box4'),
            div(selectInput(
              inputId = 'fourth',
              label = uiOutput('box4'),
              c(
                'Select Answer',
                'Increase Risk',
                'Odds',
                'Odds Ratio',
                'Probability',
                'Relative Risk',
                'Risk'
              ), width = '90%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark4'), style = 'position:absolute;top:77%;right:8%;'),
            br(),
          )),
          column(8, # score tree
            uiOutput("correct", align = 'center'),
              div(uiOutput("scoreTree"), align = 'center')
            # tags$script(HTML(
            # "#(document).ready(function() {
            # document.getElementById('scoreTree').setAttribute('aria-label',
            # 'This is the tree which shows how many chances you have left.')
            # })"
            # ))
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
