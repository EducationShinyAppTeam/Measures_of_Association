library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

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
          "In this app, you will explore measures of association for two by two tables and test your ability to distinguish probability, risk, relative risk, odds and odds ratio."),
        br(),
        
        h2("Instructions"),
        tags$ol(
          tags$li(
            "Review the different quantities on the ", actionLink("link_to_preq", "Prerequisites"), "page."
          ),
          tags$li(
            "You'll start the game with the man relaxing on top of the tree.
             If you miss a problem, he will fall to the next branch.
             You need to get ten questions right before he falls all the way to the ground."
                 ),
          tags$li(
            "For each scenario provided, choose the measure that goes with the numerical value."
                 ),
          tags$li(
            "If you get every question for the scenario correct then that counts toward your total of ten needed.
             If you miss any then the man will drop down to the next branch."
                 ),
          tags$li(
            "You cannot revise your answer once you click 'Submit', so think carefully before submit."
                 )
              ),
        br(),
        div(
          style = "text-align:center",
          bsButton(
            inputId = "nextbutton",
            label = "GO!",
            icon("bolt"),
            size = "large",
            class = "circle grow"
          )
        ),
        
        #Acknowledgements
        br(),
        br(),
        h2("Acknowledgements"),
        p(
          "This app was developed and coded by Zhiliang Zhang and further updated by Daehoon Gwak.",br(),
          "Special thanks to Luxin Wang for helping with some programming issues.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/22/2020 by DHG.")
         )
        ),
      
      ##Second tab - Prerequiste Tab
      tabItem(
        withMathJax(),
        tabName = "prerequisite",
        h2("Prerequisites"),
        br(),
        tags$ol(
          h2(tags$li("Probability")),
          p("Probability is the likelihood of an event in relation to all possible events."),
          p("Ex) If a horse wins 1 out of every 5 races, its probability of winning is 1/5 (20%)."),
          br(),
          h2(tags$li("Risk & Relative Risk")),
          p("A risk is a probability of a bad outcome."),
          p("Ex) If 1% of women's cars are stolen, the risk of a woman's car being stolen is 1 out of 100."),
          p("Relative risk (RR) is the ratio of probabilities for two groups."),
          p("Ex) The risk of having a heart attack in the next five years for men divided by the same risk for women."),
          p("In essence:"),
          p("\\[RR=\\frac{\\text{Risk for Group 1}}{\\text{Risk for Group 2}}\\]"),
          br(),
          h2(tags$li("Odds & Odds Ratio:")),
          p("Odds compare events with the opposite event."),
          p("Ex) If a horse wins 1 out of every 5 races, its odds of winning are 1 to 4
                                   (expressed as 1:4) since it wins one race for every 4 it loses."),
          p("An odds ratio  is the ratio of the odds for two groups."),
          p("Ex) the odds for having a heart attack in the next five years for
                                   men divided by the corresponding odds for women.")
        ),
        br(),
        div(
          style = "text-align:center",
          bsButton(
            inputId = "go",
            label = "GO!",
            icon("bolt"),
            size = "large",
            class = "circle grow"
          )
        )
      ),
      
      ## Third tab - Game Tab
      tabItem(
        tabName = "Hangman",
        h2("Choose the Correct Measure of Association"),
        
        
        wellPanel(
          uiOutput("question"),
          # tags$style(
          #   type = 'text/css',
          #   '#question {font-size: 20px; color: black;}'
          # )
          
        ),
        
        sidebarLayout(
          # Four small questions to choose appropriate answer
          sidebarPanel(
            # style = "background-color: #eaf2f8",
            # 
            # wellPanel(style = "background-color: #EAF2F8",
            #           fluidRow(
            #             uiOutput("result")
            #           )),
            # fluidRow(
            # 
            #   bsButton('reset','RELOAD', size = 'large', style = 'warning',disabled = TRUE),
            # ),

            fluidRow(
            p("Choose appropriate answers", style = 'text-align:center'),
            ), br(),
            
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
              ),
              width = '45%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark1'), style = 'position:absolute;top:21%;left:48%;'),
            
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
              ),
              width = '45%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark2'), style = 'position:absolute;top:41%;left:48%;'),
            
            
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
              ),
              width = '45%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark3'), style = 'position:absolute;top:60%;left:48%;'),
            
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
              ),
              width = '45%'
            ), style = 'margin-top:-29px;'),
            div(uiOutput('mark4'), style = 'position:absolute;top:80%;left:48%;'),
            br(),
            # tags$head(tags$style(HTML(
            #   "#result {font-weight:bold;}"
            # )))
          ),
          
          mainPanel(
            fluidRow(uiOutput("correct", align = 'center'),
              div(uiOutput("distPlot", width = "100%"), style = 'text-align:center')
              )
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
                label = "Next>>",
                size = "large",
                disabled = TRUE
              )
              # ,bsButton(
              #   'reset',
              #   'RESET',
              #   size = 'large',
              #   style = "danger",
              #   disabled = TRUE
              # )
            )
            , br(), br(), br(), br()
        )

        
      ),
      tabItem(
        tabName = "References",
        h2("References"),
        p(     #shinyjs
          class = "hangingindent",
          "Attali, D. (2020), Easily Improve the User Experience of Your Shiny Apps in Seconds.
            (v1.1), [R package]. Available from
            https://cran.r-project.org/web/packages/shinyjs/shinyjs.pdf"
        ),
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
        p(     #plotrix
          class = "hangingindent",
          "Lemon, J., Bolker, B., Oom, S., Klein, E., Rowlingson, B., Wickham, H., Tyagi, A., Eterradossi, O.,
          Grothendieck, G., Toews, M., Kane, J., Turner, R., Witthoft, C., Stander, J., Petzoldt, T., Duursma, R.,
          Biancotto, E., Levy, O., Dutang, C., Solymos, P., Engelmann, R., Hecker, M., Steinbeck, F., Borchers, H.,
          Singmann, H., Toal, T., Ogle, D., Baral, D., Groemping, U., and Venables, B., 
            (2020), plotrix: Various Plotting Functions
            (v3.7-8), [R package]. Available from
            https://cran.r-project.org/web/packages/plotrix/index.html"
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
