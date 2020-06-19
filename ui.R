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
    menuItem("Challenge", tabName = "Hangman", icon = icon("cogs")),
    menuItem("Reference", tabName = "References", icon = icon("leanpub"))
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
          "In this app, you will explore measures of associations and test your ability to distinguish probability, risk, relative risk, odds and odds ratio."
        ),
        br(),
        
        h2("Instructions"),
        tags$ol(
          tags$li(
            "You'll start this game with nothing on the gallows, once you have at least one answer wrong, 
            a part of the body will be drawn,and if the whole little man is completely drawn, 
            then you have lost this game."
                 ),
          tags$li(
            "Choose different measure of associations for each numeric value, then click 'Submit' to check your answer."
                 ),
          tags$li(
            "If you got every question correct, then you can click
                                           'Next Question' to move on your challenge, otherwise a part of body will be drawn on the image."
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
            label = "Explore prerequisites",
            icon("wpexplorer"),
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
          "Special thanks to Luxin Wang for helping some programming issues.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/19/2020 by DHG.")
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
          #tags$a(tags$img(src = 'rr.png', align = "center")),   #update to mathjax
          p("\\[RR=\\frac{\\text{Risk for Group 1}}{\\text{Risk for Group 2}}\\]"),
          br(),
          br(),
          h2(tags$li("Odds & Odds Ratio:")),
          p("Odds compare events with the opposite event."),
          p("Ex) If a horse wins 1 out of every 5 races, its odds of winning are 1 to 4
                                   (expressed as 1:4) since it wins one race for every 4 it loses."),
          p("An odds ratio  is the ratio of the odds for two groups."),
          p("Ex) the odds for having a heart attack in the next five years for
                                   men divided by the corresponding odds for women.")
        ),
        #h4("When data is displayed in a 2 x 2 table,
        #   the odds ratio is sometimes called the 'cross product ratio' as its estimate is calculated as the product of the values on one
        #   diagonal divided by the product of the values on the opposite diagonal."),
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
      
      ## Third tab - Challenge Tab
      tabItem(
        tabName = "Hangman",
        h2("Choose the Correct Measure of Association"),
        
        
        wellPanel(
          uiOutput("question"),
          tags$style(
            type = 'text/css',
            '#question {font-weight:bold;font-size: 20px; color: black;}'
          )
          
        ),
        
        sidebarLayout(
          sidebarPanel(div(
            #style = "background-color: #eaf2f8",
            
            #wellPanel(style = "background-color: #EAF2F8",
            #          fluidRow(
            #            uiOutput("result")
            #          )),
            #fluidRow(
            
            #  bsButton('reset','RELOAD', size = 'large', style = 'warning',disabled = TRUE),
            #  bsButton('restart','RESTART',size = 'large', style = 'primary',disabled = TRUE)
            #),
            
            fluidRow(
              h3("Choose the measure of association for the following: "),
              uiOutput('box1'),
              selectInput(
                'first',
                "",
                c(
                  "Select Answer",
                  'Relative Risk',
                  'Risk',
                  'Odds',
                  'Odds Ratio',
                  "Probability"
                ),
                width = '30%'
              ),
              uiOutput('mark1'),
              uiOutput('box2'),
              selectInput(
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
                width = '30%'
              ),
              uiOutput('mark2')
            ),
            fluidRow(
              uiOutput('box3'),
              selectInput(
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
                width = '30%'
              ),
              uiOutput('mark3'),
              uiOutput('box4'),
              selectInput(
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
                width = '30%'
              ),
              uiOutput('mark4')
            ),
            
            br(),
            div(style = "text-align:left",
                fluidRow(
                    bsButton(
                      inputId = 'submit',
                      label = "Submit",
                      size = "large",
                      style = "warning",
                      disabled = FALSE
                    )
                 ), br(),
                fluidRow(
                    bsButton(
                      inputId = 'nextq',
                      label = "Next>>",
                      size = "large",
                      style = "success",
                      disabled = TRUE
                      )
                  )
                ),
            br(),
            br(),
            
            
            tags$head(tags$style(HTML(
              "#result {font-weight:bold;}"
            )))
            
            
          )),
          
          mainPanel(
            br(),
            
            fluidRow(uiOutput("correct", align = 'center')),
            
            br(),
            br(),
            
            fluidRow(column(
              6, offset = 2,
              uiOutput("distPlot", width = "100%")
            )),
            br(),
            br(),
            br()
            
            # bsPopover("distPlot", " ","Choose different measure of association for each numeric value, then click Submit to check your answer", place="left")
            
            
          ),
          position = "left"
          
        )
        
        
        ####Previous Layout####
        
        # wellPanel(
        #
        #   fluidRow(
        #     h3("Identify the measure association of the following numeric values: ")
        #   ),
        #
        #   column(3,
        #         selectInput('first',uiOutput('box1'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
        #         uiOutput('mark1')),
        #   column(3,
        #          selectInput('second',uiOutput('box2'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
        #          uiOutput('mark2')),
        #   column(3,
        #          selectInput('third',uiOutput('box3'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
        #          uiOutput('mark3') ),
        #   column(3,
        #          selectInput('fourth',uiOutput('box4'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
        #          uiOutput('mark4')),
        #   # fluidRow(
        #   #   column(4, offset= 7,
        #   #    verbatimTextOutput("result")
        #   #   )
        #   # ),
        #
        # fluidRow(
        # column(3, offset=7,
        #          verbatimTextOutput("result"))),
        #
        # plotOutput("distPlot",width = "50%"),
        #
        #
        # column(2, offset=6,
        #        bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)),
        # column(2, offset = 6,
        #        bsButton('submit', "Submit", size= "large", style ="warning", disabled =FALSE)),
        #
        # tags$head(tags$style(HTML("#result {font-size: 18px;background-color:white}"))),
        #
        # bsPopover("disPlot", " ","Choose different measure of association for each numeric value, then click Submit to check your answer", place="right"),
        #
        # br(),
        # br(),
        # br(),
        # br(),
        # br()
        #
        #
        #
        # )
        
        
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
