library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Measures of Association", titleWidth = 250,
                                    tags$li(class = "dropdown",
                                            tags$a(href = "https://shinyapps.science.psu.edu/",
                                                   icon("home"))),
                                    tags$li(class = "dropdown",
                                            actionLink("info", icon("info"), class = "myClass"))),
                    dashboardSidebar(
                      sidebarMenu(
                        id ="tabs",
                        menuItem("Prerequiste",tabName = "prerequisite",icon = icon("book")),
                        menuItem("Overview", tabName = "overview",icon = icon("dashboard")),
                        menuItem("Challenge", tabName = "Hangman",icon = icon("cogs"))
                      )
                    ),
                    dashboardBody(
                      
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
                        tags$style(HTML(
                          '.popover-title{
                          color:black;
                          font-size:18px;
                          background-color: orange
                          }'
                        ))
                        
                        ),
                      
                      tabItems( 
                        # Prerequiste Tab
                        tabItem(tabName = "prerequisite",
                                h3(strong("Background: Measures of Association")),
                                br(),
                                h3(tags$li("Probability:")),
                                h4("Probability is the likelihood of an event in relation to all possible events."),
                                h5("Ex) If a horse wins 1 out of every 5 races, its probability of winning is 1/5 (20%)."),
                                br(),
                                h3(tags$li("Risk & Relative Risk:")),
                                h4("A risk is a probability of a bad outcome."),
                                h5("Ex) If 35% of women's cars are stolen, the risk of a woman's car being stolen is 35%."),
                                h4("Relative risk (RR) is the ratio of probabilities for two groups."), 
                                h5("Ex) The risk of having a heart attack in the next five years for men divided by the same risk for women."), 
                                h4("In essence:"), 
                                tags$a(tags$img(src='rr.png', align = "center")),
                                
                                br(),
                                h3(tags$li("Odds & Odds Ratio:")),
                                h4("Odds compare events with the opposite event."), 
                                h5("Ex) If a horse wins 1 out of every 5 races, its odds of winning are 1 to 4 (expressed as 1:4) since it wins one race for every 4 it loses."), 
                                h4("An odds ratio  is the ratio of the odds for two groups."), 
                                h5("Ex) the odds for having a heart attack in the next five years for men divided by the corresponding odds for women."),  
                                #h4("When data is displayed in a 2 x 2 table, 
                                #   the odds ratio is sometimes called the 'cross product ratio' as its estimate is calculated as the product of the values on one 
                                #   diagonal divided by the product of the values on the opposite diagonal."),
                                br(),
                                
                                
                                div(style = "text-align:center",
                                    bsButton("nextbutton", "Go to the overview", icon("wpexplorer"), size = "large",style = "warning"))),
                        
                        #Overview Tab
                        tabItem(tabName = "overview",
                                tags$a(href='http://stat.psu.edu/',   tags$img(src='logo.png', align = "left", width = 180)),
                                br(),
                                br(),
                                br(),
                                h3(strong("About:")),
                                h4("In this App, you will explore measures of associations and test your ability to distinguish probability, risk, relative risk, odds and odds ratio."),
                                br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("You'll start this game with nothing on the gallows, once you have at least one answer wrong, a part of the body will be drawn, and if the whole little man is completely drawn, then you have lost this game.")),
                                h4(tags$li("Choose different measure of associations for each numeric value, then click 'Submit' to check your answer.")),
                                h4(tags$li("If you got every question correct, then you can click 'Next Question' to move on your challenge, otherwise a part of body will be drawn on the image.")),
                                h4(tags$li("You cannot revise your answer once you click 'Submit', so think carefully before submit.")),
                                br(),
                                div(style = "text-align:center",
                                    bsButton("go", "GO!", icon("bolt"), size = "medium", style = "warning", class = "circle grow")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Zhiliang Zhang. Special thanks to Luxin Wang for helping some programming issues.")
                        ),
                        
                        
                        
                        # Challenge Tab
                        
                        tabItem(tabName = "Hangman",
                                #div(style="display: inline-block;vertical-align:top;",
                                #    tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                #),
                                #div(style="display: inline-block;vertical-align:top;",
                                #    circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
                                #),
                                titlePanel("Distinguish the measure associations"),
                                
                                
                                wellPanel(style = "background-color: #EAF2F8",
                                          
                                          uiOutput("question"),
                                          tags$style(type='text/css', '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}')
                                          
                                ),
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    
                                    wellPanel(style = "background-color: #EAF2F8",
                                              fluidRow(
                                                uiOutput("result")
                                              )),
                                    fluidRow(
                                      
                                      bsButton('reset','RELOAD', size = 'large', style = 'warning',disabled = TRUE),
                                      bsButton('restart','RESTART',size = 'large', style = 'primary',disabled = TRUE)
                                    ),
                                    
                                    fluidRow(
                                      h3("Identify the measure association of the following numeric values: ")
                                    ),
                                    
                                    fluidRow(uiOutput('box1'), selectInput('first',"",c('Select Answer','Relative Risk', 'Risk', 'Odds','Odds Ratio', "Probability"), width='30%'),
                                             uiOutput('mark1')),
                                    fluidRow(uiOutput('box2'),selectInput('second',"",c('Select Answer','Relative Risk', 'Risk',  'Odds','Odds Ratio', "Probability"), width='30%'),
                                             uiOutput('mark2')),
                                    fluidRow(uiOutput('box3'),selectInput('third',"",c('Select Answer','Relative Risk', 'Risk',  'Odds','Odds Ratio', "Probability"), width='30%'),
                                             uiOutput('mark3')),
                                    fluidRow(uiOutput('box4'),selectInput('fourth',"",c('Select Answer','Relative Risk', 'Risk', 'Odds', 'Odds Ratio', "Probability"), width='30%'),
                                             uiOutput('mark4')),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    
                                    
                                    
                                    
                                    tags$head(tags$style(HTML("#result {font-weight:bold;}")))
                                    
                                    
                                  ),
                                  mainPanel(
                                    
                                    br(),
                                    
                                    fluidRow(
                                      uiOutput("correct", align = 'center')
                                    ),
                                    
                                    br(),
                                    br(),
                                    
                                    fluidRow(
                                      column(6, offset = 2,
                                             uiOutput("distPlot",width = "100%"))),
                                    br(),
                                    br(),
                                    br(),
                                    fluidRow(
                                      column(3, offset=2,
                                             bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)),
                                      column(3, 
                                             bsButton('submit', "Submit", size= "large", style ="warning", disabled =FALSE)))
                                    # bsPopover("distPlot", " ","Choose different measure of association for each numeric value, then click Submit to check your answer", place="left")
                                    
                                    
                                  ),
                                  position ="left"
                                  
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
                                
                                
                        )
                        
                                )
                      
                        )
                    
                      )




