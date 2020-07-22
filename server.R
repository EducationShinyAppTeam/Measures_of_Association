library(boastUtils)

# Read in questions ----
bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

# Set up shiny server ---
shinyServer(function(session, input, output) {
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
    updateTabItems(session, "tabs", "prerequisite")
  })
  
  # Define the two go2 buttons
  observeEvent(input$go1, {
    updateTabItems(session, "tabs", "prerequisite")
  })
  
  observeEvent(input$go2, {
    updateTabItems(session, "tabs", "game")
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
  
  observeEvent(input$tabs, {
    if (input$tabs == "game") {
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
})
