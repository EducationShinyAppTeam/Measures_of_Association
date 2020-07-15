library(boastUtils)
#Let`s begin

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

  # Define the two go buttons
  observeEvent(input$nextbutton, {
    updateTabItems(session, "tabs", "prerequisite")
    })

  observeEvent(input$go, {
    updateTabItems(session, "tabs", "Hangman")
  })

  # Daehoon: Replace going to references with just starting the game over.
  observeEvent(input$end, {
    updateTabItems(session, "tabs", "References")
  })

  # Submit Button Actions
  # Daehoon: place everything that should happen when the user presses the
  # submit button in this one observeEvent call
  observeEvent(input$submit, {
    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session, "submit", disabled = TRUE)
    updateButton(session, "restart", disabled = FALSE)
  })

  # Daehoon: do the same for the Re-attempt button, the next button, and the
  # reset button; each one gets ONE observeEvent call.

  # Daehoon: Delete these two comments
  #scenario = 1
  #trythis2 = bank[scenario,4]

  # Daehoon: make everything explicit in the update* functions
  ## Also, if you aren't changing the values (i.e., the choices in the
  ## selectInputs), you don't need to list them again.
  observeEvent(input$nextq, {
    updateButton(session, inputId = 'submit', disabled = FALSE)
    updateButton(session, inputId = 'nextq', disabled = TRUE)
    updateButton(session, inputId = 'restart', disabled = TRUE)
    updateSelectInput(session, inputId = 'first', label = input$box1,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'second', label = input$box2,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'third', label = input$box3,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'fourth', label = input$box4,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
  })
  #### Reattempt button
  observeEvent(input$restart,{
    updateButton(session, 'submit', disabled = FALSE)
    updateButton(session, 'restart',disable =TRUE)
    updateSelectInput(session, inputId = 'first', label = input$box1,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'second', label = input$box2,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'third', label = input$box3,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'fourth', label = input$box4,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    #Daehoon: use this as your template for the marks
    output$mark1 <- renderImage({
      return(NULL)
      deleteFile = FALSE
    })
    output$mark2 <- renderUI({
      img(src = NULL, width = 20)
    })
    output$mark3 <- renderUI({
      img(src = NULL, width = 20)
    })
    output$mark4 <- renderUI({
      img(src = NULL, width = 20)
    })
  })
  #### Reset button
  observeEvent(input$reset,{
    index_list$list<-c(index_list$list,sample(2:15,14,replace=FALSE))
    value$index <- 15
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index
    correct_answer <- as.matrix(bank[1:60,1])

    # Daehoon: see prior comment about update* calls
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "restart",disable =TRUE)
    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "reset", disabled = TRUE)
    updateSelectInput(session, inputId = 'first', label = input$box1,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'second', label = input$box2,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'third', label = input$box3,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    updateSelectInput(session, inputId = 'fourth', label = input$box4,
                      c('Select Answer', 'Increase Risk', 'Odds', 'Odds Ratio',
                        'Probability', 'Relative Risk', 'Risk'))
    # Daehoon: use the following as the template for the marks
    output$mark1 <- renderImage({
      return(NULL)
      deleteFile = FALSE
    })
    output$mark2 <- renderUI({
      img(src = NULL, width = 30)
    })
    output$mark3 <- renderUI({
      img(src = NULL, width = 30)
    })
    output$mark4 <- renderUI({
      img(src = NULL, width = 30)
    })
    value$correct <- 0
    value$mistake <- 0
  })
  #Score logic
  value <- reactiveValues(index =  15, mistake = 0, correct = 0)
  correct_answer <- as.matrix(bank[1:60,1])
  index_list<-reactiveValues(list=sample(1:14,14,replace=FALSE))
  ## start challenge
  observeEvent(input$go | input$Hangman,{
    value$index <- 15 #15th scenario on question bank
    correct_answer <- as.matrix(bank[1:60,1])
    value$box1= 4*value$index-3 #60th question -3, so that is the first question on 15th scenario
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index
  })
  ## after clicking next >>
  observeEvent(input$nextq,{
    index_list$list=index_list$list[-1]
    value$index <- index_list$list[1]
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index
    if(length(index_list$list) == 1){
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset",disabled = FALSE)
    }
    # Daehoon: use this as the template for the marks
    output$mark1 <- renderImage({
      return(NULL)
      deleteFile = TRUE
    })
    output$mark2 <- renderUI({
      img(src = NULL, width = 30)
    })
    output$mark3 <- renderUI({
      img(src = NULL, width = 30)
    })
    output$mark4 <- renderUI({
      img(src = NULL, width = 30)
    })
  })
  # UI for score
  output$correct <- renderUI({
    p("Number of completed scenarios: " , value$correct, " out of 10")
  })
  observeEvent(input$submit,{
    output$correct <- renderUI({
      p("Number of completed scenarios: " , value$correct, " out of 10" )
    })
  })
  ########################## Hard Code Starts #######################
  #### Questions
  output$question <- renderUI({
    if(value$index == 1){ # ex) this is the question for Q1 on question bank.
      bank[1,5]
    }
    else if(value$index == 2){
      bank[5,5]
    }
    else if(value$index == 3){
      bank[9,5]
    }
    else if(value$index == 4){
      bank[13,5]
    }
    else if(value$index == 5){
      bank[17,5]
    }
    else if(value$index == 6){
      bank[21,5]
    }
    else if(value$index == 7){
      bank[25,5]
    }
    else if(value$index == 8){
      bank[29,5]
    }
    else if(value$index == 9){
      bank[33,5]
    }
    else if(value$index == 10){
      bank[37,5]
    }
    else if(value$index == 11){
      bank[41,5]
    }
    else if(value$index == 12){
      bank[45,5]
    }
    else if(value$index == 13){
      bank[49,5]
    }
    else if(value$index == 14){
      bank[53,5]
    }
    else if(value$index == 15){
      bank[57,5]
    }
  })

  ## Daehoon: the following code will replace all of lines 276-467

  # Put the values to be identified as labels
  observe({
    # Update the first box
    updateSelectInput(
      session = session,
      inputId = "first",
      label = bank[(1 + 4 * (value$index - 1)), 4]
    )
    # Update the second box
    updateSelectInput(
      session = session,
      inputId = "second",
      label = bank[(2 + 4 * (value$index - 1)), 4]
    )
    ## Daehoon: continue this for the third and fourth boxes
  })

  #### Questions - first number on each scenario
  output$box1 <- renderUI({
    if(value$index ==1){
      bank[1,4]
    }
    else if(value$index ==2){
      bank[5,4]
    }
    else if(value$index == 3){
      bank[9,4]
    }
    else if(value$index == 4){
      bank[13,4]
    }
    else if(value$index == 5){
      bank[17,4]
    }
    else if(value$index == 6){
      bank[21,4]
    }
    else if(value$index == 7){
      bank[25,4]
    }
    else if(value$index == 8){
      bank[29,4]
    }
    else if(value$index == 9){
      bank[33,4]
    }
    else if(value$index == 10){
      bank[37,4]
    }
    else if(value$index == 11){
      bank[41,4]
    }
    else if(value$index == 12){
      bank[45,4]
    }
    else if(value$index == 13){
      bank[49,4]
    }
    else if(value$index == 14){
      bank[53,4]
    }
    else if(value$index == 15){
      bank[57,4]
    }
  })
  #### Questions - second number on each scenario
  output$box2 <- renderUI({
    if(value$index ==1){
      bank[2,4]
    }
    else if(value$index ==2){
      bank[6,4]
    }
    else if(value$index == 3){
      bank[10,4]
    }
    else if(value$index == 4){
      bank[14,4]
    }
    else if(value$index == 5){
      bank[18,4]
    }
    else if(value$index == 6){
      bank[22,4]
    }
    else if(value$index == 7){
      bank[26,4]
    }
    else if(value$index == 8){
      bank[30,4]
    }
    else if(value$index == 9){
      bank[34,4]
    }
    else if(value$index == 10){
      bank[38,4]
    }
    else if(value$index == 11){
      bank[42,4]
    }
    else if(value$index == 12){
      bank[46,4]
    }
    else if(value$index == 13){
      bank[50,4]
    }
    else if(value$index == 14){
      bank[54,4]
    }
    else if(value$index == 15){
      bank[58,4]
    }
  })
  #### Questions - third number on each scenario
  output$box3 <- renderUI({
    if(value$index == 1){
      bank[3,4]
    }
    else if(value$index == 2){
      bank[7,4]
    }
    else if(value$index == 3){
      bank[11,4]
    }
    else if(value$index == 4){
      bank[15,4]
    }
    else if(value$index == 5){
      bank[19,4]
    }
    else if(value$index == 6){
      bank[23,4]
    }
    else if(value$index == 7){
      bank[27,4]
    }
    else if(value$index == 8){
      bank[31,4]
    }
    else if(value$index == 9){
      bank[35,4]
    }
    else if(value$index == 10){
      bank[39,4]
    }
    else if(value$index == 11){
      bank[43,4]
    }
    else if(value$index == 12){
      bank[47,4]
    }
    else if(value$index == 13){
      bank[51,4]
    }
    else if(value$index == 14){
      bank[55,4]
    }
    else if(value$index == 15){
      bank[59,4]
    }
  })
  #### Questions - fourth number on each scenario
  output$box4 <- renderUI({
    if(value$index == 1){
      bank[4,4]
    }
    else if(value$index == 2){
      bank[8,4]
    }
    else if(value$index == 3){
      bank[12,4]
    }
    else if(value$index == 4){
      bank[16,4]
    }
    else if(value$index == 5){
      bank[20,4]
    }
    else if(value$index == 6){
      bank[24,4]
    }
    else if(value$index == 7){
      bank[28,4]
    }
    else if(value$index == 8){
      bank[32,4]
    }
    else if(value$index == 9){
      bank[36,4]
    }
    else if(value$index == 10){
      bank[40,4]
    }
    else if(value$index == 11){
      bank[44,4]
    }
    else if(value$index == 12){
      bank[48,4]
    }
    else if(value$index == 13){
      bank[52,4]
    }
    else if(value$index == 14){
      bank[56,4]
    }
    else if(value$index == 15){
      bank[60,4]
    }
  })
  ## check marks ##
  #Daehoon: see prior comment about one observeEvent for submit
  observeEvent(input$submit,{
    ## Daehoon: use the following as the template for the other marks
    output$mark1 <- renderImage({
      if (any(input$first == correct_answer[value$box1,1])){
        return(list(
          src = "www/check.png",
          contentType = "image/png",
          alt = "Correct",
          width = 60
        ))
      }
      else{
        return(list(
          src = "www/cross.png",
          contentType = "image/png",
          alt = "Incorrect",
          width = 60
        ))
      }
      deleteFile = FALSE
    })
    output$mark2 <- renderUI({
      if (any(input$second == correct_answer[value$box2,1])){
        img(src = "check.png", width = 30)
      }
      else{
        img(src = "cross.png", width = 30)
      }
     })
    output$mark3 <- renderUI({
      if (any(input$third == correct_answer[value$box3,1])){
        img(src = "check.png", width = 30)
      }
      else{
        img(src = "cross.png", width = 30)
      }
    })
    output$mark4 <- renderUI({
      if (any(input$fourth == correct_answer[value$box4,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
      }
    })
  })
  ################ Counting Mistakes ###############
  observeEvent(input$submit,{
    if(any(input$first != correct_answer[value$box1,1])||
       any(input$second != correct_answer[value$box2,1])||
       any(input$third != correct_answer[value$box3,1])||
       any(input$fourth != correct_answer[value$box4,1]))
    {
      value$mistake <- value$mistake + 1
    }
    # when user gets all scenarios wrong, then the user has to reset the game.
    if(value$mistake == 4){
      updateButton(session, "restart", disabled = TRUE)
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
  })
  #####################Counting Correct answers##############
  # user should get all answers correct in order to get full credt
  observeEvent(input$submit,{
    if(any(input$first == correct_answer[value$box1,1])&&
       any(input$second == correct_answer[value$box2,1])&&
       any(input$third == correct_answer[value$box3,1])&&
       any(input$fourth == correct_answer[value$box4,1]))
    {
      value$correct <- value$correct + 1
      updateButton(session, "restart", disabled = TRUE)
    }
    # when the user gets all wrong sub questions, then next button is disabled
    if(any(input$first != correct_answer[value$box1,1])&&
       any(input$second != correct_answer[value$box2,1])&&
       any(input$third != correct_answer[value$box3,1])&&
       any(input$fourth != correct_answer[value$box4,1]))
    {
      updateButton(session, "nextq", disabled = TRUE)
    }
    if(value$correct == 10){
      confirmSweetAlert(
        session = session,
        inputId = "end",
        title = "Well Done!",
        type = "success",
        text = p("You have completed this challenge! Thank you for saving
                 this poor little man!"),
        btn_labels = "End",
        btn_colors = "orange"
      )
      removeUI(selector='#submit', immediate=TRUE)
      removeUI(selector='#nextq', immediate=TRUE)
      removeUI(selector='#restart', immediate=TRUE)
      removeUI(selector='#reset', immediate=TRUE)
      autoDestroy=TRUE
    }
  })
  # Gets current page address from the current session
  getCurrentAddress <- function(session){
    return(paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      session$clientData$url_pathname, ":",
      session$clientData$url_port,
      session$clientData$url_search
    ))
  }
  observeEvent(input$submit,{
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index
    statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "answered"
        ),
        object = list(
          id = paste0(getCurrentAddress(session), "#", value$index),
          name = paste('Question', value$index, ":", bank[value$index*4,5]),
          description = paste(bank[value$box1,4], bank[value$box2,4],
                              bank[value$box3,4], bank[value$box4,4], sep =";")
        ),
        result = list(
          success = (any(input$first == correct_answer[value$box1,1])&&
                       any(input$second == correct_answer[value$box2,1])&&
                       any(input$third == correct_answer[value$box3,1])&&
                       any(input$fourth == correct_answer[value$box4,1])),
          completion = (any(input$first != 'Select Answer')&&
                          any(input$second != 'Select Answer')&&
                          any(input$third != 'Select Answer')&&
                          any(input$fourth != 'Select Answer')),
          response = paste(input$first, input$second, input$third,
                           input$fourth, correct_answer[value$box1,1],
                           correct_answer[value$box2,1],
                           correct_answer[value$box3,1],
                           correct_answer[value$box4,1], sep = ";"),
          duration = value$correct/10
          #the total questions number to reach win is 10, the duration is
          #the percentage of correct answers/total 10
        )
      )
    )
    # Store statement in locker and return status
    status <- rlocker::store(session, statement)
  })

  ##### Draw the Hangman Game#####
  # Daehoon: please convert these to renderImage; reference the marks for template
  output$scoreTree <- renderUI({
    ## Background
    if(value$mistake == 0){
      img(src = "Cell01.jpg",
          alt = "This is first tree which shows you are fine.",
          width = '100%')
    }
    ## Head
    else if(value$mistake == 1 ) {
      img(src = "Cell02.jpg",
          alt = "This is the first tree which shows you are fine.",
          width = '100%')
    }
    ## Arms
    else if(value$mistake == 2) {
      img(src = "Cell03.jpg",
          alt = "This is the second tree which shows you lose one life.",
          width = '100%')
    }
    ## Body
    else if(value$mistake == 3 ) {
      img(src = "Cell04.jpg",
          alt = "This is the third tree which shows you only have one life.",
          width = '100%')
    }
    ## Legs
    else if(value$mistake == 4) {
      img(src = "Cell05.jpg",
          alt = "This is the last tree which shows you are dead.",
          width = '100%')
      }
  })
})
