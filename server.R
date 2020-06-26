library(boastUtils)
#Let`s begin
bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)
shinyServer(function(session, input, output) {
    observeEvent(input$nextbutton, {
    updateTabItems(session, "tabs", "prerequisite")
    })
  
  observeEvent(input$link_to_preq, {
    updateTabItems(session, "tabs", "prerequisite")
  })
  
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "Hangman")
  })
  
  observeEvent(input$end, {
    updateTabItems(session, "tabs", "References")
  })
  
  observeEvent(input$submit, {
    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session, "submit", disabled = TRUE)
    updateButton(session, "restart", disabled = FALSE)
  })
  
  observeEvent(input$nextq, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "restart", disabled = TRUE)
    updateSelectInput(session, "first","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "second","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "third","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "fourth","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
  })
  #### Restart button
  observeEvent(input$restart,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "restart",disable =TRUE)
    updateSelectInput(session, "first","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "second","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "third","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "fourth","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    output$mark1 <- renderUI({
      img(src = NULL,width = 20)
    })
    output$mark2 <- renderUI({
      img(src = NULL,width = 20)
    })
    output$mark3 <- renderUI({
      img(src = NULL,width = 20)
    })
    output$mark4 <- renderUI({
      img(src = NULL,width = 20)
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

    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "restart",disable =TRUE)
    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "reset", disabled = TRUE)
    # removeUI(selector='#reset', immediate=TRUE)
    # insertUI(selector='#nextq',immediate = TRUE)
    # autoDestroy=TRUE

    updateSelectInput(session, "first","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "second","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "third","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "fourth","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))

    output$mark1 <- renderUI({
      img(src = NULL,width = 30)
    })
    output$mark2 <- renderUI({
      img(src = NULL,width = 30)
    })
    output$mark3 <- renderUI({
      img(src = NULL,width = 30)
    })
    output$mark4 <- renderUI({
      img(src = NULL,width = 30)
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
    value$index <- 15 #15th question on question bank
    correct_answer <- as.matrix(bank[1:60,1])
    value$box1= 4*value$index-3
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
    output$mark1 <- renderUI({
      img(src = NULL,width = 30)
    })
    output$mark2 <- renderUI({
      img(src = NULL,width = 30)
    })
    output$mark3 <- renderUI({
      img(src = NULL,width = 30)
    })
    output$mark4 <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  # UI for score
  output$correct <- renderUI({
    p("Number of completed scenarios:" ,"", value$correct, " out of 10")
  })
  observeEvent(input$submit,{
    output$correct <- renderUI({
      p("Number of completed scenarios: " ,"", value$correct, " out of 10" )
    })
  })
  #### Questions
  output$question <- renderUI({
    if(value$index == 1){ # ex) this is the question for Q1 on question bank.
      p(bank[1,5])
    }
    else if(value$index == 2){
      p(bank[5,5])
    }
    else if(value$index == 3){
      p(bank[9,5])
    }
    else if(value$index == 4){
      p(bank[13,5])
    }
    else if(value$index == 5){
      p(bank[17,5])
    }
    else if(value$index == 6){
      p(bank[21,5])
    }
    else if(value$index == 7){
      p(bank[25,5])
    }
    else if(value$index == 8){
      p(bank[29,5])
    }
    else if(value$index == 9){
      p(bank[33,5])
    }
    else if(value$index == 10){
      p(bank[37,5])
    }
    else if(value$index == 11){
      p(bank[41,5])
    }
    else if(value$index == 12){
      p(bank[45,5])
    }
    else if(value$index == 13){
      p(bank[49,5])
    }
    else if(value$index == 14){
      p(bank[53,5])
    }
    else if(value$index == 15){
      p(bank[57,5])
    }
  })
  #### Questions - first number on each scenario
  output$box1 <- renderUI({
    if(value$index ==1){
      p(bank[1,4])
    }
    else if(value$index ==2){
      p(bank[5,4])
    }
    else if(value$index == 3){
      p(bank[9,4])
    }
    else if(value$index == 4){
      p(bank[13,4])
    }
    else if(value$index == 5){
      p(bank[17,4])
    }
    else if(value$index == 6){
      p(bank[21,4])
    }
    else if(value$index == 7){
      p(bank[25,4])
    }
    else if(value$index == 8){
      p(bank[29,4])
    }
    else if(value$index == 9){
      p(bank[33,4])
    }
    else if(value$index == 10){
      p(bank[37,4])
    }
    else if(value$index == 11){
      p(bank[41,4])
    }
    else if(value$index == 12){
      p(bank[45,4])
    }
    else if(value$index == 13){
      p(bank[49,4])
    }
    else if(value$index == 14){
      p(bank[53,4])
    }
    else if(value$index == 15){
      p(bank[57,4])
    }
  })
  #### Questions - second number on each scenario
  output$box2 <- renderUI({
    if(value$index ==1){
      p(bank[2,4])
    }
    else if(value$index ==2){
      p(bank[6,4])
    }
    else if(value$index == 3){
      p(bank[10,4])
    }
    else if(value$index == 4){
      p(bank[14,4])
    }
    else if(value$index == 5){
      p(bank[18,4])
    }
    else if(value$index == 6){
      p(bank[22,4])
    }
    else if(value$index == 7){
      p(bank[26,4])
    }
    else if(value$index == 8){
      p(bank[30,4])
    }
    else if(value$index == 9){
      p(bank[34,4])
    }
    else if(value$index == 10){
      p(bank[38,4])
    }
    else if(value$index == 11){
      p(bank[42,4])
    }
    else if(value$index == 12){
      p(bank[46,4])
    }
    else if(value$index == 13){
      p(bank[50,4])
    }
    else if(value$index == 14){
      p(bank[54,4])
    }
    else if(value$index == 15){
      p(bank[58,4])
    }
  })
  #### Questions - third number on each scenario
  output$box3 <- renderUI({
    if(value$index == 1){
      p(bank[3,4])
    }
    else if(value$index == 2){
      p(bank[7,4])
    }
    else if(value$index == 3){
      p(bank[11,4])
    }
    else if(value$index == 4){
      p(bank[15,4])
    }
    else if(value$index == 5){
      p(bank[19,4])
    }
    else if(value$index == 6){
      p(bank[23,4])
    }
    else if(value$index == 7){
      p(bank[27,4])
    }
    else if(value$index == 8){
      p(bank[31,4])
    }
    else if(value$index == 9){
      p(bank[35,4])
    }
    else if(value$index == 10){
      p(bank[39,4])
    }
    else if(value$index == 11){
      p(bank[43,4])
    }
    else if(value$index == 12){
      p(bank[47,4])
    }
    else if(value$index == 13){
      p(bank[51,4])
    }
    else if(value$index == 14){
      p(bank[55,4])
    }
    else if(value$index == 15){
      p(bank[59,4])
    }
  })
  #### Questions - fourth number on each scenario
  output$box4 <- renderUI({
    if(value$index == 1){
      p(bank[4,4])
    }
    else if(value$index == 2){
      p(bank[8,4])
    }
    else if(value$index == 3){
      p(bank[12,4])
    }
    else if(value$index == 4){
      p(bank[16,4])
    }
    else if(value$index == 5){
      p(bank[20,4])
    }
    else if(value$index == 6){
      p(bank[24,4])
    }
    else if(value$index == 7){
      p(bank[28,4])
    }
    else if(value$index == 8){
      p(bank[32,4])
    }
    else if(value$index == 9){
      p(bank[36,4])
    }
    else if(value$index == 10){
      p(bank[40,4])
    }
    else if(value$index == 11){
      p(bank[44,4])
    }
    else if(value$index == 12){
      p(bank[48,4])
    }
    else if(value$index == 13){
      p(bank[52,4])
    }
    else if(value$index == 14){
      p(bank[56,4])
    }
    else if(value$index == 15){
      p(bank[60,4])
    }
  })
  ## check marks ##
  observeEvent(input$submit,{ 
    output$mark1 <- renderUI({
      if (any(input$first == correct_answer[value$box1,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
      }
    })
    output$mark2 <- renderUI({
      if (any(input$second == correct_answer[value$box2,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
      }
     })
    output$mark3 <- renderUI({
      if (any(input$third == correct_answer[value$box3,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
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
        text = p("You have completed this challenge! Thank you for saving this poor little man!"),
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
          description = paste(bank[value$box1,4], bank[value$box2,4], bank[value$box3,4], bank[value$box4,4], sep =";")
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
          response = paste(input$first, input$second, input$third, input$fourth, correct_answer[value$box1,1], correct_answer[value$box2,1], correct_answer[value$box3,1],correct_answer[value$box4,1], sep = ";"),
          duration = value$correct/10
          #the total questions number to reach win is 10, the duration is the percentage of correct answers/total 10
        )
      )
    )
    # Store statement in locker and return status
    status <- rlocker::store(session, statement)
  })
  
  ##### Draw the Hangman Game#####
  output$scoreTree <- renderUI({
    ## Background
    if(value$mistake == 0){
      img(src = "Cell01.jpg", height = '530px', width = '100%')
    }
    ## Head
    else if(value$mistake == 1 ) {
      img(src = "Cell02.jpg", height = '530px', width = '100%')
    }
    ## Arms
    else if(value$mistake == 2) {
      img(src = "Cell03.jpg", height = '530px', width = '100%')
    }
    ## Body
    else if(value$mistake == 3 ) {
      img(src = "Cell04.jpg", height = '530px', width = '100%')
    }
    ## Legs
    else if(value$mistake == 4) {
      img(src = "Cell05.jpg", height = '530px', width = '100%')
      }
  })
})
