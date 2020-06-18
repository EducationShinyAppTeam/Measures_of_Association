library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(plotrix)
library(shinyWidgets)
library(rlocker)

bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)


shinyServer(function(session, input, output) {
  
  #Initialized learning  locker connection
  connection <- rlocker::connect(session, list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rlocker::createAgent()
  ))
  
  # Setup demo app and user.
  currentUser <- 
    connection$agent
  
  if(connection$status != 200){
  }
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Choose the Measure of Association in the dropdown menu with the number in the context described.",
      type = "info"
    )
  })
  observeEvent(input$nextbutton, {
    
    updateTabItems(session, "tabs", "prerequisite")
    
  })
  
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "Hangman")
  })
  
  observeEvent(input$submit, {
    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session, "submit", disabled = TRUE)
  })
  
  
  observeEvent(input$nextq, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = TRUE)
    
    updateSelectInput(session, "first","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "second","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "third","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "fourth","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    
    #output$result <- renderUI({
    #  h3("Choose different measure of associations for each numeric value, then click 'Submit' to check your answer")
    #})
    
  })
  
  observeEvent(input$restart,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"restart",disable =TRUE)
    #output$result <- renderUI({
    #  h3("Choose different measure of associations for each numeric value, then click 'Submit' to check your answer")
    #})
    updateSelectInput(session, "first","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "second","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "third","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    updateSelectInput(session, "fourth","",c('Select Answer','Relative Risk', 'Risk','Odds', 'Odds Ratio', "Probability"))
    
    index_list$list<-c(index_list$list,sample(2:15,14,replace=FALSE))
    value$index <- 1
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index
    correct_answer <- as.matrix(bank[1:60,1])
    
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
    
    value$correct <- 0;
  })
  
  output$box1 <- renderUI({
    uiOutput('box1')
  })
  output$box2 <- renderUI({
    
    uiOutput('box2')
  })
  
  output$box3 <- renderUI({
    
    uiOutput('box3')
  })
  
  output$box4 <- renderUI({
    
    uiOutput('box4')  
  })
  #output$result <- renderUI({
   # h3("Choose different measure of associations for each numeric value, then click 'Submit' to check your answer")
  #})
  
  
  value <- reactiveValues(index =  15, mistake = 0, correct = 0)
  correct_answer <- as.matrix(bank[1:60,1])
  
  index_list<-reactiveValues(list=sample(1:14,14,replace=FALSE))
  
  
  observeEvent(input$reset,{
    index_list$list<-c(index_list$list,sample(2:15,14,replace=FALSE))
    value$index <- 1
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index
    correct_answer <- as.matrix(bank[1:60,1])
    
    updateButton(session,"submit",disabled = FALSE)
    updateButton(session,"reset", disabled = TRUE)
    
    #output$result <- renderUI({
    #  h3("Choose different measure of associations for each numeric value, then click 'Submit' to check your answer")
    #})
    
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
  })
  
  observeEvent(input$go,{
    value$index <- 15
    correct_answer <- as.matrix(bank[1:60,1])
    
    value$box1= 4*value$index-3
    value$box2= 4*value$index-2
    value$box3= 4*value$index-1
    value$box4= 4*value$index
    
  })
  
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
      #output$result <- renderUI({
      #  h3("Please click RELOAD to reload questions from database so you can continue this game")
      #})
    }
    
  })
  output$correct <- renderUI({
    
    h3("Number of correct answers:" ,"", value$correct )
  })
  
  observeEvent(input$submit,{
    output$correct <- renderUI({
      
      h3("Number of correct answers:" ,"", value$correct )
    })
    
  })
  
  observeEvent(input$nextq,{
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
  
  
  
  output$question <- renderUI({
    if(value$index == 1){
      h3(bank[1,5])
    }
    else if(value$index == 2){
      h3(bank[5,5])
    }
    else if(value$index == 3){
      h3(bank[9,5])
    }
    else if(value$index == 4){
      h3(bank[13,5])
    }
    else if(value$index == 5){
      h3(bank[17,5])
    }
    else if(value$index == 6){
      h3(bank[21,5])
    }
    else if(value$index == 7){
      h3(bank[25,5])
    }
    else if(value$index == 8){
      h3(bank[29,5])
    }
    else if(value$index == 9){
      h3(bank[33,5])
    }
    else if(value$index == 10){
      h3(bank[37,5])
    }
    else if(value$index == 11){
      h3(bank[41,5])
    }
    else if(value$index == 12){
      h3(bank[45,5])
    }
    else if(value$index == 13){
      h3(bank[49,5])
    }
    else if(value$index == 14){
      h3(bank[53,5])
    }
    else if(value$index == 15){
      h3(bank[57,5])
    }
    
  })
  
  output$box1 <- renderUI({
    if(value$index ==1){
      h3(bank[1,4])
    }
    else if(value$index ==2){
      h3(bank[5,4])
    }
    else if(value$index == 3){
      h3(bank[9,4])
    }
    else if(value$index == 4){
      h3(bank[13,4])
    }
    else if(value$index == 5){
      h3(bank[17,4])
    }
    else if(value$index == 6){
      h3(bank[21,4])
    }
    else if(value$index == 7){
      h3(bank[25,4])
    }
    else if(value$index == 8){
      h3(bank[29,4])
    }
    else if(value$index == 9){
      h3(bank[33,4])
    }
    else if(value$index == 10){
      h3(bank[37,4])
    }
    else if(value$index == 11){
      h3(bank[41,4])
    }
    else if(value$index == 12){
      h3(bank[45,4])
    }
    else if(value$index == 13){
      h3(bank[49,4])
    }
    else if(value$index == 14){
      h3(bank[53,4])
    }
    else if(value$index == 15){
      h3(bank[57,4])
    }
    
  })
  
  output$box2 <- renderUI({
    if(value$index ==1){
      h3(bank[2,4])
    }
    else if(value$index ==2){
      h3(bank[6,4])
    }
    else if(value$index == 3){
      h3(bank[10,4])
    }
    else if(value$index == 4){
      h3(bank[14,4])
    }
    else if(value$index == 5){
      h3(bank[18,4])
    }
    else if(value$index == 6){
      h3(bank[22,4])
    }
    else if(value$index == 7){
      h3(bank[26,4])
    }
    else if(value$index == 8){
      h3(bank[30,4])
    }
    else if(value$index == 9){
      h3(bank[34,4])
    }
    else if(value$index == 10){
      h3(bank[38,4])
    }
    else if(value$index == 11){
      h3(bank[42,4])
    }
    else if(value$index == 12){
      h3(bank[46,4])
    }
    else if(value$index == 13){
      h3(bank[50,4])
    }
    else if(value$index == 14){
      h3(bank[54,4])
    }
    else if(value$index == 15){
      h3(bank[58,4])
    }
    
    
  })
  output$box3 <- renderUI({
    if(value$index == 1){
      h3(bank[3,4])
    }
    else if(value$index == 2){
      h3(bank[7,4])
    }
    else if(value$index == 3){
      h3(bank[11,4])
    }
    else if(value$index == 4){
      h3(bank[15,4])
    }
    else if(value$index == 5){
      h3(bank[19,4])
    }
    else if(value$index == 6){
      h3(bank[23,4])
    }
    else if(value$index == 7){
      h3(bank[27,4])
    }
    else if(value$index == 8){
      h3(bank[31,4])
    }
    else if(value$index == 9){
      h3(bank[35,4])
    }
    else if(value$index == 10){
      h3(bank[39,4])
    }
    else if(value$index == 11){
      h3(bank[43,4])
    }
    else if(value$index == 12){
      h3(bank[47,4])
    }
    else if(value$index == 13){
      h3(bank[51,4])
    }
    else if(value$index == 14){
      h3(bank[55,4])
    }
    else if(value$index == 15){
      h3(bank[59,4])
    }
  })
  output$box4 <- renderUI({
    if(value$index == 1){
      h3(bank[4,4])
    }
    else if(value$index == 2){
      h3(bank[8,4])
    }
    else if(value$index == 3){
      h3(bank[12,4])
    }
    else if(value$index == 4){
      h3(bank[16,4])
    }
    else if(value$index == 5){
      h3(bank[20,4])
    }
    else if(value$index == 6){
      h3(bank[24,4])
    }
    else if(value$index == 7){
      h3(bank[28,4])
    }
    else if(value$index == 8){
      h3(bank[32,4])
    }
    else if(value$index == 9){
      h3(bank[36,4])
    }
    else if(value$index == 10){
      h3(bank[40,4])
    }
    else if(value$index == 11){
      h3(bank[44,4])
    }
    else if(value$index == 12){
      h3(bank[48,4])
    }
    else if(value$index == 13){
      h3(bank[52,4])
    }
    else if(value$index == 14){
      h3(bank[56,4])
    }
    else if(value$index == 15){
      h3(bank[60,4])
    }
  })
  
  
 
  
  observeEvent(input$submit,{ 
    
    output$mark1 <- renderUI({
      
      if (any(input$first == correct_answer[value$box1,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
        
      }
      
    })
    
  })
  
  observeEvent(input$submit,{ 
    
    output$mark2 <- renderUI({
      if (any(input$second == correct_answer[value$box2,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
        
      }
      
    })
  })
  
  observeEvent(input$submit,{ 
    
    output$mark3 <- renderUI({
      if (any(input$third == correct_answer[value$box3,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
        
        
      }
    })
  })
  
  observeEvent(input$submit,{ 
    
    output$mark4 <- renderUI({
      if (any(input$fourth == correct_answer[value$box4,1])){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
        
        
      }
    })
  })
  
  
  ################Counting Mistakes###############  
  observeEvent(input$submit,{
    if(any(input$first != correct_answer[value$box1,1])||
       any(input$second != correct_answer[value$box2,1])||
       any(input$third != correct_answer[value$box3,1])||
       any(input$fourth != correct_answer[value$box4,1]))
    {
      value$mistake <- value$mistake + 1
      
      #output$result <- renderUI({
      #  h3("Sorry, you have at least one wrong answer. Click 'Next Question' to move on your challenge")
      #})
    }
    
    
  })
  
  observeEvent(input$nextq,{
    if(value[["mistake"]] == 4){
      
      updateButton(session, "nextq", disabled = TRUE)
      
      value[["mistake"]] <- 0
      value$correct <- 0
      #output$result <- renderUI({
      #  h3("Sorry, you have lost the Game. You need to start this game from the beginning")
        
      #})
    }
  })
  
  
  #####################Counting Correct answers##############
  observeEvent(input$submit,{
    if(any(input$first == correct_answer[value$box1,1])&&
       any(input$second == correct_answer[value$box2,1])&&
       any(input$third == correct_answer[value$box3,1])&&
       any(input$fourth == correct_answer[value$box4,1]))
    {
      value$correct <- value$correct + 1
      
      #output$result <- renderUI({
      #  h3("Congratulation! You got this one correct. Click 'Next Question' to move on your challenge")
      #})
    }
    
    if(value$correct == 10){
      #output$result <- renderUI({
      #  h3("Well Done! You have completed this challenge!  You saved that poor little man!")
      #})
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "restart", disabled = FALSE)
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
    
    #print(statement) # remove me
    #print(status) # remove me
  })
  
  ##### Draw the Hangman Game#####
  
  output$distPlot <- renderUI({
    
    ## Background
    if(value$mistake == 0){
      img(src = "Cell01.jpg", width = 500)
      
    }
    
    ## Head
    else if(value$mistake == 1 ) {
      img(src = "Cell02.jpg", width = 500)
    }
    
    ## Arms
    else if(value$mistake == 2) {
      img(src = "Cell03.jpg", width = 500)
    }
    
    ## Body
    else if(value$mistake == 3 ) {
      img(src = "Cell04.jpg", width = 500)
    }
    
    
    ## Legs
    else if(value$mistake == 4) {
      img(src = "Cell05.jpg", width = 500)
    }
    
  })
  
  
  
})



