library(shiny)
library(png)
library(shinyDND)
library(shinyjs)
library(shinyBS)
library(V8)
library(shinyWidgets)

bank<- read.csv("easyQuestions.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)


shinyServer(function(input, output, session) {
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click buttons to see improved wordings.",
      type = "info"
    )
  })
  
  observeEvent(input$info1,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Drag pink rectangles with questions to the correct category box.",
      type = "info"
    )
  })
  #Reset Button For Main Page
  observeEvent(input$reset_button, {js$reset()}) 
  
  
  ###go button
  observeEvent(input$start,{
    updateTabItems(session,"tabs","overview")
  })
  
  ########Timer Info
  time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)
  
  observe({
    time$timer()
    if(isolate(time$started))
      time$inc<-isolate(time$inc)+1
  })
  
  observeEvent(input$go, {time$started<-TRUE})
  observeEvent(input$submitA, {time$started <- FALSE})
  observeEvent(input$next1, {time$started <- TRUE})
  observeEvent(input$submitB, {time$started <- FALSE})
  observeEvent(input$submitC, {time$timer<-reactiveTimer(Inf)})
  
  output$timer1 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer2 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer3 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer4 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  output$timer5 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  
  ######Back and Forth Buttons
  observeEvent(input$go,{
    updateNavbarPage(session = session,"navMain", selected = "b")
  })
  
  observeEvent(input$next1,{
    updateNavbarPage(session = session,"navMain", selected = "c")
  })
  
  observeEvent(input$next2,{
    updateNavbarPage(session = session,"navMain", selected = "d")
  })
  
  observeEvent(input$prev1,{
    updateNavbarPage(session = session,"navMain", selected = "b")
  })
  
  observeEvent(input$prev2,{
    updateNavbarPage(session = session,"navMain", selected = "a")
  })
  
  observeEvent(input$prev3,{
    updateNavbarPage(session = session,"navMain", selected = "c")
  })
  
  
  #Main Page
  observeEvent(input$runif,{
    output$text_example <- renderText({"Do you agree or disagree that it is hard for today's college graduates to have a bright future?" })
  })
  
  
  
  observeEvent(input$runif1,{
    output$text_example1<- renderText({"What is your opinion of our current President? a. favorable b. unfavorable c. undecided"})
  })
  
  observeEvent(input$runif2,{
    output$myImage <- renderImage({
      output$myImage <- renderImage({
        
        image_file <- paste("www/",input$image.type,".png",sep="")
        
        return(list(
          src = image_file,
          filetype = "importanceoforder.png",
          height = 250,
          width = 500
        ))
        
      }, deleteFile = FALSE)
      
      
      
    })
  })
  
  
  observeEvent(input$runif3,{
    output$text_example3<-renderText({"What is the population of Canada?"})
  })
  
  observeEvent(input$runif4, {
    output$text_example4<-renderText({"Do you favor or oppose an ordinance that does not allow 
      surveillance cameras to be placed on Beaver Avenue?"})
    })
  
  observeEvent(input$runif5, {
    output$text_example5<-renderText({"Who should have priority in receiving the smallpox vaccination? 
      a. health care workers 
      b. military personnel
      c. both health care workers and military personnel 
      d. neither"})
    })
  observeEvent(input$runif6,{
    output$text_example6<-renderText({"Do you agree or disagree that children who have a Body Mass Index (BMI) at or above the 95th percentile should spend less time watching television, playing computer games, and listening to music?"})
  })
  
  
  #Judge Correctness
  numbers <- reactiveValues( dis = c())
  
  observeEvent(input$go,{
    numbers$important = sample(2:6,1)
    numbers$filtering = sample(7:11,1)
    numbers$deliberate = sample(12:16,1)
    numbers$anchoring = sample(17:21,1)
    numbers$unintentional = sample(22:26,1)
    numbers$unnecessary = sample(27:31,1)
    numbers$nonbias = sample(32:35,1)
  })
  
  output$importantID1 <- renderText({
    bank[numbers$important[1], 2]
  })
  
  
  output$importantName1 <- renderText({
    bank[numbers$important[1],3]
  })
  
  output$unintentionalID1<- renderText({
    bank[numbers$unintentional[1],2]
  })
  
  output$unintentionalName1<- renderText({
    bank[numbers$unintentional[1],3]
  })
  
  output$unintentionalID2<- renderText({
    bank[numbers$unintentional[1],2]
  })
  
  output$unintentionalName2<- renderText({
    bank[numbers$unintentional[1],3]
  })
  
  output$filteringID1 <- renderText({
    bank[numbers$filtering[1], 2]
  })
  
  output$filteringName1 <- renderText({
    bank[numbers$filtering[1],3]
  })
  output$filteringID2 <- renderText({
    bank[numbers$filtering[1], 2]
  })
  
  output$filteringName2 <- renderText({
    bank[numbers$filtering[1],3]
  })
  
  
  output$anchoringID1 <- renderText({
    bank[numbers$anchoring[1], 2]
  })
  
  output$anchoringName1 <- renderText({
    bank[numbers$anchoring[1],3]
  })
  
  output$deliberateID1 <- renderText({
    bank[numbers$deliberate[1], 2]
  })
  
  output$deliberateName1 <- renderText({
    bank[numbers$deliberate[1],3]
  })
  
  output$unnecessaryID1 <- renderText({
    bank[numbers$unnecessary[1], 2]
  })
  
  output$unnecessaryName1 <- renderText({
    bank[numbers$unnecessary[1],3]
  })
  
  output$unnecessaryID2 <- renderText({
    bank[numbers$unnecessary[1], 2]
  })
  
  output$unnecessaryName2 <- renderText({
    bank[numbers$unnecessary[1],3]
  })
  
  output$deliberateID2 <- renderText({
    bank[numbers$deliberate[1], 2]
  })
  
  output$deliberateName2 <- renderText({
    bank[numbers$deliberate[1],3]
  })
  
  output$anchoringID2 <- renderText({
    bank[numbers$anchoring[1], 2]
  })
  
  output$anchoringName2 <- renderText({
    bank[numbers$anchoring[1],3]
  })
  
  output$nonbiasID1 <- renderText({
    bank[numbers$nonbias[1], 2]
  })
  
  output$nonbiasName1 <- renderText({
    bank[numbers$nonbias[1],3]
  })
  
  
  output$nonbiasID2 <- renderText({
    bank[numbers$nonbias[1], 2]
  })
  
  output$nonbiasNAME2 <- renderText({
    bank[numbers$nonbias[1],3]
  })
  
  observeEvent(input$submitA,{
    updateButton(session,"submitA",disabled = TRUE)
  })
  observeEvent(input$clear,{
    updateButton(session,"submitA",disabled = FALSE)
  })
  observeEvent(input$submitB,{
    updateButton(session,"submitB",disabled = TRUE)
  })
  observeEvent(input$clearB,{
    updateButton(session,"submitB",disabled = FALSE)
  })
  
  output$test<-renderPrint(input$drp1)
  output$test2 <- renderPrint({
    paste("\n                            ",bank[c(1:5),3],"\n                          ", sep = "")
  })
  
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer1 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer1 <- renderUI({
        if (!is.null(input$drp1)){
          if (any(input$drp1 == paste("\n                            ",bank[c(2:6),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer2 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer2 <- renderUI({
        if (!is.null(input$drp2)){
          if (any(input$drp2 == paste("\n                            ",bank[c(7:11),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer3 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer3 <- renderUI({
        if (!is.null(input$drp3)){
          if (any(input$drp3 == paste("\n                            ",bank[c(17:21),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer4 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer4 <- renderUI({
        if (!is.null(input$drp4)){
          if (any(input$drp4 == paste("\n                            ",bank[c(12:16),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer5 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer5 <- renderUI({
        if (!is.null(input$drp5)){
          if (any(input$drp5 == paste("\n                            ",bank[c(17:21),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer6 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer6 <- renderUI({
        if (!is.null(input$drp6)){
          if (any(input$drp6 == paste("\n                            ",bank[c(32:36),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer7 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer7 <- renderUI({
        if (!is.null(input$drp7)){
          if (any(input$drp7 == paste("\n                            ",bank[c(22:26),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer8 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer8 <- renderUI({
        if (!is.null(input$drp8)){
          if (any(input$drp8 == paste("\n                            ",bank[c(27:31),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  observeEvent(input$submitC,{  
    observeEvent(input$clearC,{
      output$answer9 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer9 <- renderUI({
        if (!is.null(input$drp9)){
          if (any(input$drp9 == paste("\n                            ",bank[c(32:36),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  observeEvent(input$submitC,{  
    observeEvent(input$clearC,{
      output$answer10 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer10 <- renderUI({
        if (!is.null(input$drp10)){
          if (any(input$drp10 == paste("\n                            ",bank[c(22:26),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  observeEvent(input$submitC,{  
    observeEvent(input$clearC,{
      output$answer11 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer11 <- renderUI({
        if (!is.null(input$drp11)){
          if (any(input$drp11 == paste("\n                            ",bank[c(27:31),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  observeEvent(input$submitC,{  
    observeEvent(input$clearC,{
      output$answer12 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer12 <- renderUI({
        if (!is.null(input$drp12)){
          if (any(input$drp12 == paste("\n                            ",bank[c(7:11),3],"\n                          ", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitC,{  
    observeEvent(input$clearC,{
      output$answer13 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer13 <- renderUI({
        if (!is.null(input$drp13)){
          if (any(input$drp13 == paste("haha ur wrong",bank[c(7:11),3],"lol", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitC,{  
    observeEvent(input$clearC,{
      output$answer14 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer14 <- renderUI({
        if (!is.null(input$drp14)){
          if (any(input$drp14 == paste("haha ur wrong",bank[c(7:11),3],"lol", sep = ""))){
            img(src = "check.png",width = 10)
          }else{
            img(src = "wrong.png",width = 10)
          }
        }
      })
    })
  })
  
  
  #########Scoring
  summation <- reactiveValues(summationA = c(rep(0,20)), summationB = c(rep(0,20)),summationScore = c(rep(0,20)))
  observeEvent(input$submitA,{
    score1 = c()
    score2 = c()
    score3 = c()
    
    for (i in c(input$drp2)){
      if (any(i == paste("\n                            ",bank[c(7:11),3],"\n                          ", sep = ""))){
        score1 = c(score1,10)
      }else{
        score1 = c(score1,-5)
      }
    }
    for (i in c(input$drp3)){
      if (any(i == paste("\n                            ",bank[c(17:21),3],"\n                          ", sep = ""))){
        score2 = c(score2, 10)}else{
          score2 = c(score2, -5)}
    }
    for (i in c(input$drp4)){
      if (any(i == paste("\n                            ",bank[c(12:16),3],"\n                          ", sep = ""))){
        score3 = c(score3, 10)}else{
          score3 = c(score3, -5)}
    }
    
    # summation$summationA <- c(summation$summationA, sum(c(score1,score2,score3))) 
    summation$summationA[input$submitA] <- sum(c(score1,score2,score3))
  })
  
  observeEvent(input$submitB,{
    score6 = c()
    score7 = c()
    score8 = c()
    
    
    for (i in c(input$drp6)){
      if (any(i == paste("\n                            ",bank[c(32:36),3],"\n                          ", sep = ""))){
        score6 = c(score6,10)
      }else{
        score6 = c(score6,-5)
      }
    }
    for (i in c(input$drp7)){
      if (any(i == paste("\n                            ",bank[c(22:26),3],"\n                          ", sep = ""))){
        score7 = c(score7, 10)}else{
          score7 = c(score7, -5)}
    }
    for (i in c(input$drp8)){
      if (any(i == paste("\n                            ",bank[c(27:31),3],"\n                          ", sep = ""))){
        score8 = c(score8, 10)}else{
          score8 = c(score8, -5)}
    }
    
    #summation$summationB <- c(summation$summationB, sum(score5)) 
    summation$summationB[input$submitB] <- sum(c(score6,score7,score8))
  })
  
  observeEvent(input$submitC,{
    score9 = c()
    score10 = c()
    score11 = c()
    score12 = c()
    score13 = c()
    score14 = c()
    
    for (i in c(input$drp9)){
      if (any(i == paste("\n                            ",bank[c(32:36),3],"\n                          ", sep = ""))){
        score9 = c(score9,10)
      }else{
        score9 = c(score9,-5)
      }
    }
    for (i in c(input$drp10)){
      if (any(i == paste("\n                            ",bank[c(22:26),3],"\n                          ", sep = ""))){
        score10 = c(score10, 10)}else{
          score10 = c(score10, -5)}
    }
    for (i in c(input$drp11)){
      if (any(i == paste("\n                            ",bank[c(27:31),3],"\n                          ", sep = ""))){
        score11 = c(score11, 10)}else{
          score11 = c(score11, -5)}
    }
    for (i in c(input$drp12)){
      if (any(i == paste("\n                            ",bank[c(7:11),3],"\n                          ", sep = ""))){
        score12 = c(score12, 10)}else{
          score12 = c(score12, -5)}
    }
    for (i in c(input$drp13)){
      if (any(i == paste("haha ur wrong",bank[c(7:11),3],"lol", sep = ""))){
        score13 = c(score13, 0)}else{
          score13 = c(score13, 0)}
    }
    for (i in c(input$drp14)){
      if (any(i == paste("haha ur wrong",bank[c(7:11),3],"lol", sep = "", sep = ""))){
        score14 = c(score14, 0)}else{
          score14 = c(score14, 0)}
    }
    
    # summation$summationC <- c(summation$summationC, sum(c(score9,score10,score11,score12,score13,score14)) 
    summation$summationC[input$submitC] <- sum(c(score9,score10,score11,score12,score13,score14))
  })
  
  values = reactiveValues(
    count = 0
  )
  observeEvent(input$submitA,{
    if(summation$summationA[input$submitA] == 30){
      updateButton(session, "next1",disabled = FALSE)
      #values$count = values$count + 30
    }
    
  })
  observeEvent(input$submitB,{
    if(summation$summationB[input$submitB] == 30){
      updateButton(session, "next2",disabled = FALSE)
      #values$count = values$count + 30
    }
  })
  observeEvent(input$submitC,{
    if(summation$summationC[input$submitC] == 40){
      updateButton(session, "finish",disabled = FALSE)
      #values$count = values$count + 30
    }
    else{
      updateButton(session, "finish", disabled = TRUE)}
    
  })
  
  
  output$scoreA <- renderPrint({
    cat("Score",summation$summationA[input$submitA])
  })
  
  output$scoreB <- renderPrint({
    cat("Score",summation$summationB[input$submitB])
  })
  
  output$scoreC <- renderPrint({
    cat("Score", summation$summationC[input$submitC])
  })
  
    })