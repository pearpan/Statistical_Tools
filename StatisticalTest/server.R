source('global.R')
shinyServer(function(input, output){
  
  
  
  #   observe({
  #     if(input$calculate == 0){ #check if this is first run
  #       return()   
  #     }
  #     isolate({
  #       # Read inputs and save values to database here
  #     })
  #   })
  #   
  #logo
  output$logo <- renderImage({
    filename <- 'www/Li_avatar.PNG'
    list(src = filename,
         width = 272,
         height = 153,
         alt = "Statistical Testing Lab")
    
  }, deleteFile = FALSE)
  
  
  output$traditional_test <- renderPrint({    
    input$calculate
    isolate({
      sig.thres <- as.numeric(input$significanceLevel)
      sig <- NULL
      n1 <- as.numeric(input$n1)
      n2 <- as.numeric(input$n2)
      value1 <- as.numeric(input$value1)
      value2 <- as.numeric(input$value2)
      pwr <- as.numeric(input$samplePower)
      test_type <- input$TestType
      calcVal <- input$valueToCalculate
      
      if(calcVal=='sample size'){
        n1 <- NULL
        n2 <- NULL
        sig <- sig.thres
      }else if(calcVal =='power'){
        pwr <- NULL
        sig <- sig.thres
      }
      
      if(test_type=='two sample proportion test'){
        h<- ES.h(value1,value2)
        if(h==0){
          results<-NULL
          msg<-paste('No difference between test and control group at all!')
          
        }else if(calcVal =='significant level(two-sided)'){
          results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = pwr)   
          if(results$sig.level<sig.thres){
            if(value1>value2){
              msg='Test group has significanly higher conversion rate!'
            }else{
              msg='Test group has significanly lower conversion rate!'
            }
            
          }else{
            sample.needed <- ceiling(pwr.2p.test(h=h,power=pwr,sig.level=sig.thres)$n)
            msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group!')
          }
          
        }else if(calcVal =='significant level(one-sided)'){
          if(h>0){
            results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = pwr,alternative = 'greater')
            if(results$sig.level<sig.thres){
              msg='Test group has significantly higher conversion rate'
            }else{
              sample.needed <- ceiling(pwr.2p.test(h=h,power=pwr,sig.level=sig.thres,alternative = 'greater')$n)
              msg=paste('There is no siginificant increase in test group.\n Require',sample.needed,'samples in both test and control group!')
            }
          }else{
            results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = pwr,alternative = 'less')
            if(results$sig.level<sig.thres){
              msg='Test group has significantly lower conversion rate'
            }else{
              sample.needed <- ceiling(pwr.2p.test(h=h,power=pwr,sig.level=sig.thres,alternative = 'less')$n)
              msg=paste('There is no siginificant decrease in test group. \n Require',sample.needed,'samples in both test and control group!')
            }
          }      
        }else if(calcVal =='power'){
          results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = pwr)
          msg=paste('The power is',results$power)
        }else if(calcVal =='sample size'){
          results <- pwr.2p.test(h =h , sig.level = sig, power = pwr)
          msg=paste('Sample size required for both test and control group is',ceiling(results$n))
        }
        cat(msg)
        cat('\n\n')
        cat('Detals:\n')
        results
      }else if(test_type=='two sample t-test'){
        cat('Sorry, this test is not working now. Li is developing it')
      }
      
    })
    
  })
  
  
  output$win_prob <- renderPrint({
    input$calculate
    isolate({
      calcVal <- input$valueToCalculate
      test_type <- input$TestType
      if(test_type=='two sample proportion test'){
        if(calcVal=='significant level(two-sided)'|calcVal=='significant level(one-sided)'){
          sig <- as.numeric(input$significanceLevel)
          n1 <- as.numeric(input$n1)
          n2 <- as.numeric(input$n2)
          k1 <- round(as.numeric(input$value1)*n1)
          k2 <- round(as.numeric(input$value2)*n2)
          betaPost = sim_post(c(k1,k2),c(n1,n2))
          win.prob<-round(prob_winner(betaPost),2)
          #win.prob <- round(win_prob_test(c(k1,k2),c(n1,n2))$p_best,3)
          names(win.prob)=c('test','control')
          if(win.prob[1]>1-sig){
            cat('Test group is the winner!')
          }else if(win.prob[2]>1-sig){
            cat('Control group is the winner!')
          }else{
            cat('No winner under selected significant level.')
          }
          cat('\n\n')
          win.prob   
        }
      }else{
        cat('Sorry, this test is not working now. Li is developing it')
      }
      
    })
    
    
  })
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    if(input$TestType=='two sample proportion test'){
      return()
    }else{
      inFile <- input$testfile
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)[1:20,]
    }
    
  })
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    if(input$TestType=='two sample proportion test'){
      return()
    }else{
      inFile <- input$controlfile
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)[1:20,]
    }
    
  })
})