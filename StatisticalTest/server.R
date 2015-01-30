source('global.R')
shinyServer(function(input, output){
  
  #logo
  output$logo <- renderImage({
    filename <- 'www/Li_avatar.PNG'
    list(src = filename,
         width = 272,
         height = 153,
         alt = "Statistical Testing Lab")
    
  }, deleteFile = FALSE)
  
  #traditional statistical test
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
        cat('Details:\n')
        results
      }else if(test_type=='two sample t-test'){
        inFile <- input$testfile
        if (is.null(inFile)){
          testdata <- read.csv('data/test_data.csv',header=TRUE)
        }else{
          testdata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
        }
        if(ncol(testdata)>1){
          cat('Input test group data has more than one column!\n')
          cat('Please load a dataset with only 1 column/variable. \n')
          return
        }
        inFile <- input$controlfile
        if (is.null(inFile)){
          controldata <- read.csv('data/control_data.csv',header=TRUE)
        }else{
          controldata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
        }
        if(ncol(controldata)>1){
          cat('Input control group data has more than one column!\n')
          cat('Please load a dataset with only 1 column/variable. \n')
          return
        }
        x <- testdata[,1]
        y <- controldata[,1]
        x <- x[x>=quantile(x,input$trim_lower) & x<=quantile(x,1-input$trim_upper)]
        y <- y[y>=quantile(y,input$trim_lower) & y<=quantile(y,1-input$trim_upper)]
        n1 <- length(x)
        n2 <- length(y)
        d <- (mean(x)-mean(y))/sd(c(x,y))
        if(d==0){
          results<-NULL
          msg<-paste('No difference between test and control group at all!')
          
        }else if(calcVal =='significant level(two-sided)'){
          results<-pwr.t2n.test(n1 = n1,n2 = n2,d = d,sig.level=sig, power=pwr)
          if(results$sig.level<sig.thres){
            if(d > 0){
              msg='Test group has significanly higher average value!'
            }else{
              msg='Test group has significanly lower average value!'
            }            
          }else{
            sample.needed <- ceiling(pwr.t.test(d=d,power=pwr,sig.level=sig.thres)$n)
            msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group!')
          }
        }else if(calcVal =='significant level(one-sided)'){
          if(d>0){
            results <- pwr.t2n.test(d = d , n1 = n1 , n2 = n2, sig.level = sig, power = pwr,alternative = 'greater')
            if(results$sig.level<sig.thres){
              msg='Test group has significantly higher average value'
            }else{
              sample.needed <- ceiling(pwr.t.test(d=d,power=pwr,sig.level=sig.thres,alternative = 'greater')$n)
              msg=paste('There is no siginificant increase in test group.\n Require',sample.needed,'samples in both test and control group!')
            }
          }else{
            results <- pwr.t2n.test(d = d , n1 = n1 , n2 = n2, sig.level = sig, power = pwr,alternative = 'less')
            if(results$sig.level<sig.thres){
              msg='Test group has significantly lower average value'
            }else{
              sample.needed <- ceiling(pwr.t.test(d = d,power=pwr,sig.level=sig.thres,alternative = 'less')$n)
              msg=paste('There is no siginificant decrease in test group. \n Require',sample.needed,'samples in both test and control group!')
            }
          }      
        }else if(calcVal =='power'){
          results <- pwr.t2n.test(d = d , n1 =n1 , n2=n2, sig.level = sig, power = pwr)
          msg=paste('The power is',results$power)
        }else if(calcVal =='sample size'){
          results <- pwr.t.test(d = d , sig.level = sig, power = pwr)
          msg=paste('Sample size required for both test and control group is',ceiling(results$n))
        }
        cat(msg)
        cat('\n\n')
        if(input$trim_upper>0 | input$trim_lower){
          cat('Note: Based on your selection, \n')
          if(input$trim_upper>0){
            cat('top',input$trim_upper*100,'% data is removed,\n')
          }
          if(input$trim_lower>0){
            cat('bottom',input$trim_lower*100,'% data is removed.\n')
          } 
        }
        
        cat('\n')
        cat('details:')
        print(results)
        cat('test group data statistics:\n')
        print(summary(x))
        cat('control group data statistics:\n')
        print(summary(y))        
      }
      
    })
    
  })#end of traditional statistical test
  
  #winning prob
  output$win_prob <- renderPrint({
    input$calculate
    isolate({
      calcVal <- input$valueToCalculate
      test_type <- input$TestType
      if(test_type=='two sample proportion test'){
        if(calcVal=='significant level(two-sided)'|calcVal=='significant level(one-sided)'){
          n1 <- as.numeric(input$n1)
          n2 <- as.numeric(input$n2)
          k1 <- round(as.numeric(input$value1)*n1)
          k2 <- round(as.numeric(input$value2)*n2)
          betaPost = sim_post(c(k1,k2),c(n1,n2))
          win.prob<-round(prob_winner(betaPost),2)
          #win.prob <- round(win_prob_test(c(k1,k2),c(n1,n2))$p_best,3) 
          names(win.prob)=c('test','control')
          sig <- as.numeric(input$significanceLevel)
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
      }else if(test_type=='two sample t-test' & !calcVal %in% c('power','sample size') ){
        inFile <- input$testfile
        if (is.null(inFile)){
          testdata <- read.csv('data/test_data.csv',header=TRUE)
        }else{
          testdata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
        }
        if(ncol(testdata)>1){
          return
        }
        inFile <- input$controlfile
        if (is.null(inFile)){
          controldata <- read.csv('data/control_data.csv',header=TRUE)
        }else{
          controldata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
        }
        if(ncol(controldata)>1){
          return
        }
        x <- testdata[,1]
        y <- controldata[,1]
        x <- x[x>=quantile(x,input$trim_lower) & x<=quantile(x,1-input$trim_upper)]
        y <- y[y>=quantile(y,input$trim_lower) & y<=quantile(y,1-input$trim_upper)]
        result<-sapply(1:10000,function(z){
          xsample <- sample(x,length(x),replace = TRUE)
          ysample <- sample(y,length(y),replace = TRUE)
          win.x <- mean(xsample)>mean(ysample)
          win.y <- mean(xsample)<mean(ysample)
          c(win.x,win.y)
        })
        win.prob <- round(rowSums(result)/10000,2)   
        names(win.prob)=c('test','control')
        sig <- as.numeric(input$significanceLevel)
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
      
      
    })#end of isolate
    
    
  })#end of winning prob
  
  #data preview_test
  output$testcontents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    if(input$TestType=='two sample proportion test'){
      return()
    }else{
      inFile <- input$testfile
      if (is.null(inFile)){
        head(testdata <- read.csv('data/test_data.csv',header=TRUE))
      }else{
        head(testdata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote))
      }
        
      
    }
    
  })
  #data preview_control
  output$controlcontents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    if(input$TestType=='two sample proportion test'){
      return()
    }else{
      inFile <- input$controlfile
      
      if (is.null(inFile)){
        head(controldata<-read.csv('data/control_data.csv',header=TRUE))
      }else{
        head(controldata<-read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote))
      }      
    }
    
  })
})