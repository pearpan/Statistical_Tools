source('global.R')
options(shiny.maxRequestSize=100*1024^2)
shinyServer(function(input, output){
  
  #logo
  output$logo <- renderImage({
    filename <- 'www/lottery_girls.jpg'
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
      p1 <- as.numeric(input$p1)
      p2 <- as.numeric(input$p2)
      ES <- as.numeric(input$ES)
      pwr <- as.numeric(input$samplePower)
      test_type <- input$TestType
      calcVal <- input$valueToCalculate
      power_test <- input$PwrTestType
      
      if(calcVal=='sample size'){
        n1 <- NULL
        n2 <- NULL
        sig <- sig.thres
        #}else if(calcVal =='power'){
        #  pwr <- NULL
        #  sig <- sig.thres
      }
      
      if(test_type=='two sample proportion test'){
        h<- ES.h(value1,value2)
        if(h==0){
          results<-NULL
          msg<-paste('No difference between test and control group at all!')
          
        }else if(calcVal =='significant level(two-sided)'){
          #results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = 0)#for sig test
          results <- prop.test(x=c(round(value1*n1),round(value2*n2)),n=c(n1,n2),conf.level=1-sig.thres,alternative="two.sided")
          if(results$p.value<sig.thres){
            if(value1>value2){
              lift <- round((value1-value2)/value2*100,1)
              msg=paste('Test group has significantly higher conversion rate! \n The lift is',lift,'%\n P-value is',round(results$p.value,3))
            }else{
              drop <- round((value2-value1)/value2*100,1)
              msg=paste('Test group has siginificantly lower conversion rate! \n The drop is',drop,'%\n P-value is',round(results$p.value,3))              
            }          
          }else{
            sample.needed <- ceiling(pwr.2p.test(h=h,power=pwr,sig.level=sig.thres)$n)
            msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group with', pwr*100,'% power!')
          }
          
        }else if(calcVal =='significant level(one-sided)'){
          if(h>0){
            #results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = 0,alternative = 'greater')
            results <- prop.test(x=c(round(value1*n1),round(value2*n2)),n=c(n1,n2),conf.level=1-sig.thres,alternative="greater")
            if(results$p.value<sig.thres){
              lift <- round((value1-value2)/value2*100,1)
              msg=paste('Test group has significantly higher conversion rate! \n The lift is',lift,'%\n P-value is',round(results$p.value,3))
            }else{
              sample.needed <- ceiling(pwr.2p.test(h=h,power=pwr,sig.level=sig.thres,alternative = 'greater')$n)
              msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group with', pwr*100,'% power!')
            }
          }else{
            #results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = 0,alternative = 'less')
            results <- prop.test(x=c(round(value1*n1),round(value2*n2)),n=c(n1,n2),conf.level=1-sig.thres,alternative="less")
            if(results$p.value<sig.thres){
              drop <- round((value2-value1)/value2*100,1)
              msg=paste('Test group has siginificantly lower conversion rate! \n The drop is',drop,'%\n P-value is',round(results$p.value,3))              
            }else{
              sample.needed <- ceiling(pwr.2p.test(h=h,power=pwr,sig.level=sig.thres,alternative = 'less')$n)
              msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group with', pwr*100,'% power!')
            }
          }      
          #}else if(calcVal =='power'){
          #  results <- pwr.2p2n.test(h =h , n1 =n1 , n2=n2, sig.level = sig, power = pwr)
          #  msg=paste('The power is',results$power)
          #}else if(calcVal =='sample size'){
          #  results <- pwr.2p.test(h =ES , sig.level = sig, power = pwr)
          #  msg=paste('Sample size required for both test and control group is',ceiling(results$n))
        }
        cat(msg)
        cat('\n\n')
        #cat('Details:\n')
        #results
      }else if(test_type=='two sample t-test'){
        inFile <- input$testfile
        if (is.null(inFile)){
          testdata <- read.csv('data/test_data.csv',header=TRUE)
        }else{
          testdata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,na.strings = "(null)")
        }
        if(ncol(testdata)>1){
          stop('Input test group data has more than one column! Please load a dataset with only 1 column/variable.')
        }
        inFile <- input$controlfile
        if (is.null(inFile)){
          controldata <- read.csv('data/control_data.csv',header=TRUE)
        }else{
          controldata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,na.strings = "(null)")
        }
        if(ncol(controldata)>1){
          stop('Input control group data has more than one column! Please load a dataset with only 1 column/variable.')
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
          #results<-pwr.t2n.test(n1 = n1,n2 = n2,d = d,sig.level=sig, power=0)
          results <- t.test(x,y,conf.level=1-sig.thres,alternative = "two.sided")
          if(results$p.value<sig.thres){
            if(d > 0){
              lift <- round((mean(x)-mean(y))/mean(y)*100,1)
              msg=paste('Test group has significantly higher value! \n The lift is',lift,'%. \n P-value is',round(results$p.value,3))
            }else{
              drop <- round((mean(y)-mean(x))/mean(y)*100,1)
              msg=paste('Test group has siginificantly lower value! \n The drop is',drop,'%. \n P-value is',round(results$p.value,3))       
            }            
          }else{
            sample.needed <- ceiling(pwr.t.test(d=d,power=pwr,sig.level=sig.thres)$n)
            msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group with', pwr*100,'% power!')
          }
        }else if(calcVal =='significant level(one-sided)'){
          if(d>0){
            #results <- pwr.t2n.test(d = d , n1 = n1 , n2 = n2, sig.level = sig, power = 0,alternative = 'greater')
            results <- t.test(x,y,conf.level=1-sig.thres,alternative = "greater")
            if(results$p.value<sig.thres){
              lift <- round((mean(x)-mean(y))/mean(y)*100,1)
              msg=paste('Test group has significantly higher value! \n The lift is',lift,'%. \n P-value is',round(results$p.value,3))
            }else{
              sample.needed <- ceiling(pwr.t.test(d=d,power=pwr,sig.level=sig.thres,alternative = 'greater')$n)
              msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group with', pwr*100,'% power!')
            }
          }else{
            #results <- pwr.t2n.test(d = d , n1 = n1 , n2 = n2, sig.level = sig, power = 0,alternative = 'less')
            results <- t.test(x,y,conf.level=1-sig.thres,alternative = "less")
            if(results$p.value<sig.thres){
              drop <- round((mean(y)-mean(x))/mean(y)*100,1)
              msg=paste('Test group has siginificantly lower value! \n The drop is',drop,'%. \n P-value is',round(results$p.value,3))       
            }else{
              sample.needed <- ceiling(pwr.t.test(d = d,power=pwr,sig.level=sig.thres,alternative = 'less')$n)
              msg=paste('There is no significant difference between test and control.\n Require ',sample.needed,'samples in both test and control group with', pwr*100,'% power!')
            }
          }      
          #}else if(calcVal =='power'){
          #  results <- pwr.t2n.test(d = d , n1 =n1 , n2=n2, sig.level = sig, power = pwr)
          #  msg=paste('The power is',results$power)
          #}else if(calcVal =='sample size'){
          #  results <- pwr.t.test(d = ES , sig.level = sig, power = pwr)
          #  msg=paste('Sample size required for both test and control group is',ceiling(results$n))
        }
        cat(msg)
        cat('\n\n')
        if(input$trim_upper>0 | input$trim_lower){
          cat('Note: Based on your selection, \n')
          if(input$trim_upper>0){
            cat('top',input$trim_upper*100,'% data are removed,\n')
          }
          if(input$trim_lower>0){
            cat('bottom',input$trim_lower*100,'% data are removed.\n')
          } 
        }
        
        cat('\n')
        #cat('details:')
        #print(results)
        cat('test group data statistics:\n')
        print(summary(x))
        cat('control group data statistics:\n')
        print(summary(y))        
      }else if(test_type=='power analysis: proportion test'){
        h = 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
        if(calcVal =='significant level(two-sided)'){
          results <- pwr.2p.test(h =h , sig.level = sig.thres, power = pwr, alternative = 'two.sided')
        }else if(calcVal =='significant level(one-sided)'){
          if(h>0){
            results <- pwr.2p.test(h =h , sig.level = sig.thres, power = pwr, alternative = 'greater')
          }else{
            results <- pwr.2p.test(h =h , sig.level = sig.thres, power = pwr, alternative = 'less')
          }
        }
        msg=paste('Sample size required for both test and control group is',ceiling(results$n))
        cat(msg)
      }else if(test_type=='power analysis: t-test'){
        if(calcVal =='significant level(two-sided)'){
          results <- pwr.t.test(d = ES , sig.level = sig.thres, power = pwr, alternative = 'two.sided')
        }else if(calcVal =='significant level(one-sided)'){
          if(ES>0){
            results <- pwr.t.test(d = ES , sig.level = sig.thres, power = pwr, alternative = 'greater')
          }else{
            results <- pwr.t.test(d = ES , sig.level = sig.thres, power = pwr, alternative = 'less')
          }
        }
        msg=paste('Sample size required for both test and control group is',ceiling(results$n))
        cat(msg)
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
          testdata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,na.strings = "(null)")
        }
        if(ncol(testdata)>1){
          stop('Input test group data has more than one column! Please load a dataset with only 1 column/variable.')
        }
        inFile <- input$controlfile
        if (is.null(inFile)){
          controldata <- read.csv('data/control_data.csv',header=TRUE)
        }else{
          controldata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,na.strings = "(null)")
        }
        if(ncol(controldata)>1){
          stop('Input control group data has more than one column! Please load a dataset with only 1 column/variable.')
        }
        x <- testdata[,1]
        y <- controldata[,1]
        x <- x[x>=quantile(x,input$trim_lower) & x<=quantile(x,1-input$trim_upper)]
        y <- y[y>=quantile(y,input$trim_lower) & y<=quantile(y,1-input$trim_upper)]
        resample.count <- 10000
        if(max(length(x),length(y))>500000){
          resample.count <- max(floor(10000*500000/max(length(x),length(y))),500)
        }
        #cat('resample size:',resample.count,'\n')
        result<-sapply(1:resample.count,function(z){
          xsample <- sample(x,length(x),replace = TRUE)
          ysample <- sample(y,length(y),replace = TRUE)
          win.x <- mean(xsample)>mean(ysample)
          win.y <- mean(xsample)<mean(ysample)
          c(win.x,win.y)
        })
        win.prob <- round(rowSums(result)/resample.count,2)   
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
        head(testdata <- read.csv('data/test_data.csv',header=TRUE,nrows=10))
      }else{
        head(testdata <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,na.strings = "(null)"))
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
        head(controldata<-read.csv('data/control_data.csv',header=TRUE,nrows=10))
      }else{
        head(controldata<-read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,na.strings = "(null)"))
      }      
    }
    
  })
})
