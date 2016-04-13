source('global.R')
options(shiny.maxRequestSize=1024^4)
shinyServer(function(input, output){
  
  #logo
  output$logo <- renderImage({
    filename <- 'www/lottery_girls.jpg'
    list(src = filename,
         width = 272,
         height = 153,
         alt = "Statistical Testing Lab")
    
  }, deleteFile = FALSE)
  
  #data preview_test
  output$preview <- renderTable({
    inFile <- input$datafile
    if (is.null(inFile)){
      return(NULL)
    }else{
      read.csv(inFile$datapath, header = TRUE,sep = input$sep, quote = input$quote,nrow=10)
    }   
  })
  
  Dataset <- reactive({
    inFile <- input$datafile
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath, header = TRUE,sep = input$sep, quote = input$quote,nrow=10)
  })
  
  output$response <- renderUI({    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    cols <- names(Dataset())
    selectInput("dependent.var", "Select a response variable:",choices=cols, selected=cols, multiple=F)     
  })
  
  output$variables <- renderUI({    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    cols <- names(Dataset())
    selectInput("independent.variables", "Select all the variables of interests:",choices=cols, multiple=T)     
  })
  
  output$selected <- renderTable({
    independent.variables <- input$independent.variables
    dependent.var <- input$dependent.var
    if (is.null(independent.variables) || length(independent.variables)==0) return(NULL)
    if (is.null(dependent.var) || length(dependent.var)==0) return(NULL)
    Dataset()[,independent.variables,drop=FALSE]
  })
  
  Importance <- reactive({
    input$calculate
    isolate({
      independent.variables <- input$independent.variables
      dependent.var <- input$dependent.var
      if (is.null(independent.variables) || length(independent.variables)==0) return(NULL)
      if (is.null(dependent.var) || length(dependent.var)==0) return(NULL)
      data <- read.csv(input$datafile$datapath, header = TRUE,sep = input$sep, quote = input$quote)
      n <- nrow(data)
      attributes <- data.frame(data[, independent.variables])
      y <- data[, dependent.var]
      attributes <- cbind(attributes, 
                          Noise1 = rnorm(length(y), mean=0, sd=1), 
                          Noise2 = rnorm(length(y), mean=0, sd=1))
      for(i in seq_along(attributes)){
        if(class(attributes[,i])=='character') attributes[,i] <- factor(attributes[,i])
      }
      data.set <- cbind(attributes,y)
      
      if(n <= 10000){
        current.rf <- cforest(y~.,data=data.set, 
                              controls=cforest_unbiased(ntree=500, mtry=floor(sqrt(ncol(data.set)-1)))) 
        current.varimp <- varimp(current.rf)
        importance <- sort(current.varimp)
      }else{
        value.count <- tapply(y, y, length)
        data.count <- nrow(data.set)
        if (length(value.count) > 20 & is.numeric(y) == TRUE) {
          y.mean <- mean(abs(y))
          y.var <- var(y)
          sample.size <- ceiling(y.var/(y.mean)^2)*100     
          if(sample.size>50000 & sum(y!=0)/length(y)<.01){
            conv.rate <- sum(y!=0)/length(y)
            sample.size <- ceiling((1-conv.rate)/conv.rate * 200)
          }
        } else {
          ## the size of the smallest group
          min.value.count <- min(value.count)
          sample.size <- ceiling((data.count - min.value.count)/min.value.count)*200   
        }  
        weights <- rep(1,length(y))
        if(input$weights & all(unique(y)%in%c(0,1))){
          cat('assigning weights ...\n')
          conv.rate <- mean(y)
          weights[y==1] <- (1-conv.rate)/conv.rate
        } 
        if(sample.size < min(.01*nrow(data.set),10000)) sample.size <- ceiling(min(.01*nrow(data.set),10000))
        cat('Sample size is:',sample.size,'\n')
        
        cl <- makeCluster(detectCores()-1)
        clusterExport(cl,c('data.set','sample.size','weights'),envir = environment())
        imp.mx <- parSapply(cl,1:500,function(i){
          require(party)
          sample.idx <- sample.int(nrow(data.set), size=sample.size, replace=TRUE,prob=weights/sum(weights))
          current.data.set <- data.set[sample.idx, ]
          current.rf <- cforest(y~.,data=current.data.set, 
                                controls=cforest_unbiased(ntree=1, mtry=floor(sqrt(ncol(current.data.set)-1)))) 
          current.varimp <- varimp(current.rf)
          current.varimp
        })      
        stopCluster(cl)
        importance<-sort(rowMeans(imp.mx))
      }#end or rrf
     importance
    })
  })
  
  output$summary_plot <- renderPlot({
    input$calculate
    isolate({
      independent.variables <- input$independent.variables
      dependent.var <- input$dependent.var
      importance <- Importance()
      boxplot(t(importance),las=2,cex.axis=.7,
              main=paste('importance of attributes on',dependent.var))
      score<-importance
      noise.score <- max(abs(score[names(score)%in%c('Noise1','Noise2')]))
      abline(h=noise.score,col=2,lty=2)
      score <- score[score > noise.score]
      score_cutoff<-(max(score)-max(min(score),0))*0.05 + max(min(score),0)
      abline(h=score_cutoff,col=3,lty=2)
      var.selected <- names(sort(score[score>score_cutoff]))
      cat('selected variables are:',var.selected)
    })   
  })
  output$summary_table <- renderTable({
    input$calculate
    isolate({
      importance <- Importance()
      out <- data.frame(importance[length(importance):1])
      out <- out/sum(out)*100
      names(out) <- "normalized importance score"
      out
    })
  })
  
})
