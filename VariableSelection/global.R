#check for and/or install dependencies
need<-c("shiny","party","parallel","snowfall") # some more should auto load...
for(i in 1:length(need)){
  if(require(need[i], character.only = TRUE)==FALSE) install.packages(need[i])
  library(need[i], character.only = TRUE)
}

