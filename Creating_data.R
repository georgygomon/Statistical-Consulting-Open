#-------------------------------------------------------------------------------
#Reads data, creates new variables and loads required packages
#-------------------------------------------------------------------------------

#All packages are loaded and if necessary installed
if(!require("aod")){install.packages("aod"); library("aod")}
if(!require("foreign")){install.packages("foreign"); library("foreign")}
if(!require("ggplot2")){install.packages("ggplot2"); library("ggplot2")}
if(!require("nlme")){install.packages("nlme"); library("nlme")}
if(!require("lme4")){install.packages("lme4"); library("lme4")}
if(!require("tidyverse")){install.packages("tidyverse"); library("tidyverse")}

#Function that reads in the data and creates new variables for binary depression at intake and follow-up
#Note:by default the function takes a completers analysis sample; but you can change sample = 'completers' 
#to sample = 'itt' for intention to treat sample if you call Creating_data()
create_data<-function(sample = 'completers'){ 
  PTSD_MDD_data <- as.data.frame(read.spss(file = "My_version.sav", use.value.labels = F)) 
  if(sample = 'completers'){
  data <- PTSD_MDD_data[PTSD_MDD_data$BEH =="1" & PTSD_MDD_data$TOEWO==1 & PTSD_MDD_data$BEHVB == 0,]}
  else if(sample = 'itt'){
  data <- PTSD_MDD_data[PTSD_MDD_data$BEH =="1" & PTSD_MDD_data$TOEWO==1,]}

  
  #Create Binary Depression variable for intake from the MINI
  for (i in 1:nrow(data)){
    if(is.na(data$depressie_IN[i])){
      data$binarydepIN[i] <- NA}
    else if(data$depressie_IN[i] == 0|data$depressie_IN[i] == 4|data$depressie_IN[i] == 5|
            data$depressie_IN[i] == 6){
      data$binarydepIN[i] <- 0
    } else {
      data$binarydepIN[i] <- 1}
  }

  #Create Binary Depression variable for follow-up from the MINI
  for (i in 1:nrow(data)){
    if(is.na(data$depressie_FU[i])){
      data$binarydepFU[i] <- NA}
    else if(data$depressie_FU[i] == 0|data$depressie_FU[i] == 4|data$depressie_FU[i] == 5|
            data$depressie_FU[i] == 6){
      data$binarydepFU[i] <- 0
    } else {
      data$binarydepFU[i] <- 1}
  }
  return(data)
}
