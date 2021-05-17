#-------------------------------------------------------------------------------
#Calculates Reliable Change Index and Clinical Significance for Depression as measured on the QIDS-score
#-------------------------------------------------------------------------------

#Function used to Classify the change patients have undergone from 
#time point T1 till T2 based on CS and RCI
Classify_RCI_CS<-function(RCI_1.96_T1_T2, CS_T1, CS_T2 ){
  out<-numeric(length(RCI_1.96_T1_T2))
  for (i in 1: length(RCI_1.96_T1_T2)){
    if(is.na(RCI_1.96_T1_T2[i]) | is.na(CS_T1[i]) | is.na(CS_T2[i])){
      out[i]<-NA
    } else if(RCI_1.96_T1_T2[i]=="BETTER" & CS_T1[i]=="DEP" & CS_T2[i]=="HEALTHY"){
      out[i]<-'RECOVERED'
    } else if (RCI_1.96_T1_T2[i]=="BETTER"){
      out[i]<-'IMPROVED'
    } else if (RCI_1.96_T1_T2[i]=="WORSE"){
      out[i]<-'DETERIORATED'
    } else{
      out[i]<-'UNCHANGED'
    }
  }
  return(out)
}

#Data is read and libraries are loaded in Creating_data.R
source("Creating_data.R")
#Create data
data<-create_data()
attach(data)


#-------------------------------------------------------------------------------
#QIDS-SD 16 values 
#-------------------------------------------------------------------------------
#Reference: https://www.sciencedirect.com/science/article/pii/S0165032716305171?via%3Dihub
QIDS_test_retest<-0.49
#Calculating means and SD's for groups with and without MDD in data
QIDS_healthy_mean<-mean(data$QIDS_IN_TOTAAL[binarydepIN==0], na.rm=TRUE)
QIDS_healthy_SD<-sd(data$QIDS_IN_TOTAAL[binarydepIN==0], na.rm=TRUE)
QIDS_MDD_mean<-mean(data$QIDS_IN_TOTAAL[binarydepIN==1], na.rm=TRUE)
QIDS_MDD_SD<-sd(data$QIDS_IN_TOTAAL[binarydepIN==1], na.rm=TRUE)
#Using these values we need a change of 5.8!! on the QIDS questionnaire to have a reliable change!!
#Mainly caused by the small test-retest reliability and the high sd in the 
#non MDD group (I found similar figures in articles)


#-------------------------------------------------------------------------------
#Calculating RCI
#-------------------------------------------------------------------------------
#Calculating RCI for all patients
#RCI from Intake to Terugkom
data$RCI_IN_TK<-(data$QIDS_IN_TOTAAL-data$QIDS_TK_TOTAAL)/
  (QIDS_healthy_SD*sqrt(1-QIDS_test_retest))
data$RCI_1.96_IN_TK<-ifelse(data$RCI_IN_TK>1.96, 'BETTER', 
                            ifelse(data$RCI_IN_TK<(-1.96), 'WORSE', 'SAME'))

#RCI from Intake to Follow-up
data$RCI_IN_FU<-(data$QIDS_IN_TOTAAL-data$QIDS_FU_TOTAAL)/
  (QIDS_healthy_SD*sqrt(1-QIDS_test_retest))
data$RCI_1.96_IN_FU<-ifelse(data$RCI_IN_FU>1.96, 'BETTER', 
                            ifelse(data$RCI_IN_FU<(-1.96), 'WORSE', 'SAME'))


#-------------------------------------------------------------------------------
#Calculating CS
#-------------------------------------------------------------------------------
#Determine whether QIDS score more probable under MDD distribution or healthy distribution
data$CS_IN<-ifelse(dnorm(data$QIDS_IN_TOTAAL, mean=QIDS_healthy_mean, sd=QIDS_healthy_SD)<
                     dnorm(data$QIDS_IN_TOTAAL, mean=QIDS_MDD_mean, sd=QIDS_MDD_SD), 'DEP', 'HEALTHY')
data$CS_TK<-ifelse(dnorm(data$QIDS_TK_TOTAAL, mean=QIDS_healthy_mean, sd=QIDS_healthy_SD)<
                     dnorm(data$QIDS_TK_TOTAAL, mean=QIDS_MDD_mean, sd=QIDS_MDD_SD), 'DEP', 'HEALTHY')
data$CS_FU<-ifelse(dnorm(data$QIDS_FU_TOTAAL, mean=QIDS_healthy_mean, sd=QIDS_healthy_SD)<
                     dnorm(data$QIDS_FU_TOTAAL, mean=QIDS_MDD_mean, sd=QIDS_MDD_SD), 'DEP', 'HEALTHY')
#Determine if the change is Clinically significant from intake to terugkom and to follow-up
data$CS_change_IN_TK<-ifelse(data$CS_IN=='DEP' & data$CS_TK=='HEALTHY', TRUE, FALSE)
data$CS_change_IN_FU<-ifelse(data$CS_IN=='DEP' & data$CS_FU=='HEALTHY', TRUE, FALSE)



#-------------------------------------------------------------------------------
#Classifies patients depending on RCI and CS
#-------------------------------------------------------------------------------
data$RCI_CS_Classification_IN_TK_QIDS<-Classify_RCI_CS(data$RCI_1.96_IN_TK, data$CS_IN, data$CS_TK)
data$RCI_CS_Classification_IN_FU_QIDS<-Classify_RCI_CS(data$RCI_1.96_IN_FU, data$CS_IN, data$CS_FU)


#-------------------------------------------------------------------------------
#Construct Classification table
#-------------------------------------------------------------------------------
#Find indexes of patients with depression as on MINI
indexes<-binarydepIN==1
indexes[is.na(indexes)==TRUE]<-FALSE
#Only use data for patients with depression as on MINI
QIDS_Classification_table<-round(rbind(
  addmargins(table(data$RCI_CS_Classification_IN_TK_QIDS[indexes], exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_TK_QIDS[indexes], exclude=NULL)/sum(indexes)) ,
  addmargins(table(data$RCI_CS_Classification_IN_FU_QIDS[indexes], exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_FU_QIDS[indexes], exclude=NULL)/sum(indexes))),digits=0)

rownames(QIDS_Classification_table)<-c('Change from Intake to Terugkom in number of subjects',
                                       'Change from Intake to Terugkom in percentages',
                                       'Change from Intake to Follow-up in number of subjects',
                                       'Change from Intake to Follow-up in percentages')

QIDS_Classification_table
