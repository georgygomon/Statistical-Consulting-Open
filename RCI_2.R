#-------------------------------------------------------------------------------
#Calculates Reliable Change Index and Clinical Significance for PTSD as measured on the CAPS
#-------------------------------------------------------------------------------

#Function used to Classify the change patients have undergone from 
#time point T1 till T2 based on CS and RCI
Classify_RCI_CS<-function(RCI_1.96_T1_T2, CS_T2 ){
  out<-numeric(length(RCI_1.96_T1_T2))
  for (i in 1: length(RCI_1.96_T1_T2)){
    if(is.na(RCI_1.96_T1_T2[i]) | is.na(CS_T2[i])){
      out[i]<-NA
    } else if(RCI_1.96_T1_T2[i]=="BETTER" &  CS_T2[i]=="HEALTHY"){
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
data<-create_data()
attach(data)

#-------------------------------------------------------------------------------
#CAPS-5 values
#-------------------------------------------------------------------------------
#Reference: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5805662/
CAPS_test_retest<-0.83
#https://www.ptsd.va.gov/professional/assessment/documents/CAPSmanual.pdf
#Here is says that a change of 15 points is considered clinically significant. 
#CAPS-5 value for healthy population. This should be looked at more closely later, to also
#introduce the different PTSD categories (absent, mild, moderate, severe)
CAPS_healthy_mean<-10
CAPS_healthy_SD<-8
#Calculating means and SD's for groups with PTSD
CAPS_PTSD_mean<-mean(data$totaalscore_KIP_IN, na.rm=TRUE)
CAPS_PTSD_SD<-sd(data$totaalscore_KIP_IN, na.rm=TRUE)


#-------------------------------------------------------------------------------
#Calculating RCI
#-------------------------------------------------------------------------------
#From Intake to Terugkom
data$CAPS_RCI_IN_TK<-(data$totaalscore_KIP_IN-data$totaalscore_KIP_TK)/
  (CAPS_healthy_SD*sqrt(1-CAPS_test_retest))
data$CAPS_RCI_1.96_IN_TK<-ifelse(data$CAPS_RCI_IN_TK>1.96, 'BETTER', 
                                 ifelse(data$CAPS_RCI_IN_TK<(-1.96), 'WORSE', 'SAME'))
#From Intake to Follow-Up
data$CAPS_RCI_IN_FU<-(data$totaalscore_KIP_IN-data$totaalscore_KIP_FU)/
  (CAPS_healthy_SD*sqrt(1-CAPS_test_retest))
data$CAPS_RCI_1.96_IN_FU<-ifelse(data$CAPS_RCI_IN_FU>1.96, 'BETTER', 
                                 ifelse(data$CAPS_RCI_IN_FU<(-1.96), 'WORSE', 'SAME'))

#-------------------------------------------------------------------------------
#Calculating CS
#-------------------------------------------------------------------------------
#Determine whether QIDS score more probable under MDD distribution or healthy distribution
data$CAPS_CS_TK<-ifelse(dnorm(data$totaalscore_KIP_TK, mean=CAPS_healthy_mean, sd=CAPS_healthy_SD)<
                          dnorm(data$totaalscore_KIP_TK, mean=CAPS_PTSD_mean, sd=CAPS_PTSD_SD), 'PTSD', 'HEALTHY')
data$CAPS_CS_FU<-ifelse(dnorm(data$totaalscore_KIP_FU, mean=CAPS_healthy_mean, sd=CAPS_healthy_SD)<
                          dnorm(data$totaalscore_KIP_FU, mean=CAPS_PTSD_mean, sd=CAPS_PTSD_SD), 'DEP', 'HEALTHY')

#Determine what the change in CS is from intake to terugkom and to follow-up
data$CS_change_IN_TK<-ifelse(data$CAPS_CS_TK=='HEALTHY', TRUE, FALSE)
data$CS_change_IN_FU<-ifelse(data$CAPS_CS_FU=='HEALTHY', TRUE, FALSE)


#-------------------------------------------------------------------------------
#Classifies patients depending on RCI and CS
#-------------------------------------------------------------------------------
data$RCI_CS_Classification_IN_TK_CAPS<-Classify_RCI_CS(data$CAPS_RCI_1.96_IN_TK, data$CAPS_CS_TK)
data$RCI_CS_Classification_IN_FU_CAPS<-Classify_RCI_CS(data$CAPS_RCI_1.96_IN_FU, data$CAPS_CS_FU)



#-------------------------------------------------------------------------------
#Construct Classification tables
#-------------------------------------------------------------------------------
#Classification table for all patients
CAPS_Classification_table_Total<-round(rbind(
  addmargins(table(data$RCI_CS_Classification_IN_TK_CAPS, exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_TK_CAPS, exclude=NULL)/nrow(data)),
  addmargins(table(data$RCI_CS_Classification_IN_FU_CAPS, exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_FU_CAPS, exclude=NULL)/nrow(data))),digits=0)
rownames(CAPS_Classification_table_Total)<-c('Total: Change from Intake to Terugkom in number of subjects',
                                             'Total: Change from Intake to Terugkom in percentages',
                                             'Total: Change from Intake to Follow-up in number of subjects',
                                             'Total: Change from Intake to Follow-up in percentages')


#Classification table for MDD patients
#Find indexes of patients with depression as on MINI
indexes_MDD<-binarydepIN==1
indexes_MDD[is.na(indexes_MDD)==TRUE]<-FALSE
indexes_non_MDD<-binarydepIN==0
indexes_non_MDD[is.na(indexes_non_MDD)==TRUE]<-FALSE
CAPS_Classification_table_MDD<-round(rbind(
  addmargins(table(data$RCI_CS_Classification_IN_TK_CAPS[indexes_MDD], exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_TK_CAPS[indexes_MDD], exclude=NULL)/sum(indexes_MDD)),
  addmargins(table(data$RCI_CS_Classification_IN_FU_CAPS[indexes_MDD], exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_FU_CAPS[indexes_MDD], exclude=NULL)/sum(indexes_MDD))),
  digits=0)
rownames(CAPS_Classification_table_MDD)<-c('MDD: Change from Intake to Terugkom in number of subjects',
                                           'MDD: Change from Intake to Terugkom in percentages',
                                           'MDD: Change from Intake to Follow-up in number of subjects',
                                           'MDD: Change from Intake to Follow-up in percentages')

#Classification table for Non-MDD patients
CAPS_Classification_table_non_MDD<-round(rbind(
  addmargins(table(data$RCI_CS_Classification_IN_TK_CAPS[indexes_non_MDD], exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_TK_CAPS[indexes_non_MDD], exclude=NULL)/sum(indexes_non_MDD)),
  addmargins(table(data$RCI_CS_Classification_IN_FU_CAPS[indexes_non_MDD], exclude=NULL)),
  addmargins(100*table(data$RCI_CS_Classification_IN_FU_CAPS[indexes_non_MDD], exclude=NULL)/sum(indexes_non_MDD))),
  digits=0)
rownames(CAPS_Classification_table_non_MDD)<-c('Non-MDD: Change from Intake to Terugkom in number of subjects',
                                               'Non-MDD: Change from Intake to Terugkom in percentages',
                                               'Non-MDD: Change from Intake to Follow-up in number of subjects',
                                               'Non-MDD: Change from Intake to Follow-up in percentages')


CAPS_Classification_table<-rbind(CAPS_Classification_table_MDD, 
                                 CAPS_Classification_table_non_MDD, 
                                 CAPS_Classification_table_Total)

#Note that 164 (total with MDD) +117 (total without MDD)=281, which is not equal to grand total of 284,
#since for 3 patients the depression on the MINI at intake is unknown.
CAPS_Classification_table
