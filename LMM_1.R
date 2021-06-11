#-----------------------------------------------------------------------------------------
#Fits a Linear Mixed Model with as outcome the depression severity as measured by the QIDS
#-----------------------------------------------------------------------------------------

#Data is read and libraries are loaded in Creating_data.R
source("Creating_data.R")
data<-create_data()
attach(data)

# For an intention-to-treat analysis: uncomment the following code:
# rm(list = ls())  
# source("Creating_data.R")
# data <- create_data(sample = 'itt')


#Centering age and Sex
for (i in 1:nrow(data)){
  if (data$geslacht[i] ==1) {data$geslacht[i] <--0.5} else {data$geslacht[i] <-0.5}
}

data$Age <- scale(data$Age, center = T, scale =F)


#-------------------------------------------------------------------------------
#Uncomment to test whether outliers haveeffect on results
#-------------------------------------------------------------------------------
#They do, they make the interaction between Female:Follow_up non-significant
# upper<-quantile(data$tijd_IN_TK,  na.rm = TRUE, probs=c(.25, .75))[2]+
#   1.5*IQR(data$tijd_IN_TK,  na.rm = TRUE)
# lower<-quantile(data$tijd_IN_TK,  na.rm = TRUE, probs=c(.25, .75))[1]-
#   1.5*IQR(data$tijd_IN_TK,  na.rm = TRUE)
# ouliers_IN_TK<-c(which(data$tijd_IN_TK>upper),which(data$tijd_IN_TK<lower))
# upper<-quantile(data$tijd_TK_FU,  na.rm = TRUE, probs=c(.25, .75))[2]+
#   1.5*IQR(data$tijd_TK_FU,  na.rm = TRUE)
# lower<-quantile(data$tijd_TK_FU,  na.rm = TRUE, probs=c(.25, .75))[1]-
#   1.5*IQR(data$tijd_TK_FU,  na.rm = TRUE)
# ouliers_TK_FU<-c(which(data$tijd_TK_FU>upper),which(data$tijd_TK_FU<lower))
# all_outliers<-c(ouliers_IN_TK, ouliers_TK_FU)
# data<-data[-all_outliers,]


#-------------------------------------------------------------------------------
#Creating long dataset
#-------------------------------------------------------------------------------
#Transform data into long format
long_data_1 <- pivot_longer(data = data, cols = c("QIDS_IN_TOTAAL", "QIDS_TK_TOTAAL", "QIDS_FU_TOTAAL"), 
                            names_to = "Time", values_to = "QIDS_score")
long_data_1$Time[long_data_1$Time == "QIDS_IN_TOTAAL"] <- "Intake"
long_data_1$Time[long_data_1$Time == "QIDS_TK_TOTAAL"] <- "Post-treatment"
long_data_1$Time[long_data_1$Time == "QIDS_FU_TOTAAL"] <- "6-months FU"
long_data_1$Time <- factor(long_data_1$Time, levels = c("Intake", "Post-treatment", "6-months FU"))


#-------------------------------------------------------------------------------
#Final easy Model (only time as covariate)
#-------------------------------------------------------------------------------
LMM_1.1a<-lme(QIDS_score~Time, random=~Time|CIN, 
             data=long_data_1, na.action=na.exclude, method = "REML") 
#Only random intercept
LMM_1.2a <- lme(QIDS_score~Time, random=~1|CIN, 
               data=long_data_1, na.action=na.exclude, method = "REML") 
anova(LMM_1.1a, LMM_1.2a) #random intercepts and slopes is better

#Summary of the model and post-hoc results for individual timepoints:
summary(LMM_1.1a)$tTable

#-------------------------------------------------------------------------------
#Significance testing with easy model
#-------------------------------------------------------------------------------
#Significance of overall change
Lmat_timea <- rbind(c(0,1,0), c(0,0,1))
anova(LMM_1.1a, L = Lmat_timea)
#Significance of change from post-treatment to follow-up
Lmat_timediff <- rbind(c(0,1,-1))
anova(LMM_1.1a, L = Lmat_timediff)

#------------------------------------------------------------------------------
#Plotting result:
#------------------------------------------------------------------------------
newdat <- expand.grid(Time = unique(long_data_1$Time))
fittedmeans <- data.frame(newdat, QIDS_score = predict(LMM_1.1a, level = 0, newdata=newdat))
fittedmeans$Time  <- c("Intake", "Post-treatment", "6 months follow-up")
fittedmeans$Time <- factor(fittedmeans$Time, levels =  c("Intake", "Post-treatment", "6 months follow-up"))
ggplot() +geom_line(data =fittedmeans, aes(x=Time, y =QIDS_score, group =0), size =3, colour = "#00BFC4") +
  ylab("QIDS score")+ylim(8.5,16)   




#------------------------------------------------------------------------------
#Finding best elaborate model
#------------------------------------------------------------------------------
#Elaborate mean model to determine random effects structure
LMM_1.1<-lme(QIDS_score~Time*geslacht*(I(Age) + I(Age^2)) + LEC_cat, random=~Time|CIN, 
             data=long_data_1, na.action=na.exclude, method = "REML") 
LMM_1.2 <- lme(QIDS_score~Time*geslacht*(I(Age) +I(Age^2)) + LEC_cat, random=~1|CIN, 
               data=long_data_1, na.action=na.exclude, method = "REML") 
anova(LMM_1.1, LMM_1.2) #for now clear that random slopes are needed as well


#Reduce mean part:
summary(LMM_1.1)$tTable
LMM_1.3 <- update(LMM_1.1, method = "ML")
LMM_1.4 <- lme(QIDS_score~Time*geslacht*Age, random=~Time|CIN, data=long_data_1, 
               na.action=na.exclude, method = "ML") 
#better fit with quadratics, but AIC is lower without, so leave that variable out.
anova(LMM_1.3, LMM_1.4)
LMM_1.4 <- update(LMM_1.4, method = "REML")
summary(LMM_1.4)$tTable
#3-way interactions needed?
anova(LMM_1.4, Terms = "Time:geslacht:Age") #no

#-------------------------------------------------------------------------------
#Final elaborate Model
#-------------------------------------------------------------------------------
LMM_1.5 <- lme(QIDS_score~Time*geslacht+Time*Age, random=~Time|CIN, data=long_data_1, 
               na.action=na.exclude, method = "REML") 


#-------------------------------------------------------------------------------
#Significance testing
#-------------------------------------------------------------------------------
#Significance of overall change with time
Lmat_Time <- rbind(c(0,1,rep(0,3)), c(0,0,1, rep(0,2)))
anova(LMM_1.5, L=Lmat_Time)
#Testing for interaction between sex and time
Lmat_sexint <- rbind(c(rep(0,5),1,rep(0,3)), c(rep(0,6),1,0,0))
anova(LMM_1.5, L = Lmat_sexint)
#Testing for interaction between age and time
Lmat_ageint <- rbind(c(rep(0,7),1,0), c(rep(0,8),1))
anova(LMM_1.5, L = Lmat_ageint)
#Significance of change from post-treatment to follow-up
Lmat_postFU_diff <- rbind(c(0,1,-1, rep(0,6)))
anova(LMM_1.5, L = Lmat_postFU_diff)
#Significance of difference from post-treatment to follow-up between sexes
Lmat_sexesdiff_postFU <- rbind(c(rep(0,5),1, -1,0,0))
anova(LMM_1.5, L =Lmat_sexesdiff_postFU )


#-------------------------------------------------------------------------------
#Testing Model assumptions:
#-------------------------------------------------------------------------------
#Distribution of marginal residuals is approximately normal
qqnorm(residuals(LMM_1.5,level=0, type='normalized'))
#Distribution of conditional residuals is approximately normal
qqnorm(residuals(LMM_1.5,level=1, type='normalized'))
#No clear pattern in residuals vs fitted values
plot(LMM_1.5$fitted[,2], residuals(LMM_1.5,level=1, type='normalized')[!is.na(residuals(LMM_1.5,level=0, type='response'))])
#No signs of heterogeneity of variance per time slot
plot(resid(LMM_1.5, type = "n") ~ long_data_1$Time, 
     type = c("p", "smooth"), lwd = 3)

