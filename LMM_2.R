#-------------------------------------------------------------------------------
#Fits a Linear Mixed Model with as outcome PTSD severity as measured by the CAPS
#-------------------------------------------------------------------------------

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
#Uncomment to test whether outliers have effect on results
#-------------------------------------------------------------------------------
#To check whether outliers have an effect on our results. They don't :)
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
long_data_2 <- pivot_longer(data = data, 
                            cols = c("totaalscore_KIP_IN", "totaalscore_KIP_TK", "totaalscore_KIP_FU"), 
                            names_to = "Time", values_to = "CAPS_score")

long_data_2$Time[long_data_2$Time == "totaalscore_KIP_IN"] <- "Intake"
long_data_2$Time[long_data_2$Time == "totaalscore_KIP_TK"] <- "Post-treatment"
long_data_2$Time[long_data_2$Time == "totaalscore_KIP_FU"] <- "6-months FU"
long_data_2$Time <- factor(long_data_2$Time, levels = c("Intake", "Post-treatment", "6-months FU"))
long_data_2$binarydepIN <- factor(long_data_2$binarydepIN)

#-------------------------------------------------------------------------------
#Choosing model
#-------------------------------------------------------------------------------
#Elaborate mean model to determine random effects structure:
LMM_2.1<-lme(CAPS_score~Time*geslacht*binarydepIN*(I(Age) +I(Age^2)), random=~Time|CIN, 
             data=long_data_2, na.action=na.exclude, method = "REML")
summary(LMM_2.1)$tTable
LMM_2.2 <- lme(CAPS_score~Time*geslacht*binarydepIN*(I(Age) +I(Age^2)), random=~1|CIN, 
               data=long_data_2, na.action=na.exclude, method = "REML") 
anova(LMM_2.1, LMM_2.2) #random slopes needed

#Reduce mean part:
LMM_2.1 <- update(LMM_2.1, method = "ML")
LMM_2.3 <- lme(CAPS_score~Time*geslacht*binarydepIN*Age, random=~Time|CIN, data=long_data_2, 
               na.action=na.exclude, method = "ML") 
anova(LMM_2.1, LMM_2.3) # no indication that bigger model with quadratic terms is needed

summary(LMM_2.3)$tTable 
#remove interactions except for the one for the research question and important covariate interactions
#First removing 4 and 3 way interaction
LMM_2.4 <-lme(CAPS_score~ Time*factor(binarydepIN) + Time*geslacht+Time*Age, random=~Time|CIN, 
              data=long_data_2, na.action=na.exclude, method = "ML") 
anova(LMM_2.3, LMM_2.4) #Looks like 3- and 4-way interactions are not necessary


#-------------------------------------------------------------------------------
#Final model
#-------------------------------------------------------------------------------
LMM_2.4 <- update(LMM_2.4, method = "REML")


#-------------------------------------------------------------------------------
#Significance testing
#-------------------------------------------------------------------------------
#Testing interaction age and time
Lmat_ageint <- rbind(c(rep(0,10),1,0), c(rep(0,11),1))
anova(LMM_2.4, L=Lmat_ageint)
#Testing interaction between sex and time
Lmat_sexint <- rbind(c(rep(0,8),1,0,0,0), c(rep(0,9),1,0,0))
anova(LMM_2.4, L=Lmat_sexint)
#Testing interaction between group (MDD or non-MDD) and time
Lmat_treatint <- rbind(c(rep(0,6),1,0), c(rep(0,7),1))
anova(LMM_2.4, L = Lmat_treatint)
#Testing change over time
Lmat_time <- rbind(c(0,1, rep(0,10)), c(0,0,1, rep(0,9)))
anova(LMM_2.4, L = Lmat_time)
#Testing change from post-treatment to follow-up
Lmat_postFU <- rbind(c(0,1,-1,rep(0,9)))
anova(LMM_2.4, L = Lmat_postFU)

#Summary individual coefficients and post-hoc results:
summary(LMM_2.4)$tTable 

#-------------------------------------------------------------------------------
#Testing Model assumptions:
#-------------------------------------------------------------------------------
#Distribution of marginal residuals is approximately normal
qqnorm(residuals(LMM_2.4,level=0, type='normalized'))
#Distribution of conditional residuals is approximately normal
qqnorm(residuals(LMM_2.4,level=1, type='normalized'))
#No clear pattern in residuals vs fitted values
plot(LMM_2.4$fitted[,2], residuals(LMM_2.4,level=1, type='normalized')[!is.na(residuals(LMM_2.4,level=0, type='response'))])
#No clear heterogeneity of variance over the time levels
plot(resid(LMM_2.4, type = "n") ~ long_data_2$Time, 
     type = c("p", "smooth"), lwd = 3)


#-------------------------------------------------------------------------------
#Plotting the results
#-------------------------------------------------------------------------------
newdatQ2 <- expand.grid(Time = unique(long_data_2$Time), binarydepIN = c(0,1), 
                        geslacht=0, Age= 0)
newdatQ2$binarydepIN <- factor(newdatQ2$binarydepIN)
predicted_pop_means <-  predict(LMM_2.4, level = 0, newdata=newdatQ2)
predicted_means_df <- data.frame(Time= rep(c("Intake", "Post-treatment", "6 months follow-up"),2),CAPS_score = predicted_pop_means, 
                                     binarydepIN = factor(c(0,0,0,1,1,1)))
predicted_means_df$Time <- factor(predicted_means_df$Time, levels = c("Intake", "Post-treatment", "6 months follow-up"))
ggplot(predicted_means_df, aes(x=Time, y =CAPS_score, colour = binarydepIN)) + 
  geom_line(aes(group =binarydepIN), size=3) + 
  scale_colour_manual(name = "Comorbid depression", labels= c("no", "yes"), values = c("#00BFC4","#F8766D"))  +ylab("total CAPS score")





