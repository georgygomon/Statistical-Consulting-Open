#-------------------------------------------------------------------------------
#Fits a Binomial Generalized Linear Mixed Model with as binary outcome depression on the MINI
#-------------------------------------------------------------------------------

#Data is read and libraries are loaded in Creating_data.R
source("Creating_data.R")
data<-create_data()
attach(data)

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
#Creating long format based on MINI scores:
long_data_MINI <- pivot_longer(data = data, cols = c("binarydepIN", "depressie_MINI_TK", "binarydepFU"), 
                               names_to = "Time", values_to = "MINI_score")
long_data_MINI$Time[long_data_MINI$Time == "binarydepIN"] <- "Intake"
long_data_MINI$Time[long_data_MINI$Time == "depressie_MINI_TK"] <- "Post-treatment"
long_data_MINI$Time[long_data_MINI$Time == "binarydepFU"] <- "6-months FU"
long_data_MINI$Time <- factor(long_data_MINI$Time, levels = c("Intake", "Post-treatment", "6-months FU"))
#not as factor after recoding: long_data_MINI$geslacht <- factor(long_data_MINI$geslacht)
long_data_MINI <- as.data.frame(long_data_MINI)
long_data_MINI<- long_data_MINI[order(long_data_MINI$CIN, long_data_MINI$Time), ]


#-------------------------------------------------------------------------------
#Fitting model
#-------------------------------------------------------------------------------
#random slopes not successful: Error: number of observations (=779) < number of random effects (=852)
#for term (Time | CIN); the random-effects parameters are probably unidentifiable
#Time*geslacht*Age didnt work, therefore leave out the 3-way interaction
#Use Gaussian-Hermite quadrature, with 10 support points
glmm.1 <- glmer(MINI_score ~ Time*geslacht+ Time*Age + (1|CIN), data = long_data_MINI, 
                na.action = na.exclude, family =  "binomial", nAGQ = 10, 
                control = glmerControl(optimizer="bobyqa"))
coefs <- summary(glmm.1)$ coefficients[, "Estimate"]
summary(glmm.1)

#-------------------------------------------------------------------------------
#Testing Model assumptions:
#-------------------------------------------------------------------------------
#Normality assumption of pearson residuals is checked
qqnorm(residuals(glmm.1, type='pearson'))
#Variability of outcomes: Dev/df=868.4/769=1.13
#Thus, no signs of over/under-dispersion


#-------------------------------------------------------------------------------
#Significance testing
#-------------------------------------------------------------------------------
#Testing change over time
Lglmm_time <- rbind(c(0,1,rep(0,7)), c(0,0,1,rep(0,6)))
wald.test(Sigma= vcov(glmm.1), b = coefs, L = Lglmm_time)
#Testing interaction sex with time
Lglmm_sex <- rbind(c(rep(0,5),1, 0,0,0), c(rep(0,6), 1,0,0))
wald.test(Sigma= vcov(glmm.1), b = coefs, L = Lglmm_sex)
#Testing interaction age with time
Lglmm_age <- rbind(c(rep(0,7),1, 0), c(rep(0,8), 1))
wald.test(Sigma= vcov(glmm.1), b = coefs, L = Lglmm_age)
summary(glmm.1)$coefficients
#Testing change from post-treatment to follow-up
Lglmm_postFU <- rbind(c(0,1, -1, rep(0,6)))
wald.test(Sigma= vcov(glmm.1), b = coefs, L = Lglmm_postFU)

#Summary of coefficients in the model and significance tests individual terms/coefficients:
summary(glmm.1)$coefficients


#-------------------------------------------------------------------------------
#Marginal coefficients, OR's and probabilities
#-------------------------------------------------------------------------------
# approximating marginal coefficients instead of subject-specific: 
marginal_coef <- round(coefs/sqrt(1 + 0.346 * VarCorr(glmm.1)[[1]][1]), 3)
SEs <- summary(glmm.1)$coefficients[, "Std. Error"]
SE_marginal <- SEs/sqrt(1+ 0.346 * VarCorr(glmm.1)[[1]][1])
#ORs and CIs of ORs
OR<- exp(marginal_coef)
exp(marginal_coef - 1.96*SE_marginal)
exp(marginal_coef + 1.96*SE_marginal)
#Plotting the results for new recoded variables:
est_baseline <- marginal_coef[1]
est_TK <-marginal_coef[1] + marginal_coef[2] 
est_FU <- marginal_coef[1] + marginal_coef[3] 
#To probs:
prob_baseline <-1/(1+exp(-est_baseline))
prob_TK <- 1/(1+exp(-est_TK))
prob_FU <- 1/(1+exp(-est_FU))
probs <- c(prob_baseline, prob_TK, prob_FU)

#-------------------------------------------------------------------------------
#Plotting the results
#-------------------------------------------------------------------------------
dfprobs <- data.frame(probs = c(prob_baseline, 1-prob_baseline, prob_TK, 1-prob_TK, prob_FU, 1-prob_FU), 
                      label = rep(c("yes MINI", "no MINI"), 3), 
                      Time = c(rep("Intake",2), rep("Post-treatment",2), rep("6-months follow-up",2)))
dfprobs$Time <- factor(dfprobs$Time, levels = c("Intake", "Post-treatment", "6-months follow-up"))
ggplot(data=dfprobs, aes(Time, probs)) +
  geom_bar(stat="identity", position = "dodge", aes(fill= label)) + ylab("Probability") + scale_fill_manual(name = "MDD diagnosis", labels= c("no", "yes"), values = c("#00BFC4","#F8766D"))


