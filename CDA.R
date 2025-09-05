library(readxl)
F00013184_WVS_Wave_7_Ecuador_Excel_v5_0 <- read_excel("C:/Users/Farah/Downloads/F00013184-WVS_Wave_7_Ecuador_Excel_v5.0.xlsx")
data1 <- F00013184_WVS_Wave_7_Ecuador_Excel_v5_0
sat <- as.factor(data1$`Q49: Satisfaction with your life`)
tr <- as.factor(data1$`Q58: Trust: Your family`)              
tr1 <- factor(ifelse(tr%in% c(1,2),1,2))
levels(sec)
table(s)
data1$sat1 <- factor(ifelse(sat%in% c(1,2,3,4,5),0,1))               
sat1 <- factor(ifelse(sat%in% c(1,2,3,4,5),0,1))               

table(sat1)
trustsat <- table(tr1,sat1)

sex <- as.factor(data1$`Q260: Sex`)
sexsat <- table(sex ,sat1)
age <- as.factor(data1$`X003R2: Age recoded (3 intervals)`)
agesat <- table(age,sat1)
mar <- as.factor(data1$`Q273: Marital status`)
mar1 <- factor(ifelse(mar %in% c(1,2,3,4,5),1,2))
marsat <- table(mar1,sat1)
ur <- as.factor(data1$`H_URBRURAL: Urban-Rural`)
urbansat <- table(ur,sat1)
emp <- as.factor(data1$`Q279: Employment status`)
emp1 <- factor(ifelse(emp%in%c(1,2,3),1,2))
empsat <- table(emp1,sat1)
edu <- as.factor(data1$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`)
#for (i in 1:length(edu)) {
 # if (edu[i] == -5 || edu[i] == -2) {
  #  edu[i] <- 2
 
# }
#}
edu[edu == -5 | edu == -2] <- 2
table(edu)
# إزالة القيم السلبية من المتغير edu
edu_clean <- edu[edu > 0]
# Convert edu to numeric and filter out negative values
edu_clean <- as.numeric(as.character(edu))
edu_clean <- edu_clean[edu_clean > 0]

# Display the table after cleaning
table(edu_clean)

# عرض التوزيع بعد إزالة القيم السلبية
table(edu_clean)
edusat <- table(edu,sat1)
inc <- as.factor(data1$`Q288R: Income level (Recoded)`)
inc[inc == -5 |inc == -2 |inc == -1] <- 2
incsat <- table(inc,sat1)
incsat1 <- data.frame(
    `0` = c(93, 99, 18),  
    `1` = c(237, 628, 125) 
  )

rownames(incsat1) <- c("1", "2", "3")
faith <- data1$`Q165: Believe in: God`
faith[faith== -5|faith == -2]<-2
table(faith,sat1)
table(tr1,sat1,faith) #3way
 chief <- data1$`Q285: Are you the chief wage earner in your house`
table(chief)
chief[chief == -5 |chief == -2 |chief == -1] <- 1
empchief <- table(emp1,sat1,chief) #3way
empedu <- table(emp1,sat1,edu_clean)
empedu
mantelhaen.test(empedu)
edusat1 <- data.frame(`0` = c(86, 91, 33), `1` = c(291, 429, 270))
rownames(edusat1) <- c("1", "2", "3")



chi=chisq.test(empedu, correct = FALSE) #correct means correction factor
chi$expected
chi

fisher.test(marsat)

#Gamma measure
DescTools::GoodmanKruskalGamma(agesat, conf.level=0.95)

DescTools::OddsRatio(empsat) #the one we can calculate for it odds ratio
DescTools::OddsRatio(trustsat) #the one we can calculate for it odds ratio
# modeling
logmodel<-glm(sat1~edu_clean+sex+mar1+emp1+age+ur+tr1+free+faith+chief+pol+suc,binomial(link="logit"),data1)
summary(logmodel)
anova(logmodel)
anova(logmodel)pol
predictedlogit <- predict(logmodel, data1)
summary(predictedlogit)
car::vif(logmodel)
predictedprob<-predict(logmodel, data1,type="response")
predictclass <- ifelse(predictedprob > 0.8,1,0)
library(pROC)
roc_score=roc(logmodel)
plot(roc_score ,main ="ROC curve – Logistic Regression")
roc_score
cutoff <- coords( roc_score, "best")
cutoff


library(MASS)
backward_model <- stepAIC(logmodel, direction = "backward")
summary(backward_model)

free <- as.factor(data1$`Q149: Freedom and Equality - Which more important`)
table(free)
free <- ifelse(free== -2|free==-1|free == 1,1,2)
pol <- as.factor(data1$`Q199: Interest in politics`)
pol <- ifelse(pol==1|pol ==2 , 1,2)
table(pol)
suc <- as.factor(data1$`Q110: Success: hard work vs luck`)
table(suc)
suc <- ifelse(suc == 1|suc == 2|suc == 3 |suc == 4 |suc == 5 , "hardwork" , "luck")
