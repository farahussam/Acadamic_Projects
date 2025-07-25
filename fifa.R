#####load packages####
library(readr)
library(dplyr)
library(ggplot2)
library(wbacon)
library(psych)
library(DescTools)
library(openxlsx) 
library(e1071)
library(caTools)
library(class)
library(MASS)
library(pROC)
#####load data####
fifa <- read.csv("fifa_players.csv")
#####missing values####
dim(fifa) #dataset dimensions
colSums(is.na(fifa)) #count missing values in each column
colMeans(is.na(fifa))
fifa <- fifa[,-c(19:22)] #delete columns with 95% missing
fifa <- na.omit(fifa) #remove missing values
dim(fifa) #check dataset dimensions after removing the missing values
#####check outliers#####
w <- wBACON(as.data.frame(fifa[,c(19:47)]))
which(is_outlier(w))
summary(w)
#####typos correction####
fifa$body_type[fifa$body_type %in% c("Neymar", "PLAYER_BODY_TYPE_25", "C. Ronaldo")] <- "Lean"
fifa$body_type[fifa$body_type %in% c("Messi", "Courtois")] <- "Normal"
fifa$body_type[fifa$body_type %in% c("Akinfenwa", "Shaqiri")] <- "Stocky"
fifa$body_type<- as.factor(fifa$body_type)
levels(fifa$body_type)
#####simplifying 'positions' by recategorizing specific positions######
fifa$positions <- ifelse(grepl("CAM|RM|LM|CDM", fifa$positions), "CM", fifa$positions)
fifa$positions <- ifelse(grepl("CF|RW|LW", fifa$positions), "ST", fifa$positions)
fifa$positions <- ifelse(grepl("LB|RB|LWB|RWB|GK", fifa$positions), "CB", fifa$positions)
fifa$positions <- recode(fifa$positions, "CB,ST,CM" = "CM", "CB,ST" = "CM", "ST,CB" = "CM")
fifa$positions <- recode(fifa$positions, "CM,CB" = "CB", "CB,CM" = "CM")
fifa$positions <- recode(fifa$positions, "ST,CM" = "ST", "CM,ST" = "ST")
fifa$positions <- as.factor(fifa$positions)
levels(fifa$positions)
#####myfunction####
# list of countries and their classification by continents
countries_to_continents <- list(
  "Africa" = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
               "Cameroon", "Cape Verde", "Central African Rep.", "Chad", "Comoros", 
               "Congo", "DR Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
               "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", 
               "Guinea Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", 
               "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
               "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "São Tomé & Príncipe", 
               "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
               "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", 
               "Zimbabwe"),
  "Asia" = c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", 
             "Brunei", "Cambodia", "China PR", "Cyprus", "Hong Kong","Georgia", "India", "Indonesia", 
             "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan","Korea Republic","Korea DPR",
             "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", 
             "North Korea", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", 
             "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria", "Tajikistan", 
             "Thailand", "Timor-Leste", "Turkey", "Turkmenistan", "United Arab Emirates", 
             "Uzbekistan", "Vietnam", "Yemen"),
  "Europe" = c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia Herzegovina", 
               "Bulgaria", "Croatia", "Czech Republic", "Denmark", "England", "Estonia", 
               "Faroe Islands", "Finland", "France", "FYR Macedonia", "Germany", "Greece", 
               "Hungary", "Iceland", "Republic of Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", 
               "Lithuania", "Luxembourg", "Malta", "Moldova", "Montenegro", "Netherlands","New Caledonia" ,
               "Northern Ireland", "Norway", "Poland", "Portugal", "Romania", "Russia", 
               "Scotland", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
               "Turkey", "Ukraine", "Wales"),
  "North America" = c("Antigua & Barbuda", "Bahamas", "Barbados", "Belize", "Bermuda", 
                      "Canada", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", 
                      "El Salvador", "Grenada", "Guatemala", "Guam","Haiti", "Honduras", "Jamaica", 
                      "Mexico", "Montserrat","Nicaragua", "Panama", "St Kitts Nevis", "St Lucia", 
                      "St Vincent & Grenadines", "Trinidad & Tobago", "United States"),
  "South America" = c("Argentina", "Bolivia", "Brazil", "Chile", "Curacao","Colombia", "Ecuador", 
                      "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"),
  "Oceania" = c("Australia", "Fiji", "New Zealand", "Papua New Guinea", "Samoa", 
                "Solomon Islands", "Tonga", "Vanuatu")
)

# function to set continent based on country 
myfunction <- function(country) {
  for (continent in names(countries_to_continents)) {
    if (country %in% countries_to_continents[[continent]]) {
      return(continent)
    }
  }
  return("Unknown") 
}
fifa$continent <- sapply(fifa$nationality, myfunction)
head(fifa)
fifa$continent<- as.factor(fifa$continent)
levels(fifa$continent)
sum(fifa$continent=="Unknown")
#####numeric measures####
Desc(fifa %>% select_if(is.numeric))
desc <- describe(fifa %>% select_if(is.numeric))
desc
write.xlsx(desc , file = "data1fifa2")
#####
row.names(fifa) <- paste0(fifa$name,c(1:nrow(fifa)))
fifa1 <- fifa[,c(4,5,6,7,9,11,12,13,14,17,48)]
#####plots####
#####age vs overall rating#####
ggplot(fifa1,aes(x=age, y=overall_rating ))+
  geom_point()+
  labs(title = "Age vs Overall rating")
#####mean age by continent
fifa1 %>%
  group_by(continent) %>%
  summarise(Mean_Age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(x = continent, y = Mean_Age, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "mean age by continent",
       x = "continent",
       y = "Mean_Age") +
  theme_minimal()
#####overall rating by continent
ggplot(fifa1,aes(x=continent,y=overall_rating))+
  geom_boxplot(fill="darkred",color="black")
#####rating boxplot~positions####
ggplot(fifa1,aes(x=positions,y=overall_rating))+
  geom_boxplot(fill="orange",color="black")
#####histograms####
par(mfrow = c(3,2))
hist(fifa1$age, xlab= "Age" ,col = "red",breaks = 20,main = "histogram of Age") 
hist(fifa1$height_cm,xlab= "Height" , col = "red" ,main =  "histogram of height") 
hist(fifa1$weight_kgs,xlab= " Weight" , col = "white",breaks = 20,main =  "histogram of Weight") 
hist(fifa1$overall_rating,xlab= "overall rating" , col = "white",breaks= 20,main =  "histogram of Overall rating") 
hist(fifa1$value_euro,xlab= "Value" , col = "black",main =  "histogram of Value")
hist(fifa1$wage_euro,xlab= "wage" , col = "black",main =  "histogram of Wage")
dev.off()
#####logistic regression####
#####create variable rating####
fifa1$rating <- as.factor(ifelse(fifa1$overall_rating >= 65 , "high" , "low"))
head(fifa1)
levels(fifa1$rating)
#####split dataset into train and test####
set.seed(123)
data_train <- as.data.frame(sample_frac(fifa1,.70))
data_test <- as.data.frame(anti_join(fifa1,data_train))

data_train$rating <- ifelse(data_train$rating == "high", 1, 0)
data_test$rating <- ifelse(data_test$rating == "high", 1, 0)
#samples are almost balanced
print("Proportion of classes in training data:")
print(prop.table(table(data_train$rating)))
print("Proportion of classes in testing data:")
print(prop.table(table(data_test$rating)))

logmodel <- glm(rating ~ positions+age+value_euro  + height_cm  + continent + international_reputation.1.5. + body_type + weight_kgs + preferred_foot, 
                family = binomial(link = "logit"), 
                data = data_train)
car::vif(logmodel) #check multicollinearity

logmodel1<- glm(rating ~ positions + height_cm  + continent + international_reputation.1.5. + body_type + weight_kgs + preferred_foot, 
                family = binomial(link = "logit"), 
                data = data_train)
stepwise_model <- stepAIC(logmodel1, direction = "both") #model building
summary(stepwise_model)
#goodness of fit
#ROC curve
y_pred <- predict(stepwise_model, newdata = data_test, type = "response")
roc_score <- roc(data_test$rating, y_pred)
plot(roc_score, main = "ROC curve – Logistic Regression", col = "green4")
print(paste("AUC:", roc_score$auc))
best_cutoff <- coords(roc_score, "best", ret = "threshold", best.method = "youden")

predict_class <- ifelse(y_pred > best_cutoff, 1, 0)
#confusion matrix
confusion_matrix <- table(Predicted = predict_class, Actual = data_test$rating)
print(confusion_matrix)
#calculate accuracy
accuracy <- mean(predict_class == data_test$rating)
print(paste("Accuracy:", accuracy))
#R2
pscl::pR2(stepwise_model)["McFadden"]
#####KNN#####
#####prepare dataset to apply KNN####
set.seed(123)
fifaknn <- as.data.frame(scale(fifa[,c(23:47)]))
knn_train <- as.data.frame(sample_frac(fifaknn,.70))
knn_test <- as.data.frame(anti_join(fifaknn,knn_train))
set_train <- as.data.frame(fifa[,c(7,23:47)])
#####split dataset into train and test####
set.seed(123)
split <- sample.split(fifa, SplitRatio = 0.7)
train_set <- subset(fifa, split == "TRUE")
test_set <- subset(fifa, split == "FALSE")

# Feature Scaling
train_scale <- as.data.frame(scale(train_set[,23:47]))
test_scale <- as.data.frame(scale(test_set[,23:47]))

#####apply knn####
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_set$positions,
                      k = 13)
classifier_knn
#####determine K####
misClassError<-c()
for(i in 1:15){classifier_knn <- knn(train = train_scale,
                                     test = test_scale,
                                     cl = train_set$positions,
                                     k = i);
misClassError[i] <- mean(classifier_knn != test_set$positions)}
misClassError
#####goodness of fit####
# Confusion Matrix
cm <- table(test_set$positions, classifier_knn)
cm
# Calculate out of Sample error
misClass <- mean(classifier_knn != test_set$positions)
print(paste('Accuracy =', 1-misClass))
MCE_df <- data.frame(K = 1:15, MCE = misClassError)
ggplot(accuracy_df, aes(x = K, y = MCE)) +
  geom_line(color = "green4", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Miss classification Error vs. K",
       x = "Number of Neighbors (K)",
       y = "MissClassError") +
  theme_minimal()

