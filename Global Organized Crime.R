#---- load data ----
setwd("C:/Users/Farah/Desktop/Graduation Project/")
library(readxl)
gp <- read_excel("C:/Users/Farah/Desktop/Graduation Project/gp.xlsx")
gp$Kids_rights <- NULL
max_val <- max(gp$FSI, na.rm = TRUE)
min_val <- min(gp$FSI, na.rm = TRUE)
gp$FSI <- (max_val + min_val) - gp$FSI #reversing 
max_val1 <- max(gp$IEP, na.rm = TRUE)
min_val1 <- min(gp$IEP, na.rm = TRUE)
gp$IEP <- (max_val1 + min_val1) - gp$IEP #reversing 
colMeans(is.na(gp)) #missing percentage
gp <- na.omit(gp)
names(gp)
gp1 <- as.data.frame(scale(gp[,-1]))
row.names(gp1) <- gp$Country
####outliers####
mahal_dist <- mahalanobis(gp1, colMeans(gp1), cov(gp1))
threshold <- qchisq(0.99, df = ncol(gp1))  
outliers <- mahal_dist > threshold
gp1$outlier <- ifelse(outliers, "Outlier", "Normal")
gp1[gp1$outlier == "Outlier", ] #view
gp2 <- gp1[, -13] #outliers dataset
#----Factor Analysis----
library(psych)
KMO(gp2[,-1])
cortest.bartlett(cor(gp2[,-1]), n = 150)
scree(gp2[,-1])
fa <- fa(gp2[,-1], fm="gls", nfactors = 4 , rotate = "oblimin", scores = "regression")
summary(fa)
fa$loadings
print(fa$communality)
scores <- as.data.frame(fa$scores)
scores$crime <- gp1$crime
names(scores) <- c("Governance_Quality","Political_Stability","Democratic_Participation","Globalized_Development","crime")
fa.diagram(fa)
fa_gls <- fa(gp2[,-1], fm = "gls", nfactors = 10)
g <- fa_gls$e.values
plot(g,col = "darkred",pch = 19 ,type = "b",ylab = "eigen values (reduced correlation matrix)", main = "Scree Plot" )
#----Linear Regression----
set.seed(123)
#load needed packages
library(lmtest)
library(caret)
library(MASS)
splitIndex <- createDataPartition(scores$crime, p = 0.7, list = FALSE)
train <- scores[splitIndex, ]
test <- scores[-splitIndex, ]
model <- lm(crime ~ ., data = train)
model_step <- stepAIC(model, direction = "both")
summary(model_step)
plot(residuals(model_step),pch =19 , col = "steelblue" , main = "Residuals Plot")
hist(residuals(model_step))
qqnorm(residuals(model_step)); qqline(residuals(model_step))
shapiro.test(residuals(model_step))
bptest(model_step)
dwtest(model_step)
car::vif(model_step)
#Identify leverage points 
cook_dist <- cooks.distance(model_step)
threshold <- 4 / nrow(train)
high_influence_points <- which(cook_dist > threshold)
cat("High influence points:\n")
print(high_influence_points)

#Removing leverage points
train_cleaned <- train[-high_influence_points, ]

# --- Final Model on Cleaned Training Data ---
model_cleaned <- lm(crime ~ ., data = train_cleaned)
model_final <- stepAIC(model_cleaned, direction = "both")
summary(model_final)
# --- Residual Diagnostics (After Cleaning) ---
plot(residuals(model_final),pch =19 , col = "steelblue" , main = "Residuals Plot")
hist(residuals(model_final)) #normality assumption
qqnorm(residuals(model_final)); qqline(residuals(model_final)) #normality assumption
shapiro.test(residuals(model_final)) #normality assumption
bptest(model_final) #constant variance 
dwtest(model_final) #No Autocorrelation
car::vif(model_final) #Multicolinearity assumption

# --- Levene’s Test for Homogeneity of Variance ---
residuals_model <- residuals(model_final)
fitted_values <- fitted(model_final)
car::leveneTest(residuals_model ~ cut(fitted_values, 4))
#
predicted1<- predict(model_final, newdata = train_cleaned)
actual1 <- train_cleaned$crime

predicted<- predict(model_final, newdata = test)
actual <- test$crime

# --- Plot Actual vs Predicted ---
plot(actual, predicted, col = "steelblue", pch = 19,
     xlab = "Actual Crime", ylab = "Predicted Crime",
     main = "Actual vs Predicted Crime (Test Set)")
abline(0, 1, col = "darkred", lwd = 2)
##############
evaluate_model <- function(actual, predicted, dataset_name){
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(actual - predicted))
  r2 <- cor(actual, predicted)^2
  
  cat("\n---", dataset_name, "---\n")
  cat("MSE:", round(mse, 3), "\n")
  cat("RMSE:", round(rmse, 3), "\n")
  cat("MAE:", round(mae, 3), "\n")
  cat("R-squared:", round(r2, 3), "\n")
}

evaluate_model(actual1,predicted1,"Training set")  

evaluate_model(actual,predicted,"Test set")  
library(GGally)
ggpairs(scores)
#---- Decision Tree----
# Load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)
library(Metrics)
#Detect and Remove Outliers from Training Set using Mahalanobis Distance
mahal_dist <- mahalanobis(train, colMeans(train), cov(train))
threshold <- qchisq(0.99, df = ncol(train))
outliers <- mahal_dist > threshold
train$outlier <- ifelse(outliers, "Outlier", "Normal")


# Remove Outliers
train_dt <- train[train$outlier == "Normal", ]
train_dt$outlier <- NULL  # Drop the outlier column
# Train Decision Tree Model
tree_model <- rpart(crime ~ ., data = train_dt, method = "anova")
rpart.plot(tree_model, main = "Decision Tree")

# Evaluate on Training Set
train_pred <- predict(tree_model, train_dt)
evaluate_model(train_dt$crime, train_pred, "Training Set")

# Apply Model to Test Set
test_pred <- predict(tree_model, test)

# Evaluate on Test Set
evaluate_model(test$crime, test_pred, "Test Set")
#  Handle Overfitting with Pruning
printcp(tree_model)  # View complexity parameter table
best_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]

# Prune the tree
pruned_tree <- prune(tree_model, cp = best_cp)
rpart.plot(pruned_tree, main = "Pruned Decision Tree")

# Re-evaluate after pruning
train_pruned_pred <- predict(pruned_tree, train_dt)
test_pruned_pred  <- predict(pruned_tree, test)
class(test_pruned_pred)
table(round(test_pruned_pred, 2))  
length(unique(predict(pruned_tree, train_dt)))

evaluate_model(train_dt$crime, train_pruned_pred, "Training Set (Pruned)")
evaluate_model(test$crime, test_pruned_pred, "Test Set (Pruned)")
#May be will NOT be INCLUDED
plot(test$crime, test_pruned_pred,
     main = "Actual vs Predicted (Test Set)",
     xlab = "Actual Crime Rate",
     ylab = "Predicted Crime Rate",
     pch = 19, col = "steelblue")
abline(0, 1, col = "darkred", lwd = 2)

#####part2####
#####cluster analysis####
library(readxl)
oc <- read_excel("global_oc_index.xlsx", 
                 sheet = "2023_dataset")
crime <- oc[,c(6:20,22:26)]
row.names(crime) <- oc$Country
any(is.na(crime))
crime <- as.data.frame(scale(crime))
boxplot(crime,col = "darkred")
mahal_dist <- mahalanobis(crime, colMeans(crime), cov(crime))
threshold <- qchisq(0.99, df = ncol(crime))  
outliers <- mahal_dist > threshold
crime$outlier <- ifelse(outliers, "Outlier", "Normal")
crime[crime$outlier == "Outlier", ] #view
crime <- crime[!outliers, ]
crime <- crime[,-21]
library(factoextra)
library(cluster)
distance_matrix <- dist(crime)
fit <- hclust(distance_matrix, method = "ward.D")
fit
# Dendoregram 
plot(fit)
rect.hclust(fit,k=3,border = "darkred")
# Elbow plot for determining the optimal number of clusters
library(factoextra)
set.seed(123) 
data <- scale(crime) 
fviz_nbclust(data, kmeans, method = "wss") + 
  theme_minimal() + 
  labs(title = "Elbow Plot")
kmeans3 <-  kmeans( crime ,centers = 3,nstart = 10)
crime$cluster <- kmeans3$cluster
library(openxlsx)
x <- kmeans3$centers
write.xlsx(x,"Kmeans.xlsx")
# Compute silhouette scores
sil_scores <- silhouette(kmeans3$cluster, distance_matrix)

# Print average silhouette width
avg_sil_width <- mean(sil_scores[, 3])
cat("Average silhouette width:", avg_sil_width, "\n")

# Plot silhouette plot
plot(sil_scores, col = c("darkred"), border = NA, main = "silhouette plot")

library(rnaturalearth)
library(rnaturalearthdata)


# 1. 
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. 
cluster_data <- data.frame(
  name = row.names(crime),
  cluster = crime$cluster
)

# 3. 
world_clusters <- left_join(world, cluster_data, by = c("name"))

# 4. 
ggplot(world_clusters) +
  geom_sf(aes(fill = factor(cluster)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("gold", "tomato", "darkred"),
                    na.value = "gray90",
                    name = "Cluster") +
  labs(title = "Global Clustering of Organized Crime Levels") +
  theme_minimal()

#####descrimenation analysis####
MVN::mvn(crime[, -which(names(crime) == "cluster")], mvnTest = "mardia")
crime$cluster <- as.factor(crime$cluster)
apply(crime[, -which(names(crime) == "cluster")], 2, shapiro.test)
crime_numeric <- crime[, sapply(crime, is.numeric)]
biotools::boxM(crime_numeric, crime$cluster)
library(caret)
set.seed(123)  
splitIndex <- createDataPartition(crime$cluster, p = 0.7, list = FALSE)
train <- crime[splitIndex, ]
test <- crime[-splitIndex, ]
dim(train)
dim(test)
library(mda)
fit <- fda(train$cluster ~ ., data = train)
summary(fit)
fit$confusion
fit$percent.explained
actual <- test$cluster
predictions <- predict(fit, newdata = test)
library(caret)
confusionMatrix(actual,predictions)
