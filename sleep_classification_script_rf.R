args = commandArgs(trailingOnly = TRUE)

install.packages("data.table", repos = "http://cran.us.r-project.org")
install.packages("caTools",repos = "http://cran.us.r-project.org")
install.packages("caret",repos = "http://cran.us.r-project.org")
install.packages("pROC",repos = "http://cran.us.r-project.org")
install.packages("class",repos = "http://cran.us.r-project.org")
install.packages("randomForest",repos = "http://cran.us.r-project.org")
install.packages("tictoc",repos = "http://cran.us.r-project.org")
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
install.packages("e1071",repos = "http://cran.us.r-project.org")

library("data.table");library("caTools");library("caret");library("pROC");library("class");
library("randomForest");library("tictoc");library("tidyverse");library("e1071")

dir.create(args[1])
sink(paste0(args[1],"/log_rf.txt"),append = T)

tic()

print("preprocessing the data")
dat <- list.files(args[2], full.names = TRUE, recursive = TRUE) %>%
  map_df(read_csv) %>%
  mutate(clock_proxy = cos(time_stamp %% (24*60*60)/(60*60) * 15 * pi/180)) %>%
  mutate(classifier = if_else(sleep_state >= 1, 1, 0)) %>%
  select(activity_index,br,hr,clock_proxy,classifier)
dat <- na.omit(dat)
dat$classifier <-  factor(dat$classifier)
print("printing structure of data")
str(dat)
print("printing summary of data")
summary(dat)


######### Parameter combination 1: hr ###########

tic()
print("starting parameter combination 1: Heart_rate")
parameter_comb <- "hr";para_num = 1
df_rf <- select(dat, hr, classifier)

print("normalizing and splitting the data")
final.df_rf <- scale(df_rf[,1:length(df_rf)-1]) %>% cbind(df_rf[length(df_rf)])
final.df_rf <- na.omit(final.df_rf)
set.seed(101) 
split = sample.split(final.df_rf$classifier, SplitRatio = 0.75)
df_rf.train = subset(final.df_rf, split == TRUE) 
df_rf.test = subset(final.df_rf, split == FALSE) 

print("modelling with random forest of parameter combination 1")
model_rf <- randomForest(classifier ~ ., data = df_rf.train, ntree = 101)
if(para_num == 4){saveRDS(model_rf, paste0(args[1],"/",parameter_comb,"_model_rf.rds"));print("saving rds_rm")}
pred_rf <- predict(model_rf, df_rf.test)
accuracy_rf_hr <- (1 - mean(pred_rf != df_rf.test$classifier)) #To check the accuracy of our model
rf <- confusionMatrix(pred_rf, df_rf.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_rf_hr  <- roc(df_rf.test$classifier, as.numeric(pred_rf))
print("getting auc results")
auc_rf_hr <- auc(df_rf.test$classifier, as.numeric(pred_rf)) # Logistic Regression
print("generating sensitivity and specificity")
rf_hr <-as.data.frame(rf$byClass) #Logistic Regression
sensitiviy_rf_hr <- rf_hr["Sensitivity",]
specificity_rf_hr <- rf_hr["Specificity",]
toc()

######### Parameter combination 2: hr+ai ###########

tic()
print("starting parameter combination 2: Activity_index & Heart_rate")
parameter_comb <- "ai_hr";para_num = 2
df_rf <- select(dat, activity_index, hr, classifier)

print("normalizing and splitting the data")
final.df_rf <- scale(df_rf[,1:length(df_rf)-1]) %>% cbind(df_rf[length(df_rf)]) 
final.df_rf <- na.omit(final.df_rf)
set.seed(101) 
split = sample.split(final.df_rf$classifier, SplitRatio = 0.75)
df_rf.train = subset(final.df_rf, split == TRUE) 
df_rf.test = subset(final.df_rf, split == FALSE) 

print("modelling with random forest of parameter combination 2")
model_rf <- randomForest(classifier ~ ., data = df_rf.train)
if(para_num == 4){saveRDS(model_rf, paste0(args[1],"/",parameter_comb,"_model_rf.rds"));print("saving rds_rm")}
pred_rf <- predict(model_rf, df_rf.test)
accuracy_rf_hr_ai <- (1 - mean(pred_rf != df_rf.test$classifier)) #To check the accuracy of our model
rf <- confusionMatrix(pred_rf, df_rf.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_rf_hr_ai  <- roc(df_rf.test$classifier, as.numeric(pred_rf))
print("getting auc results")
auc_rf_hr_ai <- auc(df_rf.test$classifier, as.numeric(pred_rf)) # Logistic Regression
print("generating sensitivity and specificity")
rf_hr_ai <-as.data.frame(rf$byClass) #Logistic Regression
sensitiviy_rf_hr_ai <- rf_hr_ai["Sensitivity",]
specificity_rf_hr_ai <- rf_hr_ai["Specificity",]

toc()

######### Parameter combination 3: hr+ai+cp ###########

tic()
print("starting parameter combination 3: Activity_index & Heart_rate & clock_proxy")
parameter_comb <- "ai_hr_cp";para_num = 3
df_rf <- select(dat, activity_index, hr, clock_proxy, classifier)

print("normalizing and splitting the data")
final.df_rf <- scale(df_rf[,1:length(df_rf)-1]) %>% cbind(df_rf[length(df_rf)]) 
final.df_rf <- na.omit(final.df_rf)
set.seed(101) 
split = sample.split(final.df_rf$classifier, SplitRatio = 0.75)
df_rf.train = subset(final.df_rf, split == TRUE) 
df_rf.test = subset(final.df_rf, split == FALSE) 

print("modelling with random forest of parameter combination 3")
model_rf <- randomForest(classifier ~ ., data = df_rf.train)
if(para_num == 4){saveRDS(model_rf, paste0(args[1],"/",parameter_comb,"_model_rf.rds"));print("saving rds_rm")}
pred_rf <- predict(model_rf, df_rf.test)
accuracy_rf_hr_ai_cp <- (1 - mean(pred_rf != df_rf.test$classifier)) #To check the accuracy of our model
rf <- confusionMatrix(pred_rf, df_rf.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_rf_hr_ai_cp  <- roc(df_rf.test$classifier, as.numeric(pred_rf))
print("getting auc results")
auc_rf_hr_ai_cp <- auc(df_rf.test$classifier, as.numeric(pred_rf)) # Logistic Regression
print("generating sensitivity and specificity")
rf_hr_ai_cp <-as.data.frame(rf$byClass) #Logistic Regression
sensitiviy_rf_hr_ai_cp <- rf_hr_ai_cp["Sensitivity",]
specificity_rf_hr_ai_cp <- rf_hr_ai_cp["Specificity",]

toc()

######### Parameter combination 4: hr_ai_br ###########

tic()
print("starting parameter combination 4: Activity_index & Heart_rate & Breathing_rate")
parameter_comb <- "ai_hr_br";para_num = 3
df_rf <- select(dat, activity_index, hr, br, classifier)
print("normalizing and splitting the data")
final.df_rf <- scale(df_rf[,1:length(df_rf)-1]) %>% cbind(df_rf[length(df_rf)]) 
final.df_rf <- na.omit(final.df_rf)
set.seed(101) 
split = sample.split(final.df_rf$classifier, SplitRatio = 0.75)
df_rf.train = subset(final.df_rf, split == TRUE) 
df_rf.test = subset(final.df_rf, split == FALSE) 

print("modelling with random forest of parameter combination 4")
model_rf <- randomForest(classifier ~ ., data = df_rf.train)
if(para_num == 4){saveRDS(model_rf, paste0(args[1],"/",parameter_comb,"_model_rf.rds"));print("saving rds_rm")}
pred_rf <- predict(model_rf, df_rf.test)
accuracy_rf_hr_ai_br <- (1 - mean(pred_rf != df_rf.test$classifier)) #To check the accuracy of our model
rf <- confusionMatrix(pred_rf, df_rf.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_rf_hr_ai_br  <- roc(df_rf.test$classifier, as.numeric(pred_rf))
print("getting auc results")
auc_rf_hr_ai_br <- auc(df_rf.test$classifier, as.numeric(pred_rf)) # Logistic Regression
print("generating sensitivity and specificity")
rf_hr_ai_br <-as.data.frame(rf$byClass) #Logistic Regression
sensitiviy_rf_hr_ai_br <- rf_hr_ai_br["Sensitivity",]
specificity_rf_hr_ai_br <- rf_hr_ai_br["Specificity",]
toc()

######### Parameter combination 5: hr_ai_br_cp ###########


tic()
print("starting parameter combination 5: Activity_index & Heart_rate & clock_proxy & Breathing_rate")
parameter_comb <- "ai_hr_cp_br";para_num = 4
df_rf <- select(dat, activity_index, hr, clock_proxy, br, classifier)
print("normalizing and splitting the data")
final.df_rf <- scale(df_rf[,1:length(df_rf)-1]) %>% cbind(df_rf[length(df_rf)]) 
final.df_rf <- na.omit(final.df_rf)
set.seed(101) 
split = sample.split(final.df_rf$classifier, SplitRatio = 0.75)
df_rf.train = subset(final.df_rf, split == TRUE) 
df_rf.test = subset(final.df_rf, split == FALSE) 

print("modelling with random forest of parameter combination 5")
model_rf <- randomForest(classifier ~ ., data = df_rf.train)
if(para_num == 4){saveRDS(model_rf, paste0(args[1],"/",parameter_comb,"_model_rf.rds"));print("saving rds_rm")}
pred_rf <- predict(model_rf, df_rf.test)
accuracy_rf_hr_ai_cp_br <- (1 - mean(pred_rf != df_rf.test$classifier)) #To check the accuracy of our model
rf <- confusionMatrix(pred_rf, df_rf.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_rf_hr_ai_cp_br  <- roc(df_rf.test$classifier, as.numeric(pred_rf))
print("getting auc results")
auc_rf_hr_ai_cp_br <- auc(df_rf.test$classifier, as.numeric(pred_rf)) # Logistic Regression
print("generating sensitivity and specificity")
rf_hr_ai_cp_br <-as.data.frame(rf$byClass) #Logistic Regression
sensitiviy_rf_hr_ai_cp_br <- rf_hr_ai_cp_br["Sensitivity",]
specificity_rf_hr_ai_cp_br <- rf_hr_ai_cp_br["Specificity",]
toc()



print("saving results") 
values <- c(accuracy_rf_hr,accuracy_rf_hr_ai,accuracy_rf_hr_ai_cp,accuracy_rf_hr_ai_br,accuracy_rf_hr_ai_cp_br,
            auc_rf_hr,auc_rf_hr_ai,auc_rf_hr_ai_cp,auc_rf_hr_ai_br,auc_rf_hr_ai_cp_br,
            sensitiviy_rf_hr,sensitiviy_rf_hr_ai,sensitiviy_rf_hr_ai_cp,sensitiviy_rf_hr_ai_br,sensitiviy_rf_hr_ai_cp_br,
            specificity_rf_hr,specificity_rf_hr_ai,specificity_rf_hr_ai_cp,specificity_rf_hr_ai_br,specificity_rf_hr_ai_cp_br)
names(values) <- c("accuracy_rf_hr","accuracy_rf_hr_ai","accuracy_rf_hr_ai_cp","accuracy_rf_hr_ai_br","accuracy_rf_hr_ai_cp_br",
                   "auc_rf_hr","auc_rf_hr_ai","auc_rf_hr_ai_cp","auc_rf_hr_ai_br","auc_rf_hr_ai_cp_br",
                   "sensitiviy_rf_hr","sensitiviy_rf_hr_ai","sensitiviy_rf_hr_ai_cp","sensitiviy_rf_hr_ai_br","sensitiviy_rf_hr_ai_cp_br",
                   "specificity_rf_hr","specificity_rf_hr_ai","specificity_rf_hr_ai_cp","specificity_rf_hr_ai_br","specificity_rf_hr_ai_cp_br")
result <- as.data.frame(t(as.matrix(values)))
write.csv(result, paste0(args[1],"/values_rf.csv")) 

print("preparing the roc graph")
all.roc <- list("rf_HR" = roc_rf_hr, "rf_HR+AI" = roc_rf_hr_ai, "rf_HR+AI+CP" = roc_rf_hr_ai_cp,"rf_HR+AI+BR" = roc_rf_hr_ai_br,"rf_HR+AI+CP+BR" = roc_rf_hr_ai_cp_br)
gg <- ggroc(all.roc, size = 0.8,legacy.axes = TRUE) + # finally, plot graph using ggroc()
  theme_classic() +
  labs(title  = paste0("ROC graphs for the parameters combinations for linear regression"),
       x = "Fraction of wake scored as sleep(FPR)",
       y = "Fraction of sleep scored as sleep(TPR)",
       colour = "Classifiers") 
gg <- ggsave(paste0(args[1],"/roc_rf.png"))

sink()