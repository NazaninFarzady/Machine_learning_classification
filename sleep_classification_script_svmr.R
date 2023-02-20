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
sink(paste0(args[1],"/log_svmR.txt"),append = T)

tic()

print("preprocessing the data")
dat <- list.files(args[2], full.names = TRUE, recursive = TRUE) %>%
  map_df(read_csv) %>%
  mutate(classifier = if_else(sleep_state >= 1, 1, 0)) %>%
  select(activity_index,br,hr,time_stamp,classifier)
dat$clock_proxy <- cos(dat$time_stamp %% (24*60*60)/(60*60) * 15 * pi/180)
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
df_svmr <- select(dat, hr, classifier)

print("normalizing and splitting the data")
final.df_svmr <- scale(df_svmr[,1:length(df_svmr)-1]) %>% cbind(df_svmr[length(df_svmr)]) 
final.df_svmr <- na.omit(final.df_svmr)
set.seed(101) 
split = sample.split(final.df_svmr$classifier, SplitRatio = 0.75)
df_svmr.train = subset(final.df_svmr, split == TRUE) 
df_svmr.test = subset(final.df_svmr, split == FALSE) 

print("modelling with support vector machine of parameter combination 1")
model_svmR <- svm(formula = classifier ~ ., data = df_svmr.train)
if(para_num == 4){saveRDS(model_svmR, paste0(args[1],"/",parameter_comb,"_model_svmR.rds"));print("saving rds_svm")}
svm_predR <- predict(model_svmR, newdata = df_svmr.test[-(para_num+1)])
accuracy_svmR_hr <- (1 - mean(svm_predR != df_svmr.test$classifier))
svmR <- confusionMatrix(svm_predR, df_svmr.test$classifier, positive = "1", mode = "everything")


print("plotting roc curves")
roc_svmR_hr  <- roc(df_svmr.test$classifier, as.numeric(svm_predR))
print("getting auc results")
auc_svmR_hr <- auc(df_svmr.test$classifier, as.numeric(svm_predR)) # Logistic Regression
print("generating sensitivity and specificity")
svmR_hr <-as.data.frame(svmR$byClass) #Logistic Regression
sensitiviy_svmR_hr <- svmR_hr["Sensitivity",]
specificity_svmR_hr <- svmR_hr["Specificity",]
toc()

######### Parameter combination 2: hr+ai ###########

tic()
print("starting parameter combination 2: Activity_index & Heart_rate")
parameter_comb <- "ai_hr";para_num = 2
df_svmr <- select(dat, activity_index, hr, classifier)

print("normalizing and splitting the data")
final.df_svmr <- scale(df_svmr[,1:length(df_svmr)-1]) %>% cbind(df_svmr[length(df_svmr)]) 
final.df_svmr <- na.omit(final.df_svmr)
set.seed(101) 
split = sample.split(final.df_svmr$classifier, SplitRatio = 0.75)
df_svmr.train = subset(final.df_svmr, split == TRUE) 
df_svmr.test = subset(final.df_svmr, split == FALSE) 

print("modelling with support vector machine of parameter combination 2")
model_svmR <- svm(formula = classifier ~ ., data = df_svmr.train)
if(para_num == 4){saveRDS(model_svmR, paste0(args[1],"/",parameter_comb,"_model_svmR.rds"));print("saving rds_svm")}
svm_predR <- predict(model_svmR, newdata = df_svmr.test[-(para_num+1)])
accuracy_svmR_hr_ai <- (1 - mean(svm_predR != df_svmr.test$classifier))
svmR <- confusionMatrix(svm_predR, df_svmr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_svmR_hr_ai  <- roc(df_svmr.test$classifier, as.numeric(svm_predR))
print("getting auc results")
auc_svmR_hr_ai <- auc(df_svmr.test$classifier, as.numeric(svm_predR)) # Logistic Regression
print("generating sensitivity and specificity")
svmR_hr_ai <-as.data.frame(svmR$byClass) #Logistic Regression
sensitiviy_svmR_hr_ai <- svmR_hr_ai["Sensitivity",]
specificity_svmR_hr_ai <- svmR_hr_ai["Specificity",]

toc()

######### Parameter combination 3: hr+ai+cp ###########

tic()
print("starting parameter combination 3: Activity_index & Heart_rate & clock_proxy")
parameter_comb <- "ai_hr_cp";para_num = 3
df_svmr <- select(dat, activity_index, hr, clock_proxy, classifier)

print("normalizing and splitting the data")
final.df_svmr <- scale(df_svmr[,1:length(df_svmr)-1]) %>% cbind(df_svmr[length(df_svmr)]) 
final.df_svmr <- na.omit(final.df_svmr)
set.seed(101) 
split = sample.split(final.df_svmr$classifier, SplitRatio = 0.75)
df_svmr.train = subset(final.df_svmr, split == TRUE) 
df_svmr.test = subset(final.df_svmr, split == FALSE) 

print("modelling with support vector machine of parameter combination 3")
model_svmR <- svm(formula = classifier ~ ., data = df_svmr.train)
if(para_num == 4){saveRDS(model_svmR, paste0(args[1],"/",parameter_comb,"_model_svmR.rds"));print("saving rds_svm")}
svm_predR <- predict(model_svmR, newdata = df_svmr.test[-(para_num+1)])
accuracy_svmR_hr_ai_cp <- (1 - mean(svm_predR != df_svmr.test$classifier))
svmR <- confusionMatrix(svm_predR, df_svmr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_svmR_hr_ai_cp  <- roc(df_svmr.test$classifier, as.numeric(svm_predR))
print("getting auc results")
auc_svmR_hr_ai_cp <- auc(df_svmr.test$classifier, as.numeric(svm_predR)) # Logistic Regression
print("generating sensitivity and specificity")
svmR_hr_ai_cp <-as.data.frame(svmR$byClass) #Logistic Regression
sensitiviy_svmR_hr_ai_cp <- svmR_hr_ai_cp["Sensitivity",]
specificity_svmR_hr_ai_cp <- svmR_hr_ai_cp["Specificity",]

toc()

######### Parameter combination 4: hr_ai_br ###########

tic()
print("starting parameter combination 4: Activity_index & Heart_rate & Breathing_rate")
parameter_comb <- "ai_hr_br";para_num = 3
df_svmr <- select(dat, activity_index, hr, br, classifier)
print("normalizing and splitting the data")
final.df_svmr <- scale(df_svmr[,1:length(df_svmr)-1]) %>% cbind(df_svmr[length(df_svmr)]) 
final.df_svmr <- na.omit(final.df_svmr)
set.seed(101) 
split = sample.split(final.df_svmr$classifier, SplitRatio = 0.75)
df_svmr.train = subset(final.df_svmr, split == TRUE) 
df_svmr.test = subset(final.df_svmr, split == FALSE) 

print("modelling with support vector machine of parameter combination 4")
model_svmR <- svm(formula = classifier ~ ., data = df_svmr.train)
if(para_num == 4){saveRDS(model_svmR, paste0(args[1],"/",parameter_comb,"_model_svmR.rds"));print("saving rds_svm")}
svm_predR <- predict(model_svmR, newdata = df_svmr.test[-(para_num+1)])
accuracy_svmR_hr_ai_br <- (1 - mean(svm_predR != df_svmr.test$classifier))
svmR <- confusionMatrix(svm_predR, df_svmr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_svmR_hr_ai_br  <- roc(df_svmr.test$classifier, as.numeric(svm_predR))
print("getting auc results")
auc_svmR_hr_ai_br <- auc(df_svmr.test$classifier, as.numeric(svm_predR)) # Logistic Regression
print("generating sensitivity and specificity")
svmR_hr_ai_br <-as.data.frame(svmR$byClass) #Logistic Regression
sensitiviy_svmR_hr_ai_br <- svmR_hr_ai_br["Sensitivity",]
specificity_svmR_hr_ai_br <- svmR_hr_ai_br["Specificity",]
toc()

######### Parameter combination 5: hr_ai_br_cp ###########


tic()
print("starting parameter combination 5: Activity_index & Heart_rate & clock_proxy & Breathing_rate")
parameter_comb <- "ai_hr_cp_br";para_num = 4
df_svmr <- select(dat, activity_index, hr, clock_proxy, br, classifier)
print("normalizing and splitting the data")
final.df_svmr <- scale(df_svmr[,1:length(df_svmr)-1]) %>% cbind(df_svmr[length(df_svmr)]) 
final.df_svmr <- na.omit(final.df_svmr)
set.seed(101) 
split = sample.split(final.df_svmr$classifier, SplitRatio = 0.75)
df_svmr.train = subset(final.df_svmr, split == TRUE) 
df_svmr.test = subset(final.df_svmr, split == FALSE) 

print("modelling with support vector machine of parameter combination 5")
model_svmR <- svm(formula = classifier ~ ., data = df_svmr.train)
if(para_num == 4){saveRDS(model_svmR, paste0(args[1],"/",parameter_comb,"_model_svmR.rds"));print("saving rds_svm")}
svm_predR <- predict(model_svmR, newdata = df_svmr.test[-(para_num+1)])
accuracy_svmR_hr_ai_cp_br <- (1 - mean(svm_predR != df_svmr.test$classifier))
svmR <- confusionMatrix(svm_predR, df_svmr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_svmR_hr_ai_cp_br  <- roc(df_svmr.test$classifier, as.numeric(svm_predR))
print("getting auc results")
auc_svmR_hr_ai_cp_br <- auc(df_svmr.test$classifier, as.numeric(svm_predR)) # Logistic Regression
print("generating sensitivity and specificity")
svmR_hr_ai_cp_br <-as.data.frame(svmR$byClass) #Logistic Regression
sensitiviy_svmR_hr_ai_cp_br <- svmR_hr_ai_cp_br["Sensitivity",]
specificity_svmR_hr_ai_cp_br <- svmR_hr_ai_cp_br["Specificity",]
toc()



print("saving results") 
values <- c(accuracy_svmR_hr,accuracy_svmR_hr_ai,accuracy_svmR_hr_ai_cp,accuracy_svmR_hr_ai_br,accuracy_svmR_hr_ai_cp_br,
            auc_svmR_hr,auc_svmR_hr_ai,auc_svmR_hr_ai_cp,auc_svmR_hr_ai_br,auc_svmR_hr_ai_cp_br,
            sensitiviy_svmR_hr,sensitiviy_svmR_hr_ai,sensitiviy_svmR_hr_ai_cp,sensitiviy_svmR_hr_ai_br,sensitiviy_svmR_hr_ai_cp_br,
            specificity_svmR_hr,specificity_svmR_hr_ai,specificity_svmR_hr_ai_cp,specificity_svmR_hr_ai_br,specificity_svmR_hr_ai_cp_br)
names(values) <- c("accuracy_svmR_hr","accuracy_svmR_hr_ai","accuracy_svmR_hr_ai_cp","accuracy_svmR_hr_ai_br","accuracy_svmR_hr_ai_cp_br",
                   "auc_svmR_hr","auc_svmR_hr_ai","auc_svmR_hr_ai_cp","auc_svmR_hr_ai_br","auc_svmR_hr_ai_cp_br",
                   "sensitiviy_svmR_hr","sensitiviy_svmR_hr_ai","sensitiviy_svmR_hr_ai_cp","sensitiviy_svmR_hr_ai_br","sensitiviy_svmR_hr_ai_cp_br",
                   "specificity_svmR_hr","specificity_svmR_hr_ai","specificity_svmR_hr_ai_cp","specificity_svmR_hr_ai_br","specificity_svmR_hr_ai_cp_br")
result <- as.data.frame(t(as.matrix(values)))
write.csv(result, paste0(args[1],"/values_svmR.csv")) 

print("preparing the roc graph")
all.roc <- list("svmR_HR" = roc_svmR_hr, "svmR_HR+AI" = roc_svmR_hr_ai, "svmR_HR+AI+CP" = roc_svmR_hr_ai_cp,"svmR_HR+AI+BR" = roc_svmR_hr_ai_br,"svmR_HR+AI+CP+BR" = roc_svmR_hr_ai_cp_br)
gg <- ggroc(all.roc, size = 0.8,legacy.axes = TRUE) + # finally, plot graph using ggroc()
  theme_classic() +
  labs(title  = paste0("ROC graphs for the parameters combinations for linear regression"),
       x = "Fraction of wake scored as sleep(FPR)",
       y = "Fraction of sleep scored as sleep(TPR)",
       colour = "Classifiers") 
gg <- ggsave(paste0(args[1],"/roc_svmR.png"))

sink()