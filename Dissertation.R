# Research Dissertation ####

#H1: FWP positively influences organisational productivity, if employee motivation is enhanced and absenteeism is reduced.
#H1a: Adoption of flexible working practices (FWPs) positively affect the reduction in employee turnover.
#H1b: FWPs has a positive effect on employee motivation.
#H2: Employee motivation influences organisational productivity.
#H3: Moderate levels of employee turnover, are positively associated with organisational productivity, and provided there is flexibility arrangements.



# call library ####
library(readxl) 
library(ggplot2)
library(dplyr)
library(corrplot) # For graph of collinearity 
library(tidyr)
library(tidyverse)
library(skimr)
library(lmtest)    # For running regression models
library(sjPlot)    # For plotting regression results
library(broom) 
library(haven)
library(randomForest)
library(glmnet)
library(patchwork)
library(nnet)
library(scales)
library(RColorBrewer)
library(stargazer)
library(MASS)
library(rpart.plot)
library(car)
library(caret)    #for most statistical learning models
library(rpart)    #for decision trees
library(rattle)   #to visualize decision trees

#set working directory####
setwd("C:/Users/Home/OneDrive - Bournemouth University/Research Project 23.24/AdelekeResearch")


# load Raw data ####
RawData <- read_dta("wlbs_2013_employer_survey.dta")

# Data Restriction ####
WLB4 <- RawData %>% filter(A4 == 1, A6 != -1)

# Variables selection #####

WLB4 <- subset(WLB4, select = c(A13_1, A13_2, A13_3, 
                    A13_4, A13_5, S3, A6,B11, C1_A, C1_B,C1_D, 
                    C1_M, H6, H1_2, H1_3, H1_4, H1_6, H2_2,K1))


# Data Transformation ##########
WLB4 <- WLB4 %>% mutate(PartTime = ifelse(A13_1 == 1, 1, 0),
                                  WorkNight = ifelse(A13_2 == 1, 1, 0),
                                  DifWorkShift = ifelse(A13_3 == 1, 1, 0),
                                  ZeroHours = ifelse(A13_4 == 1, 1, 0),
                                  ThirtyPartTime = ifelse(A13_5 == 1, 1, 0),
                                  JobTitle = ifelse(S3 == 1, "HumanResources",
                                                ifelse(S3 == 2, "PersonnelMgr",
                                                ifelse(S3 == 3, "TrainingMgr",
                                                ifelse(S3 == 4, "FinAccMgr",
                                                ifelse(S3 == 5, "GeneralMgr",
                                                ifelse(S3 == 6, "Owner",
                                                ifelse(S3 == 7, "MD/CEO",
                                                ifelse(S3 == 8, "Director",
                                                ifelse(S3 == 9, "Other Manager",
                                                ifelse(S3 == 10, "PA",
                                                ifelse(S3 == 11, "Admin",
                                                ifelse(S3 == 12, "HeadTeacher",
                                                       "others")))))))))))),
                             Policies = ifelse(B11 == 1, 1, 0),
                             BizOps = ifelse(C1_A == 1, 1, 0),
                             RoleSuitability = ifelse(C1_B == 1, 1, 0),
                             EmpyeeCompetence = ifelse(C1_D == 1, 1, 0),
                             StaffRetention = ifelse(C1_M == 1, 1, 0),
                             BizImpact = ifelse(H6 == 1, 5,
                                             ifelse(H6 == 2, 4,
                                             ifelse(H6 == 3, 3,
                                             ifelse(H6 == 4, 2,
                                             ifelse(H6 == 5, 1, 0))))),
                             Productivity = ifelse(H1_2 == 1, 1,
                                            ifelse(H1_2 == 2, 2,3)),
                             Absenteeism = ifelse(H1_3 == 1, 1,
                                           ifelse(H1_3 == 2, 2,3)),
                             Turnover = ifelse(H1_4 == 1, 1,
                                        ifelse(H1_4 == 2, 2, 3)),
                             Motivation = ifelse(H1_6 == 1, 1,
                                          ifelse(H1_6 == 2, 2,3)),
                             WLB = ifelse(H2_2 == 1, 5,
                                   ifelse(H2_2 == 2, 4,
                                   ifelse(H2_2 == 3, 3,
                                   ifelse(H2_2 == 4, 2,
                                   ifelse(H2_2 == 5, 1, 0))))),
                             PeerPerf = ifelse(K1 == 1, 5,
                                        ifelse(K1 == 2, 4,
                                        ifelse(K1 == 3, 3,
                                        ifelse(K1 == 4, 2,
                                        ifelse(K1 == 5, 1, 0))))),
                             BizClass = case_when(A6 < 50 ~ 'Small',
                                                  A6 < 250 ~ 'Medium', 
                                                  A6 >= 250 ~ 'Large'))

# Var. Dropped ######
WLB4 <- WLB4[, !(names(WLB4) %in% c("A13_1", "A13_2", "A13_3", "A13_4", "A13_5",
                                    "S3", "B11", "C1_A", "C1_B", "C1_D",
                                    "C1_M", "H6", "H1_2", "H1_3", "H1_4", "H1_6",
                                    "H2_2", "K1", "A6"))] %>% filter(Policies !="NA")

# Descriptive statistics ######
sum(is.na(WLB4))
summary(WLB4)
skim(WLB4)


# Multi-collinearity Check ####
numeric_data <- select_if(WLB4, is.numeric)
correlation_matrix <- cor(numeric_data)
# Graph of correlation matrix
CorPlot <- corrplot(correlation_matrix, method = "color", 
                    title = "Variables Correlation Matrix")
  
CorPlot #Figure: Correlation Matrix 



# Transformed Data for Visualisation ####

WLB4_Visual <- WLB4 %>% mutate(PartTime = ifelse(PartTime == 1, "Yes", "No"),
                               WorkNight = ifelse(WorkNight == 1, "Yes", "No"),
                               DifWorkShift = ifelse(DifWorkShift == 1, "Yes", "No"),
                               ZeroHours = ifelse(ZeroHours == 1, "Yes", "No"),
                               ThirtyPartTime = ifelse(ThirtyPartTime == 1, "Yes", "No"),
                               Policies = ifelse(Policies == 1, "Yes", "No"),
                               BizOps = ifelse(BizOps == 1, "Yes", "No"),
                               RoleSuitability = ifelse(RoleSuitability == 1, "Yes", "No"),
                               EmpyeeCompetence = ifelse(EmpyeeCompetence == 1, "Yes", "No"),
                               StaffRetention = ifelse(StaffRetention == 1, "Yes", "No"),
                               BizImpact = ifelse(BizImpact == 1, "very -ve",
                                           ifelse(BizImpact == 2, "fairly -ve",
                                           ifelse(BizImpact == 3, "Neither",
                                           ifelse(BizImpact == 4,"fairly +ve" ,
                                           ifelse(BizImpact == 5, "very +ve", "dont know"))))),
                               Productivity = ifelse(Productivity == 1, "positive",
                                              ifelse(Productivity == 2, "negative", "noEffect")),
                               Absenteeism = ifelse(Absenteeism == 1, "positive",
                                             ifelse(Absenteeism == 2, "negative", "noEffect")),
                               Turnover = ifelse(Turnover == 1, "positive",
                                          ifelse(Turnover == 2, "negative", "noEffect")),
                               Motivation = ifelse(Motivation == 1, "positive",
                                            ifelse(Motivation == 2, "negative", "noEffect")),
                               WLB = ifelse(WLB == 1, "strongly disagree",
                                     ifelse(WLB == 2, "disagree",
                                     ifelse(WLB == 3, "neither",
                                     ifelse(WLB == 4, "agree",
                                     ifelse(WLB == 5, "strongly agree", "dont know"))))),
                               PeerPerf = ifelse(PeerPerf == 1, "very -ve",
                                          ifelse(PeerPerf == 2, "fairy -ve",
                                          ifelse(PeerPerf == 3, "neither",
                                          ifelse(PeerPerf == 4, "fairly +ve",
                                          ifelse(PeerPerf == 5, "very +ve", "dont know"))))))

names(WLB4_Visual)

# Percentage of each Variable values ####
percentage <- lapply(WLB4_Visual, function(x) prop.table(table(x)) * 100)
table1 <- print(percentage)


# PolProd####
# Productivity by provision of FW Policies 
PolProd <- WLB4_Visual %>% group_by(Policies, Productivity) %>%
  summarise(count = n()) %>%
  mutate(Percentage = count/(sum(count)*2))


PolProd_figure <- PolProd %>% ggplot(aes(Productivity, Percentage, fill = Policies)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Percentage)), position = position_stack(vjust = 1.03)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("lightsalmon","#76EEC6")) +
  labs(y = NULL, x = "Productivity",
       title = "Organisational Productivity", subtitle = "measured by provision of FW_Policies") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"))


PolProd_figure

# Hypothesis H1 ####
WLB4_V <- WLB4_Visual %>%
  group_by(Productivity, Policies, Absenteeism, Motivation) %>%
  summarise(count = n()) %>%
  mutate(Percentage = count / sum(count) * 100)


H1 <- WLB4_V %>% ggplot(aes(x = Productivity, fill = Policies)) +
  geom_bar(position = "fill") +
  geom_text(aes(label = paste0(round((count / sum(count) * 100), 2), "%")),
            position = position_fill(vjust = 0.5), stat = "count")+
  facet_grid(Absenteeism~Motivation) +
  scale_fill_manual(values = c("lightsalmon", "#76EEC6")) +
  labs(title = "Effect of FWP on Organisational Productivity",
       x = "Productivity", y = "Proportion",
       subtitle = "FWP vs. Motivation(y) and Reduced Absenteeism(x)") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"))

H1
# Hypothesis H1a ####

PolEmpTn <- WLB4_Visual %>% group_by(Turnover, Policies) %>%
  summarise(count = n()) %>%
  mutate(Percentage = round((count/(2*sum(count))), 2))

H1a<- PolEmpTn %>% 
  ggplot(aes(Policies, Percentage, fill = Turnover)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Percentage)), position = position_stack(vjust = 1.03)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("lightsalmon", "#76EEC6", "lightblue")) +
  labs(title = "Effect of FWP on Employee Turnover",
       x = "FW Policies", y = "Proportion of Employee Turnove") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"))

H1a

# Hypothesis H1b ####
#FWPs have a positive effect on employee motivation

PolMot <- WLB4_Visual %>% group_by(Motivation, Policies) %>%
  summarise(count = n()) %>%
  mutate(Percentage = round((count/(2*sum(count))), 2))

H1b <- PolMot %>% 
  ggplot(aes(Policies, Percentage, fill = Motivation)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Percentage)), position = position_stack(vjust = 1.03)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("lightsalmon", "#76EEC6", "lightblue")) +
  labs(title = "Effect of FWP on Employee Motivation",
       x = "FW Policies", y = "Proportion of Employees' Motivation") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"))

H1b
# Hypothesis H2 ####
#Employee motivation influences organisational productivity

MotProd <- WLB4_Visual %>% group_by(Motivation, Productivity) %>%
  summarise(count = n()) %>%
  mutate(Percentage = round((count/(2*sum(count))), 2))

H2 <- MotProd %>% 
  ggplot(aes(Motivation, Percentage, fill = Productivity)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Percentage)), position = position_stack(vjust = 1.03)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("lightsalmon", "#76EEC6", "lightblue")) +
  labs(title = "Effect of Employee Motivation on Organisational Productivity",
       x = "Motivation", y = "Proportion of Productivity") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"))

H2

# Hypothesis H3####
#Moderate levels of employee turnover are positively associated with organizational productivity, provided there are flexibility arrangements.

H3 <- WLB4_Visual %>%
  ggplot(aes(x = Productivity, fill = Turnover)) +
  geom_bar(position = "fill") +
  stat_count(aes(label = scales::percent(..prop..), group = 1), position = "fill", geom = "text", vjust = -0.5) +
  facet_wrap(~Policies) +
  scale_fill_manual(values = c("lightsalmon", "#76EEC6", "lightblue")) +
  labs(title = "Effect of Turnover on Organizational Productivity",
       x = "Productivity", y = "Proportion",
       subtitle = "Measured through the provision of FWP") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"))

H3


# BizClass #####
#Business Size relationship on organizational productivity

BizProd <- WLB4_Visual %>% group_by(BizClass, Productivity) %>%
  summarise(count = n()) %>%
  mutate(Percentage = count/(2*sum(count)))

BizClass <- BizProd %>% 
  ggplot(aes(BizClass, Percentage, fill = Productivity)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Percentage)), position = position_stack(vjust = 1.03)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("lightsalmon", "#76EEC6", "lightblue")) +
  labs(y = NULL, x = "Business Size",
       title = "Organisational Productivity",
       subtitle = "Measured by Business Size") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"))


BizClass


# Regression ####
# Ordinal logistic regression
model1 <- polr(factor(Productivity) ~ ., data = WLB4)
summary(model1) #Having high residual and aic indicates that model has alot of unexplained vars and does not fit
stargazer(model1, type = "text")


#Variable selection automation By StepWise Method ####
modelAuto <- polr(factor(Productivity) ~ ., data = WLB4, method = "logistic", Hess = TRUE)
step_model <- stepAIC(modelAuto, direction = "both")
# View the summary of the final selected model
summary(step_model)

# model3 from the Step_wise Method
model3 <- polr(formula = factor(Productivity) ~ DifWorkShift + 
                 RoleSuitability + BizImpact + 
                 Absenteeism + Turnover + 
                 Motivation + BizClass, data = WLB4, 
               Hess = TRUE, method = "logistic")

summary(model3)
stargazer(model3, type = "text")


# Hypothesis Testing: Decision Tree ####
model1DT <- rpart(factor(Productivity) ~ ., data = WLB4, method = "class")
rpart.plot(model1DT, main = "Decision Tree for the model 1")

ModelDT <- rpart(factor(Productivity) ~ DifWorkShift + 
                   RoleSuitability + BizImpact + 
                   Absenteeism + Turnover + 
                   Motivation + BizClass, data = WLB4, method = "class")
rpart.plot(ModelDT, main = "Decision Tree for the model 3")

# Accuracy of model3 ####
### Data Partition
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(WLB4), replace = TRUE, prob = c(0.7,0.3))
Train <- WLB4[sample,]
Test <- WLB4[!sample,]

OptModel_Predict <-  predict(model3, Test, type = "class")

cm <- confusionMatrix(OptModel_Predict, factor(Test$Productivity))
cm

# prediction of FWP or No FWP ####
FWP <- subset(WLB4, Policies != 0)
NoFWP <- subset(WLB4, Policies != 1)

modelFWP <- polr(formula = factor(Productivity) ~ DifWorkShift + 
                   RoleSuitability + BizImpact + 
                   Absenteeism + Turnover + 
                   Motivation + BizClass, data = FWP, method = "logistic")

ChaidFWP <-  rpart(factor(Productivity) ~ DifWorkShift + 
                     RoleSuitability + BizImpact + 
                     Absenteeism + Turnover + 
                     Motivation + BizClass, data = FWP, method = "class")
rpart.plot(ChaidFWP, main = "Decision Tree for the model3 with Trainset FWP")

#Accuracy 

prediction2 <- predict(modelFWP, newdata = NoFWP, method = "class")

table_mat2 <- table(NoFWP$Productivity, prediction2)
table_mat2

accuracy_Test2 <- sum(diag(table_mat2))/sum(table_mat2)
print(paste("Accuracy of the model3 on NoFWP", accuracy_Test2))




# RFM ####
WLB4RFM <- WLB4 %>% mutate_if(is.character, as.factor)

# Partition
set.seed(123)
trainIndex <- createDataPartition(WLB4RFM$Productivity, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
WLB4Train <- WLB4RFM[ trainIndex,]
WLB4Test  <- WLB4RFM[-trainIndex,]

set.seed(123)
WLB4Train$Productivity <- as.factor(WLB4Train$Productivity)

# Train the random forest model
rfModel <- randomForest(Productivity ~ ., data = WLB4Train, importance = TRUE, ntree = 500)
print(rfModel)

# Prediction  
rfPredictions <- predict(rfModel, WLB4Test)
rfPredictions <- factor(rfPredictions, levels = levels(WLB4Train$Productivity))
WLB4Test$Productivity <- factor(WLB4Test$Productivity, levels = levels(WLB4Train$Productivity))
rfPredictions <- as.factor(rfPredictions)
WLB4Test$Productivity <- as.factor(WLB4Test$Productivity)
levels(rfPredictions) <- levels(WLB4Test$Productivity)

# confusion matrix and RFModel Evalautiom
confMatrix <- confusionMatrix(rfPredictions, WLB4Test$Productivity)
confMatrix <- confusionMatrix(rfPredictions, WLB4Test$Productivity)

print(confMatrix)

# Vars Importance
importance <- importance(rfModel)
varImpPlot(rfModel)
importance_df <- data.frame(Variable = row.names(importance), Importance = round(importance[ , 'MeanDecreaseGini'],2))

# Vars Importance Plot

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  coord_flip() +
  theme_minimal() +
  labs(title = 'Variable Importance o Productivity from Random Forest Model',
       x = 'Variables',
       y = 'Importance') +
  theme(panel.background = element_rect(fill = "transparent"))


