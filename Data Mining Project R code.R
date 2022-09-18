heart<-read.csv("heart.csv")
#get few entries of data set
head(heart)
#Descriptive Statistics of Data set
summary(heart)
# Data Cleaning
library(dplyr)
heart$Age<-heart$age
heart$Sex<-heart$sex 
heart$Chestpaintype<-heart$cp
heart$RestBloodPressure<-heart$trestbps
heart$SerumCholestrol<-heart$chol
heart$FastBloodSugar<-heart$fbs
heart$RestingECG<-heart$restecg 
heart$MaxHeartRate<-heart$thalach
heart$ExerciseInducedPain<-heart$exang%>% as.logical()
heart$STDepressionExercise<-heart$oldpeak
heart$Slope<-heart$slope
heart$VesselsUnderFluro<-heart$ca
heart$FormOfThal<-heart$thal
heart$heartdisease<-heart$target %>% as.logical()
heart<-dplyr::select(heart, -c(age,sex,cp,chol,fbs,restecg,thalach,exang,oldpeak,slope,ca,thal,target,trestbps))
# Missing Data
# Uncomment next line to check NA values(Missing values)
#is.na(heart) 
# Descriptive Analysis
#install.packages("tidyr")
library("tidyr")
hd_long_fact_tb1 <- heart %>%
  dplyr::select(Sex,
                Chestpaintype,
                FastBloodSugar,
                RestingECG,
                STDepressionExercise,
                FormOfThal,
                heartdisease) %>%
  mutate(Sex = recode_factor(Sex, `0` = "female", 
                             `1` = "male" ),
         Chestpaintype = recode_factor(Chestpaintype, `0` = "typical",   
                                       `1` = "atypical",
                                       `2` = "non-angina", 
                                       `3` = "asymptomatic"),
         FastBloodSugar = recode_factor(FastBloodSugar, `0` = "<= 120 mg/dl", 
                                        `1` = "> 120 mg/dl"),
         RestingECG = recode_factor(RestingECG, `0` = "normal",
                                    `1` = "ST-T abnormality",
                                    `2` = "LV hypertrophy"),
         STDepressionExercise = recode_factor(STDepressionExercise, `0` = "up-sloaping",
                                              `1` = "flat",
                                              `2` = "down-sloaping"),
         FormOfThal = recode_factor(FormOfThal, `1` = "normal",
                                    `2` = "fixed defect",
                                    `3` = "reversible defect")) %>%
  gather(key = "key", value = "value", -heartdisease)
#Visualize with bar plot for categorical variablea
library(ggplot2)
hd_long_fact_tb1 %>% 
  ggplot(aes(value)) +
  geom_bar(aes(x        = value, 
               fill     = heartdisease), 
           alpha    = .6, 
           position = "dodge", 
           color    = "black",
           width    = .8
  ) +
  labs(x = "",
       y = "",
       title = "Scaled Effect of Categorical Variables") +
  facet_wrap(~ key, scales = "free", nrow = 4) +
  scale_fill_manual(
    values = c("#0F4D92", "#20a486ff"),
    name   = "heart\nDisease",
    labels = c("No HD", "Yes HD"))
#### numerical data
hd_long_cont_tbl <- heart  %>%
  dplyr::select(Age,
                RestBloodPressure,
                SerumCholestrol,
                MaxHeartRate,
                STDepressionExercise,
                VesselsUnderFluro,
                heartdisease) %>% 
  gather(key   = "key", 
         value = "value",
         -heartdisease)
#Visualize numeric variables as boxplots
hd_long_cont_tbl %>% 
  ggplot(aes(y = value)) +
  geom_boxplot(aes(fill = heartdisease),
               alpha  = .6,
               fatten = .7) +
  labs(x = "",
       y = "",
       title = "Boxplots for Numeric Variables") +
  scale_fill_manual(
    values = c("#0F4D92", "#20a486ff"),
    name   = "heart\nDisease",
    labels = c("No HD", "Yes HD"))  +
  facet_wrap(~ key, 
             scales = "free", 
             ncol   = 2) 
# Explanatory Data Analysis
#Correlation Analysis
library(corrplot)
mydata.cor = cor(heart)
corrplot(mydata.cor,type="upper", order="hclust", sig.level = 0.05, insig = "blank",addCoef.col = "black")
mydata.cor
mat_1 <-as.dist(round(cor(heart),2))
mat_1
#install.packages("rcompanion")
library(rcompanion)

# Uncomment next set of lines to normally distribute the variables which would not be suggested for model builiding as explained in the paper
# #Age
# plotNormalHistogram(heart$Age,main="Age",xlab="Age")
# heart$Age<-rnorm(heart$Age)
# plotNormalHistogram(heart$Age,main="Age",xlab="Age")
# #RestBloodPressure
# plotNormalHistogram(heart$RestBloodPressure,main="Rest Blood Pressure",xlab="Rest Blood Pressure")
# heart$RestBloodPressure<-rnorm(heart$RestBloodPressure)
# plotNormalHistogram(heart$RestBloodPressure,main="Rest Blood Pressure",xlab="Rest Blood Pressure")
# #chol
# plotNormalHistogram(heart$SerumCholestrol,main="Serum Cholestrol",xlab="Serum Cholestrol")
# heart$SerumCholestrol<-rnorm(heart$SerumCholestrol)
# plotNormalHistogram(heart$SerumCholestrol,main="Serum Cholestrol",xlab="Serum Cholestrol")
# #heartBeat
# plotNormalHistogram(heart$MaxHeartRate,main="Maximum Heart Rate",xlab="Serum Cholestrol")
# heart$MaxHeartRate<-rnorm(heart$MaxHeartRate)
# plotNormalHistogram(heart$MaxHeartRate,main="Maximum Heart Rate",xlab="Serum Cholestrol")
# #depression Induced exercise
# plotNormalHistogram(heart$STDepressionExercise,main="Depression Induced Exercise",xlab="Depression Induced Exercise")
# heart$STDepressionExercise<-rnorm(heart$STDepressionExercise)
# plotNormalHistogram(heart$STDepressionExercise,main="Depression Induced Exercise",xlab="Depression Induced Exercise")
# # NUMBER OF VESSEL
# plotNormalHistogram(heart$VesselsUnderFluro,main="Vessels under Fluro",xlab="Vessels under Fluro")
# heart$VesselsUnderFluro<-rnorm(heart$VesselsUnderFluro)
# plotNormalHistogram(heart$VesselsUnderFluro,main="Vessels under Fluro",xlab="Vessels under Fluro")
## Prelimnary Model Generation:Binary Logistic Regression.
#model;
model1<-glm(heart$heartdisease~heart$Age+heart$Sex+heart$Chestpaintype+heart$RestBloodPressure+heart$SerumCholestrol+heart$FastBloodSugar+heart$RestingECG+heart$MaxHeartRate+heart$ExerciseInducedPain+heart$STDepressionExercise+heart$STDepressionExercise+heart$Slope+heart$VesselsUnderFluro+heart$FormOfThal+heart$FormOfThal,family=binomial)
summary(model1)
library(car)
library(mlbench)
library(MASS)
library(pROC)
vif(model1)
# Final Model Generation:Binary Logistic Regression.
model2<- stepAIC(model1)
summary(model2)
vif(model2)
# Model Comaprison and Evaluation
summary(model2$fitted.values)
hist(model2$fitted.values,main = " Histogram ",xlab = "Probability of 'pos' heartdisease", col = 'light green')
heart$Predict <- ifelse(model2$fitted.values >0.5,"pos","neg")
#heart$Predict
model1$aic
model2$aic
mytable <- table(heart$heartdisease,heart$Predict)
rownames(mytable) <- c("Obs. neg","Obs. pos")
colnames(mytable) <- c("Pred. neg","Pred. pos")
mytable
sum(diag(mytable))
efficiency <- sum(diag(mytable))/sum(mytable)
efficiency
roc(heartdisease~model2$fitted.values, data = heart, plot = TRUE, main = "ROC CURVE", col= "blue")
auc(heartdisease~model2$fitted.values, data = heart)