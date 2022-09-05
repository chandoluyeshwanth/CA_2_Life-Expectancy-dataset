# Data preparation  ----
##Loading Packages----
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(readr)
library(corrplot)
library(leaps)
library(car)
library(ggpubr)
install.packages("Metrics")
install.packages("reshape2")
install.packages("moments")
library(moments)
install.packages("Hmisc")
library(Hmisc)
### plotting option 
set_plot_dimensions <- function(width_choice , height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}

## Loading data
Life_expectancy_analysis <- read_csv("Life-expectancy-analysis.csv")
head(Life_expectancy_analysis)
sprintf("Dataset size: [%s]", toString(dim(Life_expectancy_analysis)))
str(Life_expectancy_analysis)

#Cleaning & Understanding Data -----

names(Life_expectancy_analysis)[4] <- "Life_expectancy"
names(Life_expectancy_analysis)[5] <- "Adult_Mortality"
names(Life_expectancy_analysis)[6] <- "infant_deaths"
names(Life_expectancy_analysis)[8] <- "percentage_expenditure"
names(Life_expectancy_analysis)[9] <- "Hepatitis_B"
names(Life_expectancy_analysis)[12] <- "under_five_deaths"
names(Life_expectancy_analysis)[14] <- "Total_expenditure"
names(Life_expectancy_analysis)[19] <- "thinness_1_19_years"
names(Life_expectancy_analysis)[20] <- "thinness_5_9_years"
names(Life_expectancy_analysis)[21] <- "Income_composition_of_resources"


missing.rows = dim(Life_expectancy_analysis)[1] -  
  dim(na.omit(Life_expectancy_analysis))[1]
sprintf("Dataset size: [%s]", toString(dim(Life_expectancy_analysis)))
sprintf("Missing rows: %s (%s%%)", missing.rows,
        round((missing.rows*100)/dim(Life_expectancy_analysis)[1], 2))

missings_df <- data.frame(type=c("missing", "non-missing")
                                              ,count = c(missing.rows, 
                                                         dim(na.omit
                                                             (Life_expectancy_analysis))[1]))

set_plot_dimensions(16,4)
ggplot(missings_df, aes(fill=type, y="", x=count)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Missing vs Non-missing row counts") +
  xlab("Missing count") + ylab("") +
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Set1")


missing_counts <- data.frame(feature = factor(names(Life_expectancy_analysis)),
                             counts=sapply(Life_expectancy_analysis
                                           , function(x) sum(is.na(x))))

plot(16,8)
ggplot(missing_counts,
       aes(x=reorder(feature, -counts), y=counts, fill=counts)) +
  geom_bar(stat="identity") +
  ggtitle("Missing counts in each feature") +
  xlab("Feature") + ylab("Missing count") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_continuous(trans = 'reverse')

##Boxplot----

plot(20,10)
par(mfrow=c(2,7))
boxplot(Life_expectancy_analysis$Life_expectancy,
        ylab = "Life Expectancy",
        main = "Boxplot of Life Expectancy",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$Adult_Mortality,
        ylab = "Adult Mortality",
        main = "Boxplot of Adult Mortality",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$Alcohol,
        ylab = "Alcohol",
        main = "Boxplot of Alcohol",
        col= "#008080",
        outcol="#008080")
boxplot(Life_expectancy_analysis$Hepatitis_B,
        ylab = "Hepatitis B",
        main = "Boxplot of Hepatitis B",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$BMI,
        ylab = "BMI",
        main = "Boxplot of BMI",
        col= "#008080",
        outcol="#008080")
boxplot(Life_expectancy_analysis$Polio,
        ylab = "Polio",
        main = "Boxplot of Polio",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$Total_expenditure,
        ylab = "Total Expenditure",
        main = "Boxplot of Total Expenditure",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$Diphtheria,
        ylab = "Diphteria",
        main = "Boxplot of Diphteria",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$GDP,
        ylab = "GDP",
        main = "Boxplot of GDP",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$Population,
        ylab = "Population",
        main = "Boxplot of Population",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$thinness_1_19_years,
        ylab = "Thinness 1-19 years",
        main = "Boxplot of Thinness for 1-19 years old",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$thinness_5_9_years,
        ylab = "Thinness 5-9 years",
        main = "Boxplot of Thinness for 5-9 years old",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Life_expectancy_analysis$Income_composition_of_resources,
        ylab = "Income Composition",
        main = "Boxplot of Income Composition",
        col= "#008080",
        outcol="#008080")
boxplot(Life_expectancy_analysis$Schooling,
        ylab = "Schooling",
        main = "Boxplot of Schooling",
        col= "#FF6666",
        outcol="#FF6666")


##Calculate median for the high outliers variables.----
Life_expectancy_median <- median(Life_expectancy_analysis$Life_expectancy,
                                 na.rm = TRUE)

Adult_Mortality_median <- median(Life_expectancy_analysis$Adult_Mortality,
                                 na.rm = TRUE)

Hepatitis_B_median <- median(Life_expectancy_analysis$Hepatitis_B,  na.rm = TRUE)

Polio_median <- median(Life_expectancy_analysis$Polio,  na.rm = TRUE)

Diphtheria_median <- median(Life_expectancy_analysis$Diphtheria,  na.rm = TRUE)

Total_expenditure_median <- median(Life_expectancy_analysis$Total_expenditure
                                   ,  na.rm = TRUE)

GDP_median <- median(Life_expectancy_analysis$GDP,  na.rm = TRUE)

Population_median <- median(Life_expectancy_analysis$Population,  na.rm = TRUE)

thinness_1_19_years_median <- median(Life_expectancy_analysis$thinness_1_19_years,
                                     na.rm = TRUE)

thinness_5_9_years_median <- median(Life_expectancy_analysis$thinness_5_9_years,
                                    na.rm = TRUE)

Schooling_median <- median(Life_expectancy_analysis$Schooling,  na.rm = TRUE)


##Calculate mean for the low or none outliers variables.----
Alcohol_mean <- mean(Life_expectancy_analysis$Alcohol,  na.rm = TRUE)

BMI_mean <- mean(Life_expectancy_analysis$BMI,  na.rm = TRUE)

Income.composition.of.resources_mean <- mean(Life_expectancy_analysis
                                             $Income_composition_of_resources,
                                             na.rm = TRUE)
##Now, we replace the NAs with mean and median values respectively.----
Life_expectancy_analysis$Life_expectancy[is.na(Life_expectancy_analysis$Life_expectancy)] <- Life_expectancy_median

Life_expectancy_analysis$Adult_Mortality[is.na(Life_expectancy_analysis$Adult_Mortality)] <- Adult_Mortality_median

Life_expectancy_analysis$Hepatitis_B[is.na(Life_expectancy_analysis$Hepatitis_B)] <- Hepatitis_B_median

Life_expectancy_analysis$Polio[is.na(Life_expectancy_analysis$Polio)] <- Polio_median

Life_expectancy_analysis$Diphtheria[is.na(Life_expectancy_analysis$Diphtheria)] <- Diphtheria_median

Life_expectancy_analysis$Total_expenditure[is.na(Life_expectancy_analysis$Total_expenditure)] <- Total_expenditure_median

Life_expectancy_analysis$GDP[is.na(Life_expectancy_analysis$GDP)] <- GDP_median

Life_expectancy_analysis$Population[is.na(Life_expectancy_analysis$Population)] <- Population_median

Life_expectancy_analysis$thinness_1_19_years[is.na(Life_expectancy_analysis$thinness_1_19_years)] <- thinness_1_19_years_median

Life_expectancy_analysis$thinness_5_9_years[is.na(Life_expectancy_analysis$thinness_5_9_years)] <- thinness_5_9_years_median

Life_expectancy_analysis$Schooling[is.na(Life_expectancy_analysis$Schooling)] <- Schooling_median

#means

Life_expectancy_analysis$Alcohol[is.na(Life_expectancy_analysis$Alcohol)] <- Alcohol_mean

Life_expectancy_analysis$BMI[is.na(Life_expectancy_analysis$BMI)] <- BMI_mean

Life_expectancy_analysis$Income_composition_of_resources[is.na(Life_expectancy_analysis$Income_composition_of_resources)] <- Income.composition.of.resources_mean


Life_expectancy_analysis$Status <- as.factor(Life_expectancy_analysis$Status)

data<-  Life_expectancy_analysis
  

set_plot_dimensions(10,8)
ggplot(data, aes(x=Life_expectancy)) + 
  geom_density(alpha=.3, fill="red", color="red", size=1.5)+
  geom_vline(aes(xintercept=mean(Life_expectancy)), size=1)+
  ggtitle("Distribution density of Life expectancy") +
  theme(text = element_text(size = 18))

sprintf("Skewness: [%s]", toString(skewness(data$Life_expectancy, na.rm = TRUE)))

data$Life_expectancy <- sqrt(max(data$Life_expectancy+1)- data$Life_expectancy)
data$Life_expectancy<- scale(data$Life_expectancy, scale=TRUE, center = TRUE)
ggplot(data, aes(x=Life_expectancy)) +
  geom_density(alpha=.3, fill="red", color="red", size=1.5)+
  geom_vline(aes(xintercept=mean(Life_expectancy)))+
  ggtitle("Distribution density of Life expectancy") +
  theme(text = element_text(size = 18))

sprintf("Skewness: [%s]", toString(skewness(data$Life_expectancy, na.rm = TRUE)))


data <- as.data.frame(scale(subset(data,select = -c(as.numeric(Status)), scale=TRUE, center = TRUE)))
 Life_expectancy_analysis$Status <- data$Status
#str(data)

# Exploratory Data Analysis (EDA)----

plot(10,8)
ggplot(data ,aes(x= Status,y=Life_expectancy, fill= Status)) + 
  geom_boxplot() +
  ggtitle("Life expectancy per country Status")+
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Set1")


library(corrplot)  
plot(16,10)
corr <- round(cor(subset(Life_expectancy_analysis, select =-c((Status)))), 3)
ggcorrplot(corr,type = "upper", lab = TRUE, outline.color = "black", lab_size = 4, legend.title = "Correlation")+
  ggtitle("Correlation Matrix")

install.packages("car")
library(car)

mod.linear <- lm(Life_expectancy~ ., data = subset(data, select =-c(Status)))
vifs <- data.frame(vif(mod.linear))

plot(16,8)
ggplot(vifs, aes(y=vif.mod.linear., x=row.names(vifs))) + 
  geom_bar(aes(fill=vif.mod.linear.>5),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(5, 10, 50, 100))+
  geom_hline(yintercept = 5, colour = "red") + 
  ggtitle("VIF per feature") +
  xlab("Featurs") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Dark2")
  
data_EDA <- subset(data, select = -c(infant_deaths))
data_EDA <- subset(data_EDA, select = -c(GDP))
data_EDA <- subset(data_EDA, select = -c(thinness_1_19_years))

install.packages("ggcorrplot")
library(ggcorrplot)
plot(16,10)
corr <- round(cor(subset(data_EDA, select =-c(Status))), 3)
ggcorrplot(corr,type = "upper", lab = TRUE, outline.color = "black", lab_size = 4, legend.title = "Correlation")




# Feature ----
regfit.best <- regsubsets(Life_expectancy~., data= data_EDA, nvmax = 16)
reg.summary <- summary(regfit.best)

par(mfrow=c(2,2))

#- residual sum of squares:
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
which.min(reg.summary$rss)
points(16,reg.summary$rss[16], col="red",cex=2,pch=20)

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
which.max(reg.summary$adjr2)
points(15,reg.summary$adjr2[15], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(13,reg.summary$cp[13],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(12,reg.summary$bic[12],col="red",cex=2,pch=20)



par(mfrow=c(1,1))

regfit.fwd <- regsubsets(Life_expectancy~.,data=data_EDA,nvmax=16,method="forward")
fwd.summary <-summary(regfit.fwd)

# fwd.summary
plot(8,6)
plot(fwd.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(fwd.summary$bic)
points(12,fwd.summary$bic[12],col="red",cex=2,pch=20)



regfit.bwd <- regsubsets(Life_expectancy~.,data=data_EDA,nvmax=16,method="backward")
bwd.summary <- summary(regfit.bwd)

# fwd.summary
plot(8,6)
plot(bwd.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(bwd.summary$bic)
points(12,bwd.summary$bic[12],col="red",cex=2,pch=20)


v_names <- rownames(as.data.frame(coef(regfit.best,12)))
coefs<- data.frame(v_names)
coefs$best_coef_value<- coef(regfit.best,12)
coefs$fwd_coef_value <-  coef(regfit.fwd,12)
coefs$bwd_coef_value <-  coef(regfit.bwd,12)

plot(18,4)
ggplot(coefs,
       aes(x=v_names, y=best_coef_value, fill=best_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Features & coeffecients: [method Best]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))

ggplot(coefs,
       aes(x=v_names, y=fwd_coef_value, fill=fwd_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Features & coeffecients: [method Forward inclusion]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))
ggplot(coefs,
       aes(x=v_names, y=bwd_coef_value, fill=bwd_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Feature & coeffecients: [method Backward elimination]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))

data_FS <- subset(data_EDA, select=c(Life_expectancy,Status, Adult_Mortality, percentage_expenditure,
                                     Hepatitis_B, Polio, BMI, thinness_5_9_years, Measles,
                                     Diphtheria,HIV_AIDS,Income_composition_of_resources,Schooling))

#MODEL------
plot(16,10)
set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.70,0.30))
train <- data[sample, ]
x.test <-data[!sample, ]
y.test <- data[!sample, ]$Life_expectancy
model.full<- lm(Life_expectancy~., data = train)
summary(model.full)

pred <- predict(model.full, newdata=x.test)
rmse(pred,y.test)
summary(model.full)$adj.r.squared
par(mfrow=c(2,2))
plot(model.full)

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(data_EDA), replace=TRUE, prob=c(0.70,0.30))
train <- data_EDA[sample, ]
x.test <-data_EDA[!sample, ]
y.test <- data_EDA[!sample, ]$Life_expectancy
model.EDA <- lm(Life_expectancy~., data = train)
summary(model.EDA)


pred <- predict(model.EDA, newdata=x.test)
rmse(pred,y.test)
summary(model.EDA)$adj.r.squared
par(mfrow=c(2,2))
plot(model.EDA)

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(data_FS), replace=TRUE, prob=c(0.70,0.30))
train <- data_FS[sample, ]
x.test <-data_FS[!sample, ]
y.test <- data_FS[!sample, ]$Life_expectancy
model.FS <- lm(Life_expectancy~., data = train)
summary(model.FS)

pred <- predict(model.FS, newdata=x.test)
rmse(pred,y.test)
summary(model.FS)$adj.r.squared
par(mfrow=c(2,2))
plot(model.FS)