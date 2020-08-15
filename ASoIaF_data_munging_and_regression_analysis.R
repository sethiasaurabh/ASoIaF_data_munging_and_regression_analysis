#########################################################################################################

#Libraries used;

library(dplyr)
library(corrplot)
library(ggcorrplot)
library(stringr)


### Load datasets:

Battles <- read.csv("C:\\Users\\Lenovo\\Downloads\\Fw__Quiz_For_Data_Mining\\Battles_File.csv", 
                    header = T, na.strings=c("","NA"), sep = ',')

char_death <- read.csv("C:\\Users\\Lenovo\\Downloads\\Fw__Quiz_For_Data_Mining\\character-deaths.csv", 
                       header = T, na.strings=c("","NA"))

# Dataset Information
print("***** Dimension *****")
dim(char_death)
print("***** Column Names *****")
names(char_death)
print("***** Summary *****")
summary(char_death)
print("***** Structure *****")
dplyr::glimpse(char_death)
print("***** Sample *****")
dplyr::sample_n(char_death, 4)

# Checking isNA coulumns
print("***** Null Column Count *****")
sapply(Battles, function(x) sum(is.na(x)))

sapply(char_death, function(x) sum(is.na(x)))

### cleaning dataset;

### checking percentage of null values

missingvalues<-char_death %>% summarise_each(funs(sum(is.na(.))*100/n()))
missingvalues<- gather(missingvalues,key = "feature",value ="missing_pct")
missingvalues

missingfeatures<-filter(missingvalues,missing_pct > 0.01)
missingfeatures

missingfeatures %>% ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat = "identity",fill="red") +
  coord_flip()+theme_bw()


# calculating and inserting median values into book.intro. chapter;

char_death$Book.Intro.Chapter[is.na
                              (char_death$Book.Intro.Chapter)]<-median(char_death$Book.Intro.Chapter,na.rm = T)

# removing house word from Allegiances

char_death$Allegiances <- gsub("House ", "", char_death$Allegiances)

# Creating new variable named charcterliveornot using Death.Year variable;

Chracterlive<-char_death$Death.Year
charcterliveornot<-Chracterlive > 1
char_death$charcterliveornot<-charcterliveornot
char_death$charcterliveornot
char_death$charcterliveornot[is.na(char_death$charcterliveornot)]<-0

sapply(char_death, function(x) sum(is.na(x)))

# Converting variable to factor then integer;

char_death$Allegiances <- as.factor(char_death$Allegiances)

char_death$Allegiances <- as.integer(char_death$Allegiances)

glimpse(char_death)

# Saving edited file;

write.csv(char_death,'char_death_updated.csv')


#########################################################################################################

## Running logestic regression model:

# checking correlation;

corr<- char_death %>% 
  select (charcterliveornot,Gender,Nobility,GoT,CoK,SoS,FfC,DwD, Book.Intro.Chapter,
          Allegiances)

cor(corr)

corrplot(cor(corr), method = "number", type = "lower", bg = "grey",
         title = "Correlation Matrix")

############################################### initial model ###################################

#we are making multi logistic regression model for predicting the survical rate of characters
#affected by other independent variables;

logistic <- glm(charcterliveornot ~ Gender + Nobility + GoT + CoK + SoS + FfC + DwD + Book.Intro.Chapter +
                  Allegiances,
                data = char_death, family = binomial)

summary(logistic)


# creating new dataframe that contains the probability of survival along with the 
# actual survived status

predicted.data <- predict(logistic, char_death)
predicted.df <- data.frame(actual_values = char_death$charcterliveornot,
                           predicted_values = predicted.data)

head(predicted.df)

# Cheking model accuracy;

anova(logistic,test='LR')

### ###VIF or model validation

vif0 <- glm(charcterliveornot ~ Gender + Nobility + GoT + CoK + SoS + FfC + DwD+ Book.Intro.Chapter +
              Allegiances,
            data = char_death, family = binomial); summary(logistic)
vif(vif0)

######################### selecting backward selection method and make logistic model again;##########

step <- step(logistic, direction = 'backward')  

#creating new model after selecting vairbales acc to above selectio method step;

logistic.1 <- glm(charcterliveornot ~ Gender + Nobility + GoT + CoK + FfC + DwD+ Book.Intro.Chapter +
                    Allegiances,
                  data = char_death, family = binomial)

summary(logistic.1)

# creating new dataframe that contains the probability of survival along with the 
# actual survived status

predicted.data.1 <- predict(logistic.1, char_death)
predicted.df.1 <- data.frame(actual_values = char_death$charcterliveornot,
                             predicted_values = predicted.data.1)

head(predicted.df.1)

# Cheking model accuracy;

anova(logistic.1,test='LR')

### compare both the models using anova test;

anova(logistic,logistic.1, test = 'LR')

BIC(logistic,logistic.1)

### VIF or model validation

vif1 <- glm(charcterliveornot ~ Gender + Nobility + GoT + CoK + FfC + DwD+ Book.Intro.Chapter +
              Allegiances,
            data = char_death, family = binomial) ; summary(logistic.1)
vif(vif1)

### Model Comparision;


cat("Is the MSE of new model is LESS THAN the initial model? ",
    mean(logistic$residuals^2) > mean(logistic.1$residuals^2), "\n");

MSE.0 <- mean(logistic$residuals^2)
MSE.1 <- mean(logistic.1$residuals^2)

#######Try to make a again new model to see if their are any imorovement in model accuracy or not ####

logistic.2 <- glm(charcterliveornot ~ Gender + Nobility + GoT + CoK + FfC + DwD,
                  data = char_death, family = binomial)

summary(logistic.2)

step <- step(logistic.2, direction = 'backward')  

# Cheking model accuracy;

anova(logistic.2,test='LR')

### compare both the models using anova test;

anova(logistic.1,logistic.2, test = 'LR')

BIC(logistic.1,logistic.2)

### VIF or model validation

vif2 <- glm(charcterliveornot ~ Gender + Nobility + GoT + CoK + FfC + DwD,
            data = char_death, family = binomial) ; summary(logistic.2)
vif(vif2)

### Model Comparision;


cat("Is the MSE of new model is LESS THAN the  model 1? ",
    mean(logistic.1$residuals^2) > mean(logistic.2$residuals^2), "\n");

MSE.1 <- mean(logistic.1$residuals^2)
MSE.2 <- mean(logistic.2$residuals^2)

