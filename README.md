# About my project on how to handle missing values in R
##Load data

data("airquality")

airquality

##find missing values

is.na(airquality)

complete.cases(airquality)

###count missing values

sum(is.na(airquality))

colSums(is.na(airquality))

rowSums(is.na(airquality))

##omit missing values

na.omit(airquality)

airquality[complete.cases(airquality),]

na.exclude(airquality)

##remove missing values

mean(airquality$Ozone)

mean(airquality$Ozone,na.rm = TRUE)

library(tidyr)

airquality %>% drop_na()

airquality %>% drop_na(Ozone)

airquality %>% drop_na(Ozone,Solar.R)

##Return error message when NA exist

na.fail(airquality)

##We can leave data with no action

na.pass(airquality)


##Second approach to handling of missing values using starwars dataset

library(tidyverse)

##loading data set

view(starwars)

starwars %>%

  select(name, gender,hair_color,height)%>%
  
  view()

##visualizing and inspecting starwars data set 

starwars%>%

  select(name,gender,hair_color,height)%>%
  
  filter(!complete.cases(.))%>%
  
    view()

library(mice)

md.pattern(starwars)

##Remove missing values from the data set

starwars%>%

  select(name,gender,hair_color,height)%>%
  
  na.omit()%>%
  
  view()
  
##drop rows that has missing data from a particular variable instead of omitting

starwars%>%

  select(name,gender,hair_color,height)%>%
  
  drop_na(height)%>%
  
  filter(!complete.cases(.))%>%
  
  view()

#changing the values inside a categorical variable

starwars%>%

  select(name,gender,hair_color,height)%>%
  
  drop_na(height)%>%
  
  mutate(hair_color = replace_na(hair_color, "none"))%>%
  
  filter(!complete.cases(.))%>%
  
  view()

##showing the information in each variable

unique(starwars$hair_color)

unique(starwars$gender)

##changing the values inside a variable

starwars%>%

  select(name,gender,hair_color,height)%>%
  
  mutate(hair_color = na_if(hair_color, "none"))%>%
  
  filter(is.na(hair_color))%>%
  
  view()

##the average of the variabes

mean(starwars$height)

mean(starwars$height,na.rm = TRUE)


##Third approach on how to handle missing data in R

library(mice)

library(VIM)

library(readxl)

##Importing data
data <- read_excel("C:\\Users\\User\\Documents\\Not available values.xlsx")

View(data)

str(data)

summary(data)

##percentage of missing data in each variable

p<-function(x){sum(is.na(x))/length(x)*100}

apply(data,2,p)

md.pattern(data)

md.pairs(data)

marginplot(data)

##impute

impute<- mice(data,m=5, seed = 123)

print(impute)

##looking at some imputed values

impute$imp$LengthOfCare

impute$imp$Age

#looking at the rows of the lengthofcare variable

data[34,]

summary(data$LengthOfCare)

data[823,]

summary(data$LengthOfCare)

##complete data set where missing values are replaced by first imputation

newdata<-complete(impute,1)

##complete data set where missing values are replaced by second imputation

newdata2<-complete(impute,2)

#Distribution of observed/imputed values

stripplot(impute,pch = 20,cex = 1.2)

xyplot(impute, Age~LengthOfCare|.imp,pch=20,cex=1.4)
