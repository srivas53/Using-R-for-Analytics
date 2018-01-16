# Load packages
library(DT)
library(dplyr)
library(caTools)
library(lattice)
library(ggplot2)
library(caret)
library(magrittr)
library(dplyr)
library(corrplot)

# Read Dataset
hr = read.table('G:\\HR_comma_sep.csv', header = T,sep=',')



# Select only good employees
hr_model <- hr %>% filter(Last.Evaluation >= 0.70 | Time.spent.on.job >= 4 | No.of.Projects > 4)
summary(hr_model)

# Rowsnames= Employee ID, for future search
row.names(hr_model)=hr_model$Employee.ID

# Test the correlation of parameters in good employees
#hr_good_leaving_people <- hr %>% filter(Last.Evaluation >= 0.70 | Time.spent.on.job >= 4 | No.of.Projects > 4 )
#hr_good_people_select <- hr_good_leaving_people %>% select(Satisfaction.Level: Promotion.in.last.5yrs)
#M <- cor(hr_good_people_select)
#corrplot(M, method="circle")

# Make 'left' factor
hr_model$left <- as.factor(hr_model$Left)


set.seed(100)
# Keep some data to test again the final model
inTraining <- createDataPartition(hr_model$Left, p = .75, list = FALSE)
training <- hr_model[ inTraining,]
testing  <- hr_model[-inTraining,]

# Estimate the drivers of attrition
logreg = glm(Left ~ Satisfaction.Level + Last.Evaluation + Avg.monthly.hours, family=binomial(logit), data=hr_model)

# Make predictions on the out-of-sample data
ProbaToLeave=predict(logreg,newdata=hr_model,type="response")

# Structure the prediction output in a table
predattrition = data.frame(ProbaToLeave)

# Add columns to the predattrition dataframe containing the performance, satisfaction level, ave_work_hours
predattrition$Performance=hr_model$Last.Evaluation
predattrition$Satisfaction = hr_model$Satisfaction.Level
predattrition$Avg.monthly.hours = hr_model$Avg.monthly.hours

# Give employee new ID
#predattrition$Employee.ID=hr$Employee.ID #work on this

# Calculate the priority and generate the plots
predattrition$priority=ProbaToLeave*predattrition$Performance
plot(predattrition$ProbaToLeave,predattrition$Performance)
plot(predattrition$ProbaToLeave,predattrition$Satisfaction)


# Order the priority
orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]

# Give each good employee a ranking
for(i in 1:nrow(hr_model)){
  orderpredattrition[i,"Rank"] = i
}


# Necessary
newDf <- testing[0,]