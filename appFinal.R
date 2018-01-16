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
getwd()
setwd('C:/Users/Varun Srivastava/Desktop/analytics using R')

# Read Dataset
hr = read.table('C:/Users/Varun Srivastava/Desktop/analytics using R/HR_comma_sep (2).csv', header = T,sep=',')



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

library(shiny)
# ----------------------------------------------------------------------------------------Shiny starts here
ui<- navbarPage("Turnover Prediction",
                tabPanel("Function1",
               #Function 1 see specific Employee
               numericInput(inputId = "n",h4("Function 1: Inquire a specific 'key' employee: Input Employee ID here"),value=1,min=1,max=10394),
               titlePanel("Information of the Employee"),
               tableOutput(outputId="text")
               ),
               
               tabPanel("Function2",
               # Function 2 Datatable
               numericInput(inputId = "n2",h4(" Function 2: See the first N talents you want to retain"),value=1,min=1,max=10394),
               DT::dataTableOutput("mytable")
               ),

              tabPanel("Function3",
               # Function 3 predict the leaving of a new employee
               
               titlePanel(h4("Function 3: See new employee's intention to leave")),
               
               # Sidebar layout with input and output definitions ----
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   
                   # Input: Textbox for the satisfcation level ----
                   numericInput(inputId = "sat",
                             label = "Input satisfaction level",
                             value = 0.5,
                             min=0,
                             max=1),
                   
                   # Input: Textbox for performance ----
                   numericInput(inputId = "per",
                             label = "Input perfromance",
                             value = 0.5,
                             min=0,
                             max=1),
                   
                   # Input: Textbox for number of monthly hours ----
                   numericInput(inputId = "hrs",
                             label = "Input number of monthly hours",
                             value = 150,
                             min=50,
                             max=1000)
                   ),
                   
                   # Action button to run
                   #actionButton("go", "Run!")
                   
               
                 # Main panel for displaying outputs ----
                 mainPanel(
                   
                   # Output: Test of text ----
                   textOutput(outputId = "Prob"),
                   textOutput(outputId = "leave")
                 )
               )
               
              ),
              
              tabPanel("Function4",
               # Function 4 see charts
               mainPanel(h4("Function 4: See charts"),
               plotOutput(outputId = "plot1"),
               plotOutput(outputId = "plot2")
               )
              )
)


server<-function(input,output) {
#Function1 Output specific employee within the 'key employee list'
  
  
  output$text = renderTable({
    validate(need(input$n,"Please input Employee ID"))
    orderpredattrition[rownames(hr[input$n,]),]
    })
  
#Function2 Output the data table
    output$mytable = DT::renderDataTable({
    datatable(orderpredattrition, options= list(lengthMenu = c(input$n2,150)))
  })	
  
#Function3 Output the predection of a new employee
  
    #makeReactiveBinding("newDf")
    
    newData <- reactive({
        
        newDf <- round(predict(logreg,newdata=rbind(newDf, data.frame("Satisfaction.Level"= input$sat
                                                                , "Last.Evaluation"= input$per
                                                                , "Avg.monthly.hours"= input$hrs )),type="response")[[1]],4)
             })
      
    output$Prob = renderText({"Probability of this Employee to leave:"}) 
    output$leave = renderText({newData()})
    
    
# Function 4 output plot  
  output$plot1 <- renderPlot({plot(predattrition$ProbaToLeave,predattrition$Performance)})
  output$plot2 <- renderPlot({plot(predattrition$ProbaToLeave,predattrition$Satisfaction)})
}

shinyApp(ui=ui,server=server)


