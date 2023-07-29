houshold_data<- read.csv("zeta.csv")

#remove all meanhouseholdincome duplicate rows of data (only females’ records will be in the dataset).

houshold_data = subset(houshold_data, houshold_data$sex == 'F')

#Remove the columns zcta and sex
houshold_data = subset(houshold_data, select = -c(zcta, sex))


#Remove outliers
##8 < meaneducation < 18
houshold_data = subset(houshold_data, meaneducation <18 & meaneducation >8)
##10,000 < meanhouseholdincome < 200,000
houshold_data <- subset(houshold_data, meanhouseholdincome <200000 & meanhouseholdincome >10000)
##0 < meanemployment < 3
houshold_data <- subset(houshold_data, meanemployment <3 & meanemployment >0)
##20 < meanage < 60
houshold_data <- subset(houshold_data, meanage <60 & meanage >20)


# Create a variable called log_income and took it log10.
houshold_data$log_income <- log10(houshold_data$meanhouseholdincome)

#Rename the columns meanage, meaneducation, and meanemployment as age, education, and employment
names(houshold_data)[names(houshold_data)=="meanage"] <- "age"
names(houshold_data)[names(houshold_data)=="meaneducation"] <- "education"
names(houshold_data)[names(houshold_data)=="meanemployment"] <- "employment"



# Create a scatter plot showing the effect age has on log_income and paste it here
library(ggplot2)
ggplot(houshold_data,aes(x= age, y=log_income)) +geom_point(alpha=0.2) +labs(x="Age",y="Log_Income",title="Scaterrplot Log_Income vs Age")

#correlation
cor(houshold_data$age, houshold_data$log_income)



# Create a linear regression model between log_income and age. What is the interpretation of the t-value? What kind of t-value would indicate a significant coefficient?
linearMod <- lm(log_income ~ age, houshold_data)
print(linearMod)
summary(linearMod)



# View a detailed summary of the model
summary(linearMod)  



# Create a scatter plot showing the effect education has on log_income
ggplot(houshold_data,aes(x= education, y=log_income)) +geom_point(alpha=0.2) +labs(x="Education",y="Log_Income",title="Scaterrplot Log_Income vs Education")


#Analyze a detailed summary of a linear regression model between log_income and education
linearMod2 <- lm(log_income ~ education, houshold_data)
summary(linearMod2)



#Analyze a detailed summary of a linear regression model between the dependent variable log_income, and the independent variables age, education, and employment
linearMod3 <- lm(log_income ~ education + age + employment, houshold_data)
summary(linearMod3)




# a graph that contains a y = x line and uses the multiple regression model to plot the predicted data points against the actual data points of the training set. 
ggplot() + geom_point(aes(x= houshold_data$log_income, y=fitted(linearMod3)), alpha=0.5) + geom_line(aes(x=houshold_data$log_income, y= houshold_data$log_income), col = 'yellow') +labs(x="Actual income", y="Predicted income")


