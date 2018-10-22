## Multivariate Regression 

## Section 1: Loading Libraries ---------------------------------------------------------

## Load the "data.table", "ggplot2", and "reshape2" libraries. 
library(data.table)
library(ggplot2)
library(reshape2)

## Section 2: Regressing Years of Service on Salary  ---------------------------------------------------------

## Load the "Salaries" dataset, and convert it to a data.table. 
data("Salaries")
Salaries <- data.table(Salaries)

## Run a regression where the outcome variable is "salary" and the predictor variable is "yrs.service". 
regression_output <- lm(salary ~ yrs.service, data = Salaries)
print(regression_output)
## Run the "summary" command on your regression, and answer the following questions:

## 1. What are the coefficients for intercept and "yrs.service"?
## Coefficients: Intercept 99974.7,  yrs.service   779.6  

## 2. Put those intercepts into a sentence describing the linear relationship, like we did for the two regressions in lecture.
# A professor with zero years of service is expected to make $99,974 and every additional year of service will add $779.

## 3. What is the standard error and p-value of the "yrs.service" coefficient? Is that p-value statistically significant?
summary(regression_output)
#   Std error 110.4, p value 7.53e-12, yes

## 4. What is the adjusted R-squared value for this regression? What does that mean?
#Adjusted R-squared:  0.1098. R-squared, along with p-value, is another common statistic for assessing the goodness-of-fit of your model. the R-squared value will tell you how much variation in your outcome variable can be predicted by your predictor variable.
# about 10% of the variance in salary is explained by yrs.service
#If the data were a straight line, the R-squared value would be 1. If the data were random noise, R-squared would be close to zero (or could, in some cases, even be negative). Here we have an adjusted R-squared of 0.1098, meaning that only about 10% of the variance in salary can be explained by yrs.since.phd.

## Make a new column in your dataset with predicted salary values.
Salaries[, salary.predict:=predict(regression_output)]
print(Salaries)

## Make a scatter plot of yrs.service vs. salary, and add a line plot showing the regression predictions.
## NOTE: I want you to plot your predicted values (from the last question). Do *not* use geom_smooth().
## Fully label both axes, and add a title.
salary_plot <- ggplot(data=Salaries, aes(x=yrs.service)) + geom_point(aes(y=salary)) + geom_line(aes(y=salary.predict), color="green") + labs(title="Professor's Actual and Predicted Salary Over Years of Service", x="Years of Service", y="Salary")
print(salary_plot)

ggplot(Salaries, aes(x=yrs.service))+
  geom_point(aes(y=salary), color="orange", alpha=0.7)+
  geom_line(aes(y=predictedSalary), color="blue") +
  labs(title="Data and Regression Line for Years of Service and Salary among Professors",
       x="Years of Service",
       y="Salary (USD)")
## ---------------------------------------------------------------------------------
## Section 3: Regressing with the "tips" Dataset  ---------------------------------------------------------

## Load the "tips" dataset from the "reshape2" library, and convert it to a data.table. 
data(tips)
tips <- data.table(tips)

## What does one row of this dataset represent?
# One row represents a restaurant bill transaction and has bill amount, day of week, time (categorical bfast, lunch and dinner) and details about the customer such as number in the party, sex and smoking status.

## Run a regression where the outcome variable is "tip" and the predictor variable is "total_bill". 
regression_tip <- lm(tip ~ total_bill, data = tips)
print(regression_tip)
## Run the "summary" command on your regression, and answer the following questions:
summary(regression_tip)

## 1. What are the coefficients for intercept and "total_bill"?
#Intercept : 0.920270
# Total bill: 0.105025

## 2. Put those intercepts into a sentence describing the linear relationship, like we did for the two regressions in lecture.
# A total bill of zero dollars is expected to make 0$.92  in tips and every additional dollar of total bill will add $0.105025

## 3. What is the standard error and p-value of the "total_bill" coefficient? Is that p-value statistically significant?
#Standard error: 0.007365, p-value: <2e-16. Yes, P value is significant.

## 4. What is the adjusted R-squared value for this regression? What does that mean?
# Adjusted R-squared:  0.4544. R-squared, along with p-value, is another common statistic for assessing the goodness-of-fit of your model. the R-squared value will tell you how much variation in your outcome variable can be predicted by your predictor variable.
#If the data were a straight line, the R-squared value would be 1. If the data were random noise, R-squared would be close to zero (or could, in some cases, even be negative). Here we have an adjusted R-squared of 0.1098, meaning that only about 10% of the variance in salary can be explained by yrs.since.phd.

## 5. What are the units of the coefficients in the "Estimate" column?
#Dollars
## Make a new column in your dataset with predicted tip values.
tips[, tips.predict:=predict(regression_tip)]
print(tips)
## Make a scatter plot of total_bill vs. tips, and add a line plot showing the regression predictions.
## See note above about how to plot predicted values. Again, make sure both axes are labeled and the plot 
## has a title. 
tips_plot <- ggplot(data=tips, aes(x=total_bill)) + geom_point(aes(y=tip)) + geom_line(aes(y=tips.predict), color="red") + labs(title="Tips of Prediction", x="Total Bill", y="Tips")
print(tips_plot)

## Think about the expected relationship between "total_bill" and "tip", according to cultural norms in the US.
## How does the coefficient on "total_bill" relate to those cultural norms?


## Section 2: Exploring variables in the "tips" dataset ---------------------------------------------------------

## Load the "tips" dataset and convert it to a data.table.
data(tips)
tips <- data.table(tips)

## Let's say that we're interested in the relationship between total bill (predictor variable) and tip (outcome variable).

## For each of the other five variables in the dataset, indicate what class that variable falls into (confounder, effect modifier,
## mediator, or none of the above). Explain why you think that. Is this variable categorical or continuous?
## Should this variable be included in the regression?

#sex: confounder (since it's associated with both predictor and outcome)
#smoker: effect modifier, but would also accept confounder or irrelevant (definitely associated with outcome, more tenuously associated with predictor, if they make a good argument for why it should theoretically be irrelevant I'll accept it)
#day: confounder (associated with both predictor and outcome)
#time: irrelevant, or confounder (not strongly associated with either predictor or outcome, but you could make the case for why it might be a confounder)
#size: mediator (associated with tip and total bill, but on the causal pathway)
ggplot(tips, aes(x=total_bill, y=tip)) +
  geom_point() +
  geom_smooth(method="lm", color="black")+
  facet_grid(.~sex)+
  labs(title="Total Bill size and Tips by Sex",
       x="Total Bill ($)",
       y="Tips($)")
tips_bi <- lm(tip ~ sex, data=tips)
summary(tips_bi)
## Section 3: Designing a regression ---------------------------------------------------------

## In section 2, you decided what variables should and should not be included in your regression on the relationship 
## between total bill and tip.Write out what your regression equation should be, including all the variables
## you deemed necessary in Section 2.
## You can use "b" instead of "beta", for example: "tip = b0 + b1*total_bill + b2*sex". 
## You should have at least three predictor variables. 


## Section 4: Running that regression ---------------------------------------------------------

## Run the regression specified in Section 3. Print the output using summary().
regression_multi_tips <- lm(formula = tip ~ total_bill + size + day, data = multi_tips)
summary(regression_multi_tips)
## What category/categories is your Intercept value capturing? For example, in our "full regression" video from 
## lecture, our Intercept value captured both the category "sex=Female" and the category "discipline=A".
# The intercept is capturing the category value of "day=Friday".

## For each row of the regression output, tell me:
## --If the coefficient in question refers to a continuous or categorical variable (and if categorical, what category)
## --How to interpret that value in a sentence. 
##    For continuous variables, this should be in the format of "an [x] increase in [predictor variable] 
##    leads to a [y] increase/decrease in my outcome variable. 
##    For categorical variables, tell me how the Intercept changes for each categorical variable. 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.745787   0.281343   2.651  0.00857 ** - a bill of $0 is expected to generate $0.75 in tips.
# total_bill   0.092994   0.009239  10.065  < 2e-16 *** - continuous - an increase of $1.00 in total_bill is expected to generate
#                                                                      an increase of $0.09 in tips.
# size         0.187132   0.087199   2.146  0.03288 *   - continuous - an increase of 1 person in party size is expected to generate
#                                                                      an increase of $0.19 in tips.
# daySat      -0.124658   0.259746  -0.480  0.63172     - categorical - if the bill is on a Saturday, it's expected to generate $0.13
#                                                                      less in tips than if it were on Friday.
# daySun      -0.013498   0.266391  -0.051  0.95963     - categorical - if the bill is on a Sunday, it's expected to generate $0.14
#                                                                      less in tips than if it were on Friday.
# dayThur     -0.077493   0.268534  -0.289  0.77316     - categorical - if the bill is on a Thursday, it's expected to generate $0.08
#                                                                      less in tips than if it were on Friday.

## NOTE: You **do not** have to work through every possible combiations of categories and give me their intercept values--
## depending on which variables you choose, that could wind up being a lot of combinations. Just tell me how each categorical
## coefficient changes the intercept. 

#1) The coefficient for total_bill refers to a continuous variable. A $1 increase in the total_bill 
## leads to an increase in the tip amount by $0.093205
## 2) The coefficient for sex refers to a categorical variable and indicates the Male category.
##    If the sex of the bill payer is Male, the tip amount reduces by $0.032595
## 3) The coefficents for day refer to a categorical variable and indicate the Sat, Sun and Thur categories. 
##    The 'Fri' category is represented by absence i.e. if the bill is paid on a Friday, then none of the coefficients will apply.
##    If the bill is for Sat, the tip amount reduces by $0.120244 (~12 cents), if the bill is for Sun, the tip amount
##    reduces by $0.006392 and if the bill is for Thur, then the tip amount reduces by $0.078854 (~7cents)
## 4) The coefficnets for size refer to a continuous variable. An increase in size of party by 1, leads to 
##    an increase in the tip amount by $0.186741 (~18 cents)

## Section 5: Visualization ---------------------------------------------------------

## Make a plot of your regression outputs. If you included continuous variables, make one plot where the x-axis is 
## "total_bill" and another where it is your continuous variable. The results won't be straight lines, but that's ok.
## If you only have one categorical variable, distinguish its values by color. If you have multiple categorical variables,
## Use some combination of color and faceting to come up with the appropriate number of regression lines. 
tips[,predicted_tip := predict(tip.regression, data=tips)]

visualization <- ggplot(tips, aes(x=total_bill)) +
  geom_point(aes(y=tip, color=sex), alpha=0.7) +
  geom_line(aes(y=predicted_tip, color=sex), size=1) +
  facet_grid(.~smoker, scales="free") +
  labs(title="Multivariate Regression with Categorical Variables: Sex and Smoking", x="Total Bill", y="Tip Amount")

print(visualization)

Interesting vizualizations
tips[, predicted_tips_total_bill:= predict(combined_regression, data=tips)]
print(tips)


## Plot predicted values - sex
ggplot(tips, aes(x=total_bill)) +
  geom_point(aes(y=tip, color=sex), alpha=0.7) +
  geom_line(aes(y=predicted_tips_total_bill, color=sex), size=1) +
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")

## Plot predicted values - sex, Factetted by sex
ggplot(tips, aes(x=total_bill)) +
  geom_point(aes(y=tip, color=sex), alpha=0.7) +
  geom_line(aes(y=predicted_tips_total_bill), size=1) +
  facet_grid(.~sex, scales="free") + 
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")

#Plot Tip v Bill, Faceting by Day and Time, Sex by color of plot, Best fit line 
faceting <- ggplot(data=tips, aes(x=total_bill, y=tip)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method="lm", color="black") +
  # adding a facet_wrap component
  facet_grid(time~day) +
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")
print(faceting)

#Plots Tip v Bill, Faceting Sex by Time and Day, Best fit Line
faceting <- ggplot(data=tips, aes(x=total_bill, y=tip)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method="lm", color="black") +
  # adding a facet_wrap component
  facet_grid(time~day~sex) +
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")
print(faceting)

#Plots Tip v Bill, Faceting Sex by Time and Day, Geomteric Line
faceting <- ggplot(data=tips, aes(x=total_bill, y=tip)) +
  geom_point(aes(color=sex)) +
  geom_line(color="black") +
  # adding a facet_wrap component
  facet_grid(time~day~sex) +
  labs(title="Multivariate Regression with Categorical Variables: Sex - Time and Day",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")
print(faceting)
# Note there is only one Thursday Dinner

#Plots Tip v Bill, Faceting Sex by Time and Day, Geomteric Line & Best fit line
faceting <- ggplot(data=tips, aes(x=total_bill, y=tip)) +
  geom_point(aes(color=sex)) +
  geom_line(color="black") +
  geom_smooth(method="lm", color="green") +
  # adding a facet_wrap component
  facet_grid(time~day~sex) +
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")
print(faceting)
# Note there is only one Thursday Dinner

#Plots Tip v Bill, Faceting by Day and Time, Sex by color of plot, Geomteric Line & Best fit line
faceting <- ggplot(data=tips, aes(x=total_bill, y=tip)) +
  geom_point(aes(color=sex)) +
  geom_line(color="black") +
  geom_smooth(method="lm", color="green") +
  # adding a facet_wrap component
  facet_grid(time~day) +
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")
print(faceting)
# Note there is only one Thursday Dinner

#Plots Tip v Bill, Faceting by Day and Sex, Time by color of plot, Best fit line
faceting <- ggplot(data=tips, aes(x=total_bill, y=tip)) +
  geom_point(aes(color=time)) +
  geom_smooth(method="lm", color="black") +
  # adding a facet_wrap component
  facet_grid(sex~day) +
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")
print(faceting)

#Plots Tip v Bill, Faceting by Day and Sex, Time by color of plot, Geomteric Line & Best fit line
faceting <- ggplot(data=tips, aes(x=total_bill, y=tip)) +
  geom_point(aes(color=time)) +
  geom_line(color="black") +
  geom_smooth(method="lm", color="green") +
  # adding a facet_wrap component
  facet_grid(sex~day) +
  labs(title="Multivariate Regression with Categorical Variables: Day, Sex & Time",
       x="Total Bill (USD)",
       y="Tip Amount (USD)")
print(faceting)




