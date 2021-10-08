-
  ---
#In our group we decided to see which were
#the determinants of the income of the oldest workers, of those 
#between 50 and 65 years of age. 
#We also wanted to see how important it is to distinguish by gender and ethnicity. 
#As part of the exercise, we came up with a couple of groups: those who studied 
#something related to business and the rest of the people.


# First we created a subgroup 
attach(acs2017_ny)
use_varb <- (AGE >= 50) & (AGE <=65) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 40) & (DEGFIELD== "Business")
dat_use <- subset(acs2017_ny,use_varb) 
detach()
attach(dat_use)

# Next, we tested for obvious errors in the subgroup
summary(AGE)
summary(DEGFIELD== "Business")
summary(female)
summary(Hispanic)
summary(AfAm)
summary(Asian)
summary(race_oth)



#Linear Regression Model
model_temp1 <- lm(INCWAGE ~ AGE + female+ Hispanic+ Asian+ AfAm + Asian + Amindian + race_oth) 

require(stargazer)
stargazer(model_temp1, type = "text")

#confidence interval calculations
AGEl <--301.099-163.689  
AGEr <--301.099+163.689 
femalel <--22291.850-1413.806
femaler <--22291.850+1413.806
Hispanicl <--24464.040-2653.239
Hispanicr <--24464.040+2653.239
Asianl <- -9671.716-3178.407
Asianr <- -9671.716+3178.407
AfAml <--26408.360-2305.169
AfAmr <--26408.360+2305.169
Amindianl <--13521.030-13546.530
Amindianr <--13521.030+13546.530
race_othtl <--12728.930-2994.731
race_othtr <--12728.930+2994.731

require(AER)

# Plotting the subset
NNobs <- length(INCWAGE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2),  data = dat_graph)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,120000), data = dat_graph)

# Changing line to fit regression
to_be_predicted2 <- data.frame(AGE = 50:65, female = 1, AfAm = 1, Asian = 0, Amindian = 0, race_oth = 0, Hispanic = 1, DEGFIELD="Business")
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

# Changing line to fit regression
to_be_predicted5 <- data.frame(AGE = 50:65, female = 0, AfAm = 1, Asian = 0, Amindian = 0, race_oth = 0, Hispanic = 1, DEGFIELD="Business")
to_be_predicted5$yhat <- predict(model_temp1, newdata = to_be_predicted5)

lines(yhat ~ AGE, data = to_be_predicted5)

# Plotting different X variables
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,120000), data = dat_graph)

# Changing line to fit regression
to_be_predicted3 <- data.frame(AGE = 50:65, female = 0, AfAm = 1, Asian = 0, Amindian = 0, race_oth = 0, Hispanic = 1)
to_be_predicted3$yhat <- predict(model_temp1, newdata = to_be_predicted3)

lines(yhat ~ AGE, data = to_be_predicted3)

# Changing line to fit regression
to_be_predicted4 <- data.frame(AGE = 50:65, female = 1, AfAm = 1, Asian = 0, Amindian = 0, race_oth = 0, Hispanic = 1)
to_be_predicted4$yhat <- predict(model_temp1, newdata = to_be_predicted4)

lines(yhat ~ AGE, data = to_be_predicted4)



