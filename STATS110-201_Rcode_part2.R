#################################################
# Import the dataset into R from your local directory
# You will need to replace the part that is in quotations with your actual file path, such as "C://...../MedGPA.txt" or "Users/myname/..../MedGPA.txt"

mcat = read.table("./MedGPA.txt", fill=TRUE, header=TRUE)

# Can do a summary of each of the variables in this dataset as follows
summary(mcat)

# Plot a students GPA against their BCPM
plot(mcat$GPA, mcat$BCPM, xlab="Student GPA", ylab="Student BCPM", main="Scatterplot")

# Get bar graph of categorical variables
plot(mcat$Accept, ylab="Count", xlab="A=Accepted , D=Denied", main="Bar graph of accepted yes/no")

# Compute 5 number summaries, along with mean and variances
summary(mcat$GPA)
mean(mcat$GPA)
var(mcat$GPA)

summary(mcat$BCPM)
mean(mcat$BCPM)
var(mcat$BCPM)


# Now fit a model with GPA and Sex and interaction
model.mcat = lm(MCAT~GPA+Sex+GPA*Sex, data=mcat)

#################################################
# Compute the mean and 5 number summary of MCAT scores among students who were Female (Sex=="F)
summary(mcat[mcat$Sex=="F",]$MCAT)

# Compute the mean and 5 number summary of MCAT scores among students who were Male (Sex=="M)
summary(mcat[mcat$Sex=="M",]$MCAT)


# Get the regression line for Y = MCAT and X = GPA
model = lm(MCAT~GPA, data=mcat)

# To get full output, standard errors and R^2, use summary()
summary(model)

##################################################
# Homework 1 code
# Compute the mean and 5 number summary of GPA among students who were accepted
summary(mcat[mcat$Acceptance==1,]$GPA)

# Or can do this the following way
summary(mcat[mcat$Accept=="A",]$GPA)


# Compute the mean and 5 number summary of GPA among students who were not accepted
summary(mcat[mcat$Acceptance==0,]$GPA)

# Or can do this the following way
summary(mcat[mcat$Accept=="D",]$GPA)

############################################################

# Testing R or rho
# Import sibling age and height data
couple = read.table("./couple.txt", fill=TRUE, header=TRUE)

# Plot of Y=SAge and X=BAge
plot(couple $HAge, couple $WAge, ylab="Wife Age", xlab="Husband Age", main="Scatterplot of husband and wife age")

# Plot of Y=BAge and X=SAge
plot(couple $WAge, couple $HAge, ylab="Husband Age", xlab="Wife Age", main="Scatterplot of wife and husband age")

# Fit a model to get R. First get R^2
model.couple = lm(HAge~WAge, data=couple)
summary(model.couple)

# Create test statistic
n = dim(couple)[1]
Rsquared = summary(model.couple)$r.squared
R = cor(couple$HAge, couple$WAge)

t = (R*sqrt(n-2))/(sqrt(1-Rsquared))

# P-value
2*(1-pt(abs(t),n-2))

################################################
# Import the skincancer data
skincancer = read.table("./skincancer.txt", fill=TRUE, header=TRUE)

# Create a scatterplot of Y= Mortality and X=Latitude, and then overlay the linear regression line
plot(skincancer$Lat,skincancer$Mort, ylab="Mortality Rate", xlab="Latitude", main="Scatterplot of mortality against latitude")
abline(lm(Mort~Lat, data=skincancer))

# Run a linear regression with Y = Mort and X=Lat
model.skin = lm(Mort~Lat, data=skincancer)

# Get summary of model
summary(model.skin)

# Now get the needed plots such as residual vs fitted and qq plots
plot(model.skin)

# Compute R-squared manually. Get SSR, SSE, and SSTO
SSR = sum((predict(model.skin)-mean(skincancer$Mort))^2)

SSE = sum(model.skin$residuals^2)

SSTO = sum((skincancer$Mort-mean(skincancer$Mort))^2)

# Calculate R-squared
Rsquare = SSR/SSTO
Rsquare

################################################################################# 
# How to create confidence intervals for the mean of the responses at a given value of X
mcat = read.table("./MedGPA.txt", fill=TRUE, header=TRUE)

model = lm(MCAT~GPA, data=mcat)

# Using med school dataset, create a 95% confidence interval for the mean of the responses when X=GPA=4.0
predict(model, list(GPA=4),interval= "c")

# Now create a 99% interval for GPA=4.0
predict(model, list(GPA=4),interval= "c", level=0.99)

# Now to create a prediction interval.
predict(model, list(GPA=4),interval= "p")

# Now using the skin cancer dataset, create a 95% confidence interval for the mean of the responses when X=Lat=40
predict(model.skin, list(Lat=40), interval="c")

# Now create a prediction inteval for latitude=40.
predict(model.skin, list(Lat=40), interval="p")


##########################################################
# Categorical variables

# Import the smoking data
birthsmokers = read.table("./birthsmokers.txt", fill=TRUE, header=TRUE)



# Create boxplot of weight by smoker group
boxplot(birthsmokers$Wgt~birthsmokers$Smoke, main="Boxplot of Weight by Smoking Status", xlab="Smoker", ylab="Weight")

# Plot gestation lencth X vs birthweight Y for smokers and non-smokers
# And overlay regression lines with same slope different intercepts
plot(birthsmokers$Gest[birthsmokers$Smoke=="yes"], birthsmokers$Wgt[birthsmokers$Smoke=="yes"], xlab="Gestation Length", ylab="Birth Weights", col="red")

points(birthsmokers$Gest[birthsmokers$Smoke=="no"], birthsmokers$Wgt[birthsmokers$Smoke=="no"], col="blue", pch=24)

model = lm(Wgt~Gest+Smoke, data = birthsmokers)

lines(birthsmokers$Gest[birthsmokers$Smoke=="yes"], predict(model)[birthsmokers$Smoke=="yes"] , col="red")

lines(birthsmokers$Gest[birthsmokers$Smoke=="no"], predict(model)[birthsmokers$Smoke=="no"])


# Conduct a two sample t-test on birthweight (using pooled variance)
t.test(birthsmokers$Wgt[birthsmokers$Smoke=="yes"],birthsmokers$Wgt[birthsmokers$Smoke=="no"], var.equal=TRUE)

# Fit a linear regression model with only the categorical Smoke as explantory
model.smoke = lm(Wgt~Smoke, data=birthsmokers)

summary(model.smoke)
 
# Fit linear model with 2 explantory variables  
model.smoke2 = lm(Wgt~Gest+Smoke, data=birthsmokers) 

summary(model.smoke2)

# Fit linear model with 2 explantory variables and interaction
model.smoke3 = lm(Wgt~Gest+Smoke+Gest*Smoke, data=birthsmokers) 

summary(model.smoke3)


####################################################################
# Import the depression data
depression = read.table("./depression.txt", fill=TRUE, header=TRUE)

# Plot age vs y for treatments A and B
plot(depression$age[depression$TRT=="B"],depression$y[depression$TRT=="B"], xlab="Age", ylab="Effectiveness score", main="Scatterplot of age vs effectiveness", col="red", xlim=c(20,70), ylim=c(15,90))
abline(lm(depression$y[depression$TRT=="B"]~depression$age[depression$TRT=="B"]) , col="red")

points(depression$age[depression$TRT=="A"],depression$y[depression$TRT=="A"], pch=24)
abline(lm(depression$y[depression$TRT=="A"]~depression$age[depression$TRT=="A"]))
legend(50,40,legend=c("Treatment B", "Treatment A"),pch=c(1,24), col=c("red","black"))

# CAN ALSO DO THESE PLOTS USING GGPLOT
# If you do not have ggplot2, need to install it using the syntax: 
# install.packages("ggplot2")
library(ggplot2)

# Recreate plot of age on y by treatment group
ggplot(depression)
ggplot(depression, aes(age, y, colour = TRT)) + geom_point()+geom_smooth(method="lm", level=0)

# Fit model with Y=wgt and X1=Gest and X2=Smoke and interaction term
model.dep = lm(y~age+TRT+age*TRT, data=depression)
summary(model.dep)

##########################################################
# Import pulse dataset
pulse = read.table("./pulse.txt" ,  header=TRUE)

# Create Smoker Yes/No variable
pulse$Smoker = ifelse(pulse$Smoke==1, "Yes", "No")

# Plot Resting pulse rate against weight by smoker group (run all these lines together)
# May have to install ggplot2 package
library(ggplot2)
ggplot(pulse)
ggplot(pulse, aes(Wgt, Rest, color = Smoker, shape=Smoker)) + geom_point()+geom_smooth(method="lm", se=FALSE)

# The long way of doing the graph above (run all the lines together until marked "end")
plot(pulse$Wgt[pulse$Smoker=="Yes"] , pulse$Rest[pulse$Smoker=="Yes"], xlab="Wgt", ylab="Rest", main="Scatterplot of Wgt vs Rest", col="red", xlim=c(100,270), ylim=c(50,110))
abline(lm(pulse$Rest[pulse$Smoker=="Yes"]~pulse$Wgt[pulse$Smoker=="Yes"] ) , col="red")

points(pulse$Wgt[pulse$Smoker=="No"] , pulse$Rest[pulse$Smoker=="No"], pch=24)
abline(lm(pulse$Rest[pulse$Smoker=="No"]~pulse$Wgt[pulse$Smoker=="No"] ))
legend(240,110,legend=c("Smoker", "Non-Smoker"),pch=c(1,24), col=c("red","black"))
# end


##################################################

# Import MidWestSales dataset
MidWestSales = read.table("./MidwestSales.txt", fill=TRUE, header=FALSE)

# This dataset does not have names, so we will add names to the variables
names(MidWestSales)=c("id","price","sqft","bed","bath","ac","garage","pool","year","quality","style","lot","hwy")

