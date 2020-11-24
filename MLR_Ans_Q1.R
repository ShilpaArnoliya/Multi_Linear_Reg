#----------Q1 MLR----------

#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and
#make a table containing R^2 value for each prepared model.

#R&D Spend -- Research and devolop spend in the past few years => X
#Administration -- spend on administration in the past few years => X
#Marketing Spend -- spend on Marketing in the past few years => X
#State -- states from which data is collected => X
#Profit  -- profit of each state in the past few years => Y

Startups <- read.csv(file.choose())
View(Startups)
class(Startups)
attach(Startups)
summary(Startups)


#we have a column name state which we Transform from Character to Numeric
state_new <- as.numeric(Startups$State)
#View(state_new)
Startups_new <- data.frame(R.D.Spend, Administration, Marketing.Spend,
                           state_new, Profit)
View(Startups_new)
attach(Startups_new)


#EDA could be prform only with numeric data so we remove state column to do
#a better EDA of data except state column
Startups_woutstate <- Startups[,-c(4)]
View(Startups_woutstate)




# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations 
#(Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

#for skewnwss & kurtosis
#install.packages("moments")
library(moments)
#for graphs
#install.packages("stats")
library(stats) 
#install.packages("lattice")# for graph
library(lattice)

#-----EDA-----
summary(Startups_new)
var(Startups_woutstate)

skewness(Startups_woutstate)
kurtosis(Startups_woutstate)
#-----Grafical Analysis-----
dotplot(R.D.Spend, main = "Dot plot of R.D.Spend.")
dotplot(Administration,  main = "Dot plot of Administration")
dotplot(Marketing.Spend, main = "Dot plot of Marketing.Spend")
dotplot(Profit,  main = "Dot plot of Profit")


boxplot(Startups_woutstate ,col="dodgerblue4", horizontal = T)


hist(Startups$R.D.Spend)
hist(Startups$Administration)
hist(Startups$Marketing.Spend)
hist(Startups$Profit)

qqnorm(R.D.Spend)
qqline(R.D.Spend)

qqnorm(Administration)
qqline(Administration)

qqnorm(Marketing.Spend)
qqline(Marketing.Spend)

qqnorm(Profit)
qqline(Profit)

plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)



### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(Startups_new ,upper.panel = panel.cor,main="Scatter plot 
      matrix with Correlation coefficients")
pairs(Startups_woutstate ,upper.panel = panel.cor,
      main="Scatter plot matrix with Correlation coefficients")

#here profit & R.D.Spend has high collinearity 


# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
#cor(Startups_new)
cor(Startups_woutstate)

# 9. Partial Correlation matrix - Pure Correlation  b/w the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups_woutstate))

#-----Model Building-----

# The linear model wrt all variables
Model_all <- lm(Profit ~ ., data = Startups_new)
summary(Model_all)

#from the summary we can say Administration and Marketing.spend doesnt 
#have much effect on profit as they holds no star 
#whereas R.D.Spend holds 3 star which sign of show good effect
#R^2 = 0.9507, Adjusted R-squared:  0.9464 

# The linear model wrt R.D.Spend
x_R.D.Spend <-lm(Profit ~ R.D.Spend )
summary(x_R.D.Spend)
#***
#R-squared:  0.9465,	
#Adjusted R-squared:  0.9454

# The linear model wrt Administration
x_Administration <- lm(Profit ~ Administration)
summary(x_Administration)
#No *
#R-squared:  0.04029,	
#Adjusted R-squared:  0.02029 

# The linear model wrt Marketing.Spend
x_Marketing.Spend <- lm(Profit~ Marketing.Spend)
summary(x_Marketing.Spend)
#***
#R-squared:  0.5592,
#Adjusted R-squared:   0.55 


#Diagnostic Plots
#install.packages(car)
library(car)

plot(Model_all)


#plot 1 = Residual vs Fitted => which is very much straight =>which indicates
#that the residuals are scattered arround the mean where the mean value is 0.

#Plot 2 = normal Q-Q plot -> which test for normality of our residuals .
#which is nicely normal

#Plot 3 = scale-location -> this plot tell us about the homoscedasticity of our data.
#ideally it should be straight. But herd it is curvy

#Plot 4 = Residuals vs Leverage -> the dotted red lines we seen there.
#If there are any data points out side this range  
#in our graph the point no point is outside the cook's distance but pont 49&50 seems unusual


#install.packages("mvinfluence")
library(mvinfluence)

library(car)

#It is better to delete a single observation rather than entire variable to get rid of 
#collinearity problem. Deletion Diagnostics for identifying influential variable

influence.measures(Model_all)
#row no 7,38,47,50 are started Rows

influenceIndexPlot(Model_all, id.n=3) 
# Index Plots of the influence measures

influencePlot(Model_all, id.n=3)
# A user friendly representation of the above
#observation 46,47,49,50are popout

infIndexPlot.mlm(Model_all)

#Regression after deleting the 49th and 50th observation, which is influential observation

Model.Startups <-lm(Profit~ ., data =Startups_woutstate[-c(49,50),])
summary(Model.Startups)
#R-squared:  0.9627,	Adjusted R-squared:  0.9601 

confint(Model.Startups,level=0.95)

predict(Model.Startups, interval="predict")

# Logarthimic Transformation 
Model.Startups_Log<-lm(Profit~ R.D.Spend +log(Administration)
                       +Marketing.Spend +log(State),data=Startups[-c(49,50),])

summary(Model.Startups_Log) 
#R-squared:  0.9625,	Adjusted R-squared:  0.9599

#----------final conclusion----------
#R-squared: 0.9507, Adjusted R-squared:  0.9464 
#R-squared: 0.9627,	Adjusted R-squared:  0.9601
#R-squared: 0.9627,	Adjusted R-squared:  0.9601    *
#R-squared: 0.9625,	Adjusted R-squared:  0.9599

#Best Model
Model.Startups <-lm(Profit~ ., data =Startups_woutstate[-c(49,50),])
summary(Model.Startups)
#R-squared:  0.9627,	Adjusted R-squared:  0.9601 
