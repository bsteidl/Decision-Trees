
#HW Due Wednesday April 21 11:59 PM
############################## Decision Tree with party
library(party)
data(airquality)
aq1 <- na.omit(airquality)
aq1$Month <- as.factor(aq1$Month)
aq1$OzoneRange <- cut(aq1$Ozone, breaks=6, labels=c("VVL", "VL", "L", "M", "H", "VH"))
#Very Very Low, Very Low, Low, Medium, High, Very High
View(aq1) #eyeball the data

#Q0
#Q0.1 Explain what does the aq1$OzoneRange <- cut(...) Code do.
#The code is dividing the range of x into intervals and coding them according to which x value they have.

#Q0.2 Write the R Code to plot the barchart of the OzoneRanges
library(ggplot2)
windows()
ggplot(aq1, aes(OzoneRange))+geom_bar()

#Q1 
##1.1 Write the R commands to build and plot a conditional Inference tree (ctree) on aq1, 
# call it aq1Ct1 to classify the OzoneRange based on Solar.R, Wind, Temp and Month
windows()
aq1Ct1<- ctree(OzoneRange~Solar.R+Wind+Temp+Month, data=aq1)
plot(aq1Ct1)

#1.2 write the code to perform the extended and the simple type plots
plot(aq1Ct1, type="simple")
plot(aq1Ct1, type="extended")


##1.3 Perform a detailed analysis of terminal Node 6.
plot(aq1Ct1, drop_terminal=FALSE)


##1.4 Issue the following commands to cross tabulate the actual VS the predicted OzoneRange)
table(aq1$OzoneRange)
table(aq1$OzoneRange, predict(aq1Ct1))

## What percentage of the data in each group were misclassified
#Do the Math in R
abs(table(predict(aq1Ct1))-table(aq1$OzoneRange))/table(aq1$OzoneRange)*100

##1.5 Write the commands that adds the following columns to the aq1 dataframe as follows
aq1$PredictedOzone <- predict(aq1Ct1) #to add the redicted Ozone Range
aq1$Node <- predict(aq1Ct1) #to add the the node the recodes belong to
aq1$Prob <- predict(aq1Ct1) #to add the probability

##1.6 Interpret the meaning of Prob values for the following record, was it classified properly or was it misclassified?
aq1[90,]$Prob
#Properly.

aq2<- na.omit(airquality)
View(aq2)

#############Let us build a different classification Model, you can clenup if you wish
#Q2 A Different Prediction Model
aq2<- na.omit(airquality)
View(aq2)

##2.1 Issue the R-Commands to build and plot a conditional Inference tree (ctree) 
# on the aq2 dataset call it aq2Ct2 to Classify the Ozone based on 
# (Solar.R, Wind, Temp and Month) and plot the new Decision Tree.
aq2Ct2<- ctree(Ozone~ Solar.R+Wind+Temp+Month, data = aq2)
windows()
plot(aq2Ct2)

##2.2 In the plots, explain why the shape of the terminal nodes in aq2 is 
# different from those in aq1 (Q1)
#They are different because we are classifying two different dataframes. In Ozone there is 1 variable to plot on the x axis instead
#of multiple in OzoneRange.

##2.3 Explain the content of terminal node 6, and in detail again
nodes(aq2Ct2,6)

##2.4 Generate a record that takes the median of Solar.R, Wind, Temp and Month 12
newRec <-as.data.frame(list(median(aq2$Solar.R), median(aq2$Wind), median(aq2$Temp), 9)) #Your code goes here and below as needed

newRec
names(newRec)<- c("Solar.R", "Wind", "Temp", "Month")
class(newRec$Solar.R) <- class(aq2$Solar.R)
class(newRec$Wind) <- class(aq2$Wind)
class(newRec$Temp) <- class(aq2$Temp)
class(newRec$Month) <- class(aq2$Month)
newRec

##2.5 Write the Code that predicts the new record
aq2$PredictnewRec<- predict(newRec)
view(aq2)
?nodes

#Q3 
aq3<- na.omit(airquality)
aq2Ct3 <- ctree(Ozone~Solar.R+Wind+Temp+Month,aq3)
#Write the code that uses the panel functions that shows the density function of the data for the 
#inner-nodes and the boxplot function for the terminal nodes
#You may need to check the Panel Generating Functions of the latest party package
#https://cran.r-project.org/web/packages/party/index.html
windows()
plot(aq2Ct3, inner_panel = node_density, terminal_panel = node_boxplot)