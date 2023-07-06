data = read.table(file="cholesterol.txt",header=TRUE)
data

#----------------------------------(a)------------------------------
t.test(data$Cholesterol,conf.level=0.99)
print("Confidence interval [180.6816,185.5204] at 99%")


#----------------------------------(b)-----------------------------
library(ggplot2)

qplot(data$Cholesterol[data$Drug=="A"],geom="histogram")
qplot(data$Cholesterol[data$Drug=="B"],geom="histogram")


t.test(data$Cholesterol[data$Drug=="A"],conf.level=0.95)
print("Confidence interval [178.6626,183.3694] at 95%")


t.test(data$Cholesterol[data$Drug=="B"],conf.level=0.95)
print("Confidence interval [182.4306,187.9414] at 95%")


#----------------------------------(c&d)------------------------------
t.test(data$Cholesterol[data$Drug=="A"],data$Cholesterol[data$Drug=="B"],conf.level=0.9)
print("Confidence interval for the mean difference [-7.164971,-1.175029] at 90%")

t.test(data$Cholesterol[data$Drug=="A"],data$Cholesterol[data$Drug=="B"],alternative = "less",p.value=0.05)
print("The Hypothesis that the cholesterol means are equals is false as the p_value=0,01144")


#-----------------------------------(e)--------------------------------
var.test(data$Glucose[data$Drug=="A"],data$Glucose[data$Drug=="B"],alternative = "two.sided",p.value=0.01)


print("The p_value=0,2731 is larger than 0,01 meaning that the Null hypothesis is true and the variance of Glucose
for the two drugs is the same, statistically speaking")



#------------------------------------(f)--------------------------------------
qplot(data$Glucose[data$Drug=="A"],geom="histogram")
qplot(data$Glucose[data$Drug=="B"],geom="histogram")

t.test(data$Glucose[data$Drug=="A"],conf.level=0.95)
print("The confidence interval for the Glucose for drug A is [89.768,94.467] at 95%")

t.test(data$Glucose[data$Drug=="B"],conf.level=0.95)
print("The confidence interval for the Glucose for drug B is [87.323,91.912] at 95%")

print("There is a noticeable increase for the drug A")


print("Lets hypothesise for the Glucose means to be equal")
t.test(data$Glucose[data$Drug=="A"],data$Glucose[data$Drug=="B"],alternative = "two.sided",var.equal = TRUE,p.value=0.95)

print("The mean Difference of Glucose of the is hight enought to be statistically significant at 5% of the times")


#------------------------------------(g)-----------------------------------------
length(which(data$Myalgia == "Yes"))

prop.test(7,100,conf.level=0.95)
print("The confidence interval for the percentage of people with Myalgia is [3.1% , 14.3%] at 95%.")


#------------------------------------(h)--------------------------------------------
prop.test(7,100, p = 0.05, alternative = "greater",conf.level=0.05)
print("The p_value=0.245 so we reject the Null Hypothesis for a=0.05. Meaning that the percentage of volunteers with Myalgia is statisticaly greater than 5%")


#------------------------------------(i)------------------------------------------
myalgiaBalance = as.table(rbind(c(44,6),c(49,1)))
chisq.test(myalgiaBalance)
print("p_value > 0.05 the Null hypothesis is accepted, meaning Myalgia symptoms are idependent to the tested drug")


print("Though if for example the only volunteer with Myalgia tested with the Drug B be was absent...")
myalgiaBalance2 = as.table(rbind(c(44,6),c(49,0)))
chisq.test(myalgiaBalance2)


print("Then the Myalgia patients and the drug testing will no longer be independent. The same would be true if 2 more of the volunteers for drug A had symptoms")
myalgiaBalance3 = as.table(rbind(c(42,8),c(49,1)))
chisq.test(myalgiaBalance3)

#------------------------------------(j)------------------------------------------
symptoms = data$Glucose[data$Myalgia=="Yes"]
noSymptoms = data$Glucose[data$Myalgia=="No"]

var.test(symptoms,noSymptoms,conf.level=0.95)
print("Barely we can say that they share the same variance")


t.test(symptoms,noSymptoms,var.equal = TRUE,conf.level=0.95)
print("The 95% confidence interval for the mean differece for Glucose between Myalgia patients and healthy volunteers is [-10.326 , 2.438]")