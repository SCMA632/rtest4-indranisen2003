df.score = read.csv('IPL Ball-by-Ball 2008-2020.csv', header=TRUE)
names(df.score)
View(df.score)
dim(df.score)

library(dplyr)
runs  = df.score %>%
  group_by(batsman, id)%>%
  summarize(score = sum(batsman_runs))
View(runs)
unique(runs$batsman)
dim(runs)

runs$round = substr(runs$id, start = 1, stop = 3)
View(runs)
str(runs$round)
unique(runs$round)
#SUBSETTING

rahul = runs[runs$batsman=='KL Rahul',]
hist(rahul$score)

chopra = runs[runs$batsman=='A Chopra',]
hist(chopra$score)

mishra = runs[runs$batsman=='A Mishra',]
hist(mishra$score)

plot(rahul$score, type='l', col='blue')


rahul$time = 1:length(rahul$score)

#PROBABILITY AND FREQUENCY
hist(rahul$score,20)
plot(density(rahul$score))



# Bowlers

wickets  = df.score %>%
  group_by(bowler, id)%>%
  summarize(score = sum(is_wicket))
View(wickets)

calynn = wickets[wickets$bowler=='CA Lynn',]
hist(calynn$score)  

steyn = wickets[wickets$bowler=='DW Steyn',]
hist(steyn$score)

plot(calynn$score, type='l', col='blue')
plot(steyn$score, type='l', col='blue')

calynn$time = 1:length(calynn$score)
steyn$time = 1:length(steyn$score)


##continuous distribution for run
install.packages("fitdistrplus")

##discrete distribution for wickets
install.packages("vcd")
library(vcd)

library(MASS)

library(fitdistrplus)

descdist(rahul$score)
#Interpretations
#From the cullen and frey graph, we can see the observation at a skewness of almost 1 with a kurtosis of 3.
#It is close to the gamma line, so it is a weibull distribution.


#CA Lynn has not taken any wickets
# this seems to be because he is a batsman, not a bowler
# There are no visualizations I can make as he has not debuted in a match as a bowler yet.


########

