giants <- read.csv('baseball.csv')
library(ggplot2)
dataBefore2010 <- subset(giants, Year < 2010)

#I have set the x-axis, the optimal number of wins to 91.9 or 92 wins for simplicity
#I found this by averaging the number of wins for 1st place in the National League West from 2000-2009, which gave me 91.9
m <- ggplot(dataBefore2010, aes(x = W,
                                y = Team,
                                color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("black", "red"), name = "Made Playoffs")


m + geom_vline(aes(xintercept = 91.9), color = "blue", linetype = "dashed", size=1) + xlab("Wins")

#We will create a run differential plot to see teams that have made the playoffs with their respective run differentials, teams that made it are in red 
#Notice how many black dots are to the left of the scatterplot, Sabean and Bochy are setting their goal to the 92 wins a year
#In order to win that many games, the giants have to score runs and more importantly, outscore the other team
#In order to do this, they need to have a high run differential, so we will look at the relationship of wins and run differential

dataBefore2010$RD <- dataBefore2010$RS - dataBefore2010$RA

ggplot(dataBefore2010, aes(x = RD,
                           y = W,
                           color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")
  

#Keeping the color scheme the same, noting red teams made the playoffs, looks like if you have a high run differential, you will more than likely make the playoffs 

winsregression <- lm(W ~ RD, data = dataBefore2010)
winsregression
# Intercept is 80.8975 and Run Differential is 0.1047, thus: 
#equation is 80.8975 +0.1047RD
#W=80.8975+0.1047RD and substitute in 92 for W, the optimal amount of wins to make the playoffs 
# 92=80.9875+0.1047RD, subtract 80.8975 from both sides, and you get 11.1025=0.1047RD
# Divide both sides by 0.1047, RD= 106.04 run differential
# Thus, the Giants needed a run differential of 106 runs to win the necessary 92 games in 2010

#We will recreate the run differential and wins plot, except now with an x-intercept at the 106 runs mark, noting how much of a run differential the Giants should aim for 

ggplot(dataBefore2010, aes(x = RD,
                           y = W,
                           color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs") +
  geom_vline(aes(xintercept = 106), color = "blue", linetype = "dashed", size=1)


#This isn't the only indicator, but it seems like most of the red dots to the right made the playoffs, and the dots to the left are mostly black, denoting not making the playoffs 
#The real answer I was going for, is how many runs should the Giants score in order to get the 92 wins to make the playoffs
#This was a strategy that was implemented by the 2002 Oakland Athletics, who in an unorthodox fashion won the American League West


#Now for the real question that Sabean and Bochy have to face, is how do we score 106 more runs than we allow. 
# Since our dataset only has 3 of the significant batting statistics, batting average, on base percentage, and slugging percentage, we will focus on if these have any predictive measures on wins 

RSreg <- lm(RS ~ OBP + SLG + BA, data = dataBefore2010)
summary(RSreg)

#According to our regression, OBP and Slugging Percentage are very significant, indicated with 3 asterisks or stars
#However, this was surprising to me that batting average has 0 asterisks, which means it isn't significant.
#We are quite happy with an adjusted R-Squared of 0.9258 however, meaning the data fits well and the model is strong 
# We should try running this again, without our insignificant variable, which is Batting average 

RSregNoBA <- lm(RS ~ OBP + SLG, data = dataBefore2010)
summary(RSregNoBA)

#I found this to be quite funny, that by removing our insignificant variable which is batting average, our adjusted 
#R-squared went up to 0.9259, only 0.0001 point higher. 
#However, when doing regressions, our objective is to remove insignificant variables, while trying to make our R2 higher. In this case, removing BA has done that 

#Now, we should focus on pitching and to do this we will do the same thing, except with opponents on base percentage and opponents slugging percentage

RAreg <- lm(RA ~ OOBP + OSLG, data = dataBefore2010)
summary(RAreg)

#Both of the variables were significant on runs allowed, both indicating 3 asterisks and a pretty good R^2 of 0.8907.
#Since we have ran our regressions, it is time to find the answer to our problem that the Giants would face going into the 2010 season 
#What OBP and Slugging Percentage would they need in order to have a run differential of 106? 

#The Giants would've most likely used the 2009 statistics, since the team had not changed very much, other than signing 1B Aubrey Huff and C Bengie Molina who would later on be traded

#So, our regression equation would essentially look like this: 
#314 and 372
# RA = -885.29 + 2910.17(OOBP)+ 1613.97(OSLG)
# The giants figures in 2009 were OOBP-0.314 and OSLG-0.372
#Thus if we do the math correctly: RA=-885.29+2910.17(.314)+ 1613.97(.372)
# RA= 628.90, approximately 629 runs allowed
#Thus, if the Giants were projected to give up 629 runs in 2010 and needed a run differential of 106, they would need to score 735 runs to win 92 games in 2010
#The Giants would have to find players who raised their OBP and SLG enough to get their run differential to +106, while their run differential in 2009 was +46


#these are the following stats that the Giants would produce to win their first world series since the 1958 New York Giants 
# OBP-321 SLG-408
# OOBP -313 OSLG-370 

#Let's plug these numbers back into our equation: 
#RA = -885.29 + 2910.17(.313)+ 1613.97(.370)
#Based off this, RA would be about 623 runs allowed in 2010, while actually they gave up 583, due in part to their stellar bullpen, which might rank as one of the better ones in the past 25 years 


#Now, if we plug this in our RS regression, let's do the same thing 
#RS= -808.15 + 2809.50(.321)+ 1527.68(.408)
#RS= 717, this would give us a RD of 717-623=94, while they needed 106 to make the playoffs. 

#We can use our wins probability that we used before, W=80.8975 +0.1047(94)= approximately 91 wins in order to make the playoffs in 2010
#Thankfully, it came down to the last game of the 162 game season and the Giants and Padres both sitting at 91-70 faced off. Winner went to the playoffs and loser went home
#The Giants would win the game finishing at 92-70, a game I attended by the way, a game over the predicted 91 wins 
#The San Francisco Giants would win their first World Series in San Francisco, since moving from New York and ending a 58 year drought.













