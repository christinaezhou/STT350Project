library(readr)
proj <- read_csv("Downloads/finalproject.csv")

N<- length(proj$Timestamp)

###estimating the proper sample size ###### 
### we estimate that the proportion of students who don't think there are enough healthy options is ~80% ### 
### estimate p hat at 0.8 ##### 
#varphat <- (1 - 50 / N) * ((0.8*0.2) / (50 -1))

B <- 0.05
d<- B^2 / 4 

n = (N*(0.8)* (0.2)) /((N-1 ) * d + (0.8) * (0.2))



#choose sample size of 50 from N observations 
n = 76
set.seed(4000)
sample = sample(1:N, n)

sset<- proj[sample, ]

weeklyEats <- sset$`How many times a week do you eat  / consider eating on campus`

o<- which(weeklyEats == "1-2")
t<-which(weeklyEats == "3-5")
s<-which(weeklyEats == "6+")

#rounding down 
avgEat <- c(rep(1, length(o)), rep(3, length(t)), rep(6,length(s)))
#mean and variance of the average number of times people eat on campus 
y1bar <-mean(avgEat)
var1<- var(avgEat)

varhat <- (1 - (n)/ N) * (var1 / n)
bound = 2 * sqrt(varhat)

y1bar + bound 
y1bar - bound

tauybar <- y1bar * 16747
###################################################333
oncampus <- which(sset$`Do you live on campus now or have you ever lived on campus?` == "Yes, currently") 
off <- which(sset$`Do you live on campus now or have you ever lived on campus?` == "No, but I used to")
never<- which(sset$`Do you live on campus now or have you ever lived on campus?` == "No,  I have never")

n1<-length(oncampus)
n2<-length(off)
n3<- length(never)



####################################################################################333
#proportion of students who live on/ off campus  & think healthy 

onCAndH <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "Yes, currently") & (sset$`Do you agree that there are sufficient healthy food options on campus` == "agree" | sset$`Do you agree that there are sufficient healthy food options on campus` == "slightly agree")) 
ni1 <- length(onCAndH) 
prop1 <- ni1/ n1

offCAndH <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "No, but I used to") & (sset$`Do you agree that there are sufficient healthy food options on campus` == "agree" | sset$`Do you agree that there are sufficient healthy food options on campus` == "slightly agree"))
ni2 <- length(offCAndH) 
prop2 <- ni2/ n2

neverCandH <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "No,  I have never") & (sset$`Do you agree that there are sufficient healthy food options on campus` == "agree" | sset$`Do you agree that there are sufficient healthy food options on campus` == "slightly agree"))
ni3 <- length(neverCandH) 
prop3 <- ni3/ n3


#not healthy 
onCAndNH <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "Yes, currently") & (sset$`Do you agree that there are sufficient healthy food options on campus` == "disagree" | sset$`Do you agree that there are sufficient healthy food options on campus` == "slightly disagree")) 
ni4 <- length(onCAndNH) 
prop4 <- ni4/ n1

offCAndNH <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "No, but I used to") & (sset$`Do you agree that there are sufficient healthy food options on campus` == "disagree" | sset$`Do you agree that there are sufficient healthy food options on campus` == "slightly disagree"))
ni5 <- length(offCAndNH) 
prop5 <- ni5/ n2

neverCandNH <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "No,  I have never") & (sset$`Do you agree that there are sufficient healthy food options on campus` == "disagree" | sset$`Do you agree that there are sufficient healthy food options on campus` == "slightly disagree"))
ni6 <- length(neverCandNH) 
prop6 <- ni6/ n3


#neutral 

onCandN <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "Yes, currently") & sset$`Do you agree that there are sufficient healthy food options on campus` ==  "neither agree or disagree")
ni7 <- length(onCandN) 
prop7 <- ni7/ n1

offCandN <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "No, but I used to") & sset$`Do you agree that there are sufficient healthy food options on campus` ==  "neither agree or disagree")
ni8 <- length(offCandN) 
prop8 <- ni8/ n2

neverCandN <- which((sset$`Do you live on campus now or have you ever lived on campus?` == "No,  I have never") & sset$`Do you agree that there are sufficient healthy food options on campus` ==  "neither agree or disagree")
ni9 <- length(neverCandN) 
prop9 <- ni9/ n3


#estimate the overall proportion of students who think there are healthy options 

pst <-(1 / n) *((prop1 * n1) + (prop2 * n2) + (prop3 * n3))

varpst <- 1/n^2 * ((n1^2) * (1 - (ni1/n1))*(prop1*(1-prop1)/ (ni1 -1)) + 
            (n2^2) * (1 - (ni2/n2))*(prop2*(1-prop2)/ (ni2 -1)) +
              (n3^2) * (1 - (ni3/n3))*(prop3*(1-prop3)/ (ni3 -1)))

boundpst =  2*sqrt(varpst)

pst + boundpst 
pst - boundpst


#do dietary restrictions affect the proportion of students who think there are enough healthy options 
v<- which((sset$`Do you have dietary limitations or preferences` != "No"))
diet <- which((sset$`Do you have dietary limitations or preferences` != "No") & (sset$`Do you agree that there are sufficient healthy food options on campus` == "agree" | sset$`Do you agree that there are sufficient healthy food options on campus` == "slightly agree")) 

length(diet) / n
# there are length(v) number of people with dietary limitations
# there are length(diet) number of people who think there are enough healthy food options 


##looking at wait time, and times you eat on campus ## 

time1 <- which(sset$`How many times a week do you eat  / consider eating on campus` == "1-2")
time3 <- which(sset$`How many times a week do you eat  / consider eating on campus` == "3-5")
time6 <- which(sset$`How many times a week do you eat  / consider eating on campus` == "6+")

waitYes <- which(sset$`Does wait time affect your restaurant choice when eating on campus?`== "Yes")
waitNo <- which(sset$`Does wait time affect your restaurant choice when eating on campus?`== "No")

ptime <- length(waitYes)/ n

vartime <- (1 - ( n / N )) * (ptime * (1-ptime) / (n-1))

btime = 2 * sqrt(vartime)

ptime -btime 
ptime +btime
