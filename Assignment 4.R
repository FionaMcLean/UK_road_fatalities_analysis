#QUESTION 1
#Neither of the hypothesis are true 
#exp(𝑈𝑖) = 2 or 3 - could see a state with triple the smoking 
#

smokeFile = Pmisc::downloadIfOld("http://pbrown.ca/teaching/appliedstats/data/smoke.RData")
#Loading required namespace: R.utils
load(smokeFile)
smoke = smoke[smoke$Age > 9, ] #getting rid of 9 year olds since data is suspicious
forInla = smoke[, c("Age", "Age_first_tried_cigt_smkg",
                    "Sex", "Race", "state", "school", "RuralUrban")]
forInla = na.omit(forInla)
forInla$school = factor(forInla$school) #censoring problem since we do not know when they will try smoking
library("INLA")
forSurv = data.frame(time = (pmin(forInla$Age_first_tried_cigt_smkg,
                                  forInla$Age) - 4)/10, event = forInla$Age_first_tried_cigt_smkg <=
                       forInla$Age) #recode the variables, impossible to start smoking before 4- put time columns in range from 0 to 1
#if you havent smoked yet, you get your age. If you started smoking, its the age when you first started smoking
# left censoring
forSurv[forInla$Age_first_tried_cigt_smkg == 8, "event"] = 2
cbind(forInla$Age, forInla$Age_first_tried_cigt_smkg, forSurv)[1:10,]
smokeResponse = inla.surv(forSurv$time, forSurv$event) #event 0,1,2 -> 2 means left censoring, means you could have tried smoking anytime between 4 and 8, event 0 means havent smoked yet, 1 means they did smoke  

fitS2 = inla(smokeResponse ~ RuralUrban + Sex * Race +
               f(school, model = "iid", hyper = list(prec = list(prior = "pc.prec",
              param = c(4*log(1.5), 0.05)))) + f(state, model = "iid",  #within four standard deviations, within the worst and best school we expect to see at most a 50% difference in smoking
              hyper = list(prec = list(prior = "pc.prec", param = c(log(10), #exponential dist with median 2.5 - exp(2.5), P(x>10) =.02
              0.02)))), control.family = list(variant = 1,
              hyper = list(alpha = list(prior = "normal", param = c(log(1),
              .7^(-2))))), control.mode = list(theta = c(8,
              2, 5), restart = TRUE), data = forInla, family = "weibullsurv",
             verbose = TRUE, control.compute=list(config = TRUE))

exp(qnorm(c(0.025, 0.5, 0.975), mean = log(1), sd = .7)) #shape parameter - doesnt allow for more than four and median is 1 - allows alpha to be one and does not allow alpha to be 4


rbind(exp(-fitS2$summary.fixed[, c("mean", "0.025quant",
                              "0.975quant")]), Pmisc::priorPostSd(fitS2)$summary[,
                                                                                c("mean", "0.025quant", "0.975quant")])
fitS2$summary.hyper[1,c(1,2,3,5)] #since the mean of the posterior distribution of the alpha paramter is 3, and 1 is not in the credible interval, we do not have a flat hazard, the hazard is increasing. There is no evidence that as age increases, the propesity to smoke is constant  

#we can see that the mean of the posterior distribution for the standard deviation for the log relative rate of school is .144 and the mean for the posterior distribution of the standard deviation of for the log relative rate of state is .0584. Notice the C.I do not overlap
#therefore there is higher deviation within schools than states. We have a wide CI interval for state, the the std could be as low as .05 or as high as .10, but its definitley lower than school, whose lowest possible std is .11

#the rate at which rural children start smoking is 1-.8957% faster than non rural children

#positive B -> higher risk

#schools that are 1 std. apart have a clock that runs 14.39% faster, rate at which children start smoking is 14.39% faster than other schools

#the rate at which children start smoking is between 2.6% and 10.23% faster in some states when compared to other states
#so there is more variation among schools than states 

#If the model parameter is greater than 1, then the clock runs slower for that group. If the model 
#parameter is less than one, then the rate at which children start smoking in faster

#prior and posterior plots
sdState = Pmisc::priorPostSd(fitS2)
do.call(matplot, sdState$school$matplot) # prior and posterior density for school

sdState = Pmisc::priorPostSd(fitS2)
do.call(matplot, sdState$state$matplot)# prior and posterior density for state

#creating a chart to see the deviation between schools
library(dplyr)
library(ggplot2)
library(magrittr)
school_sum <- forInla[!is.infinite(forInla$Age_first_tried_cigt_smkg),]
school_sum <- school_sum %>% 
  group_by(school) %>%
  summarize(avg = mean(Age_first_tried_cigt_smkg))
school_sum <- school_sum[order(school_sum$avg),]
ggplot(data=school_sum, aes(x= reorder(school, avg), y= avg))+ geom_point()+ylab("Average Age First Try Smoking")+
  xlab("School")+ggtitle("Average Age First Start Smoking Per School")+ theme(axis.text.x = element_blank(), axis.ticks = element_blank())

#creating a map of mean age trying cigarettes in each state
library(openintro)
new_inla <- forInla[!is.infinite(forInla$Age_first_tried_cigt_smkg),]
new_inla[,"state"] <- tolower(abbr2state(new_inla$state))
colnames(new_inla)[colnames(new_inla)=="state"] <- "region"

new_inla <- new_inla %>% 
  group_by(region) %>%
  summarize(avg = mean(Age_first_tried_cigt_smkg))

states <- map_data("state")

dt2 <- left_join(states, new_inla, by = "region")


ggplot(data = dt2, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=avg))+
  geom_path()+ 
  scale_fill_gradientn(colours = rev(terrain.colors(5)),na.value = "grey",
                       guide = guide_colourbar(barwidth = 25, barheight = 0.4,
                                               #put legend title on top of legend
                                               title.position = "top")) +
  # map scale
  ggsn::scalebar(data = dt2, dist = 500, dist_unit = "km",
                 border.size = 0.4, st.size = 4,
                 box.fill = c('black','white'),
                 transform = TRUE, model = "WGS84") + 
  # put legend at the bottom, adjust legend title and text font sizes
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),  # font size of the legend 
        legend.text=element_text(size=10),
        axis.title.x=element_blank(),  # remove axis, title, ticks
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line=element_blank())+ggtitle("Map of US States Avg Age Start Smoking")




#QUESTION 2

pedestrainFile = Pmisc::downloadIfOld("http://pbrown.ca/teaching/appliedstats/data/pedestrians.rds")
pedestrians = readRDS(pedestrainFile)
pedestrians = pedestrians[!is.na(pedestrians$time),
                          ]
pedestrians$y = pedestrians$Casualty_Severity == "Fatal"
pedestrians$timeCat = format(pedestrians$time, "%Y_%b_%a_h%H") #create a strata from time and weather and date -> same weather, same month, and same day and same hour
pedestrians$strata = paste(pedestrians$Light_Conditions,
                           pedestrians$Weather_Conditions, pedestrians$timeCat)
length(unique(pedestrians$strata)) #there are 176391 different strata 
#remove strata with no cases or no controls - otherwise we have no idea what is going on 
theTable = table(pedestrians$strata, pedestrians$y) #this table tells us if there has been a person with injury or not in each strata - notice the first 5 are not helpful, we need one case and once control in each strata
#since we match on weather, time, etc, we do not have to worry about the fact that more men are out at night
onlyOne = rownames(theTable)[which(theTable[, 1] ==
                                     0 | theTable[, 2] == 0)]
x = pedestrians[!pedestrians$strata %in% onlyOne, ] #25-36 men is the baseline group 

library("survival")
theClogit = clogit(y ~ age + age:sex + strata(strata),
                   data = x)

#summary(glm(y ~ sex + age:sex + strata(strata)+ Light_Conditions + Weather_Conditions,
 #           data = x, family = "binomial"))$coef[1:4, ] 

#IMPORTANT -given that your an accent, a 2 means that there is a double probability that it is fatal compared to age 26, this is conditioning on being in an accident (so 50 twice as likely to be in fatal)
#theCoef[,c(1,2,6,7)]
#females is relative to males of the same age 
#more males its relative to the baseline
#throwing away assumptions by not making assumptions, but that's okay since it is a big data set
#by saying that 2am cloudy and 2am partly cloudy is different we are getting better estimates 
#conditioning on the number of controls - shouldn't effect the results, the more controls the better


theCoef = rbind(as.data.frame(summary(theClogit)$coef),
                `age 26 - 35` = c(0, 1, 0, NA, NA))
theCoef$sex = c("Male", "Female")[1 + grepl("Female",
                                            rownames(theCoef))]
theCoef$age = as.numeric(gsub("age|Over| - [[:digit:]].*|[:].*",
                              "", rownames(theCoef)))
theCoef = theCoef[order(theCoef$sex, theCoef$age),

                                    ]
matplot(theCoef[theCoef$sex == "Male", "age"], exp(as.matrix(theCoef[theCoef$sex ==
                                                                       "Male", c("coef", "se(coef)")]) %*% Pmisc::ciMat(0.99)),
        log = "y", type = "l", col = "black", lty = c(1,2, 2), xaxs = "i", yaxs = "i", xlab = "Age", ylab="Odds of Fatal Accident Compared to Baseline")


matplot(theCoef[theCoef$sex == "Female", "age"], exp(as.matrix(theCoef[theCoef$sex ==
                                                                         "Female", c("coef", "se(coef)")]) %*% Pmisc::ciMat(0.99)),
        log = "y", type = "l", col = "black", lty = c(1,
                                                      2, 2), xaxs = "i", xlab="Age", ylab= "Odds of Fatal Accident Compared to Male of Same Age")
#the plot for women is compared to men, so this looks like 70 year old women and men are about the same 
# given that a man is in an accident, the man at age x, the odds of being in a fatal accident is y times higher than those of age 25-30
#the odds of woman being in fatal accident, given they are in an accident, is the rate times the odds of the man at the given age. 






