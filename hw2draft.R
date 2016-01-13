#Spencer Klinge
#Thrusday Sept 25th, 2015
#ISTA370 - Kumar
#Homework #3

#***********************************

ChickWeight<- ChickWeight

#Within function
SS_within<-function(group){ #could be very wrong
  x<-0
  m<-mean(group)
  for(i in 1:length(group)){
    x=x+(group[i]-m)^2/(length(group))}
  return(x)
}
popvar<-function(x){
  n<-length(x)
  (((n-1) / n) * var(x))}
#cylinder groups
c4 <- mtcars$mpg[which(mtcars$cyl==4)]
c6 <- mtcars$mpg[which(mtcars$cyl==6)]
c8 <- mtcars$mpg[which(mtcars$cyl==8)]

#diet groups 1-4
d1 <- ChickWeight$weight[which(ChickWeight$Diet==1)]
d2 <- ChickWeight$weight[which(ChickWeight$Diet==2)]
d3 <- ChickWeight$weight[which(ChickWeight$Diet==3)]
d4 <- ChickWeight$weight[which(ChickWeight$Diet==4)]



g3 <- mtcars$mpg[which(mtcars$gear==3)]
g4 <- mtcars$mpg[which(mtcars$gear==4)]
g5 <- mtcars$mpg[which(mtcars$gear==5)]
#part 1
(popvar(mtcars$mpg)-SS_within(c4)+SS_within(c6)+SS_within(c8))/popvar(mtcars$mpg)
#[1]70% of the variance within mpg is due to cylinders

popvar(mtcars$mpg)/(popvar(mtcars$mpg)- SS_within(g3)+SS_within(g4)+SS_within(g5))
#[2]41% due to gears

#********************PART 2*****************************
#******************************************************

SS_between <- function(group, grandmean, sub_group){
  length(unique(group))*((mean(sub_group)-mean(mtcars$mpg))^2)}



#*******************************************
between_gear <- SS_between(mtcars$mpg, mean(mtcars$mpg), g3) + SS_between(mtcars$mpg, mean(mtcars$mpg), g4) + SS_between(mtcars$mpg, mean(mtcars$mpg), g5)

within_gear <-SS_within(g3) + SS_within(g4) + SS_within(g5)

total_gear <- between_gear + within_gear

between_cyl <- SS_between(mtcars$mpg, mean(mtcars$mpg), c4) + SS_between(mtcars$mpg, mean(mtcars$mpg), c6) + SS_between(mtcars$mpg, mean(mtcars$mpg), c8)

within_cyl <-SS_within(c4) + SS_within(c6) + SS_within(c8)

total_cyl <- between_cyl + within_cyl



MS_between<- function(group, between){
  between/ (length(unique(group)-1))
}

MS_within<- function(group, within){
 x<- within[1]/(length(group)-length(unique(group)))
 return(x)
}


MS_within(mtcars$gear, within_gear)
#2.469 ms_within gear
MS_within(mtcars$cyl, within_cyl)
#.91 ms within cylinder
MS_between(mtcars$gear, between_gear)
#310.6006 ms between gear
MS_between(mtcars$cyl, between_cyl)
#568.598 ms between cyl
F_gear <- MS_between(mtcars$gear, between_gear)[1] / MS_within(mtcars$gear, within_gear)[1]
F_cyl <- MS_between(mtcars$cyl, between_cyl)[1] / MS_within(mtcars$cyl, within_cyl)[1]
F_gear
#125.79
F_cyl
#625.92

#5

between_chick <-SS_between(ChickWeight$weight, mean(ChickWeight$weight),d1)+ SS_between(ChickWeight$weight, mean(ChickWeight$weight),d2) +SS_between(ChickWeight$weight, mean(ChickWeight$weight),d3) + SS_between(ChickWeight$weight, mean(ChickWeight$weight),d4)

within_chick <- SS_within(d1) + SS_within(d2) + SS_within(d3) + SS_within(d4)
within_chick
#20404.59
weight.aov= aov(ChickWeight$weight~ChickWeight$Diet)
cyl.aov = aov(mtcars$mpg ~ mtcars$cyl)
gear.aov = aov(mtcars$mpg ~ mtcars$gear)
summary(cyl.aov)
summary(gear.aov)
summary(weight.aov)