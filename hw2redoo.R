
#---------------------PART 1-------------------
  #1
popvar<-function(x){
  n<-length(x)
  (((n-1) / n) * var(x))}

c4 <- mtcars$mpg[which(mtcars$cyl==4)]
c6 <- mtcars$mpg[which(mtcars$cyl==6)]
c8 <- mtcars$mpg[which(mtcars$cyl==8)]
car_count<- length(c4)+length(c6)+ length(c8)
resi<-((popvar(c4)*length(c4))+(popvar(c6)*length(c6))+(popvar(c8)*length(c8)))/(car_count)
#residuel variance of all factors but cylider

resi_func<- function(g1,g2,g3,count){
  resid<-((popvar(g1)*length(g1))+(popvar(g2)*length(g2))+(popvar(g3)*length(g3)))/(count)
  return(((popvar(mtcars$mpg)-resid)/popvar(mtcars$mpg))*100)
}

#vari- residual== amount expalied cylinder.
((popvar(mtcars$mpg)-resi)/popvar(mtcars$mpg))*100
#(73.25%)
resi_func(c4,c6,c8,car_count)
#(73.25%)


  #2
g3 <- mtcars$mpg[which(mtcars$gear==3)]
g4 <- mtcars$mpg[which(mtcars$gear==4)]
g5 <- mtcars$mpg[which(mtcars$gear==5)]
car_count<- length(g3)+length(g4)+length(g5)

gr_Resi<- ((popvar(g3) *length(g3))+(popvar(g4)*length(g4))+(popvar(g5)*length(g5)))/(car_count)

((popvar(mtcars$mpg)-gr_Resi)/popvar(mtcars$mpg))*100
#42.92%
resi_func(g3,g4,g5,car_count)
#(42.92%)
#---------------------PART 2-------------------
  #3

SS_within<-function(group){ #could be very wrong
  x<-0
  m<-mean(group)
  for(i in 1:length(group)){
    x=x+(group[i]-m)^2}
  return(x)
}
cyl_within<-SS_within(c4)+SS_within(c6)+SS_within(c8)
cyl_within
#[1] 301.2626

GM<- mean(mtcars$mpg)
GM
#[1] 20.09062


SS_between <- function(group, grandmean){
  #length(unique(group))*((mean(group)-grandmean)^2)}
  length(group)*((mean(group)-grandmean)^2)}

cyl_between<-SS_between(c4,GM)+SS_between(c6,GM)+SS_between(c8,GM)
cyl_between
#Between: [1] 824.7844

SS_total<- function(within,between){
  return(within+between)
}

SS_totz <- function(y){
  return (var(y)*(length(y)-1))
}

#3 versions
SS_totz(mtcars$mpg)
#1126.047
cyl_total<- cyl_between+cyl_within
cyl_total
#[1] 1126.047
SS_total(cyl_within,cyl_between)
#[1] 1126.047


J<-3#number of groups
N<-length(mtcars$mpg)
DF_Between<-J-1 #J number of groups (ie: 3 cylander groups)
DF_Within<-N-J

MS_between<- function(group){
  return(group/DF_Between)
}
cyl_MS_Between<-MS_between(cyl_between)
#[1] 412.3923

MS_within<- function(within){
  return(within/DF_Within)
}

cyl_MS_Within<-MS_within(cyl_within)
#[1] 10.38837

F_value<- function(between,within){
  return(between/within)
}
cyl_F<-F_value(cyl_MS_Between,cyl_MS_Within)
cyl_F
#[1] 39.69752

aovcyl = aov(mpg~cyl, data=mtcars)
summary(aovcyl)

#Df Sum Sq Mean Sq F value   Pr(>F)    
#cyl          1  817.7   817.7   79.56 6.11e-10 ***
#  Residuals   30  308.3    10.3   

  #4

gr_Within<- SS_within(g3)+SS_within(g4)+SS_within(g5)
gr_Within
#[1] 642.804

gr_Between<-SS_between(g3,GM)+SS_between(g4,GM)+SS_between(g5,GM)
gr_Between
#[1] 483.2432

gr_Total<-SS_total(gr_Within,gr_Between)
gr_Total
#[1] 1126.047

gr_MS_Between<-MS_between(gr_Between)
gr_MS_Between
#[1] 241.6216

gr_MS_Within<-MS_within(gr_Within)
gr_MS_Within
#[1] 22.16566

gr_F<-F_value(gr_MS_Between,gr_MS_Within)
gr_F
#[1] 10.90072

aovmpg = aov(mpg~gear, data=mtcars)
summary(aovmpg)

#Df Sum Sq Mean Sq F value Pr(>F)   
#gear         1  259.7  259.75   8.995 0.0054 **
#  Residuals   30  866.3   28.88                  

#Cylinder explains more of the variance in MPG (F=39.69752) compared to that of the gears (F=10.90072). There is more variance between cylinders than variance between gears. The high variance between cylinders creates a high F score. This is the oppisite for gears.

  #5
J=4
N<-length(ChickWeight$weight)
DF_Between=J-1 #J number of groups (ie:4 chicken diets groups)
DF_Within=N-J

d1 <- ChickWeight$weight[which(ChickWeight$Diet==1)]
d2 <- ChickWeight$weight[which(ChickWeight$Diet==2)]
d3 <- ChickWeight$weight[which(ChickWeight$Diet==3)]
d4 <- ChickWeight$weight[which(ChickWeight$Diet==4)]

ChickWeight

diet_Within<-SS_within(d1)+SS_within(d2)+SS_within(d3)+SS_within(d4)
diet_Within
#[1] 2758693

chick_GM<- mean(ChickWeight$weight)
chick_GM
#[1] 121.8183

diet_Between<-SS_between(d1,chick_GM)+SS_between(d2,chick_GM)+SS_between(d3,chick_GM)+SS_between(d4,chick_GM)
diet_Between
#[1] 155862.7

diet_Total<-SS_total(diet_Within,diet_Between)
diet_Total
#[1] 2914556

diet_MS_Between<-MS_between(diet_Between)
diet_MS_Between
#[1] 51954.22

diet_MS_Within<-MS_within(diet_Within)
diet_MS_Within
#[1] 4806.086

diet_F<-F_value(diet_MS_Between, diet_MS_Within)
diet_F
#[1] 10.81009





chickenaov = aov(ChickWeight$weight~ChickWeight$Diet, data=ChickWeight)
summary(chickenaov)
boxplot(weight~Diet, data = ChickWeight, xlab="Diet", ylab = "Weight" )

#                  Df  Sum Sq Mean Sq F value   Pr(>F)    
#ChickWeight$Diet   3  155863   51954   10.81 6.43e-07 ***
#Residuals        574 2758693    4806   

# A lower F score means more overlap and low variance (F=10.81), and subsequently, the effect of the variable is low. The F Score is 10.81, this is a low F score similar to our gear F score. Gears had less between variance than the cylinders. There is a lot of overlap across all diets around the same weight (somewhere around 75-125). Diet does not have a significant effect on weight. Diet does not matter

