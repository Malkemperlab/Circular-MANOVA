
######## Real data examples for the MANOVA test 

#loading the libraries libraries
library(circular) #needed for circular functions and pigeon data set
library(readxl) #needed to import axcel files (Obleser data)
library(sjPlot) #load library for table creation

library(stringr)
library(stringi) #needed for manova_AIC_selection function 

####### load the functions used in this script (file needs to be in working directory)
source("Functions_For_Real_Examples_Feb2022.R")

##################
#the pigeon example (package circular)
######################

data(pigeons) # load data from in package circular 

#define the variables needed 
ori.p <- rad(pigeons$bearing) #direction needs to be in radians -> convert to radians 
trt.p <- as.factor(pigeons$treatment) #extract the treatment from data set 

#perform MANOVA for treatment effect 
MANOVA_pigeon<-summary(manova(cbind(cos(ori.p),sin(ori.p)) ~ trt.p),intercept=T)

#Write the table -> this takes a few steps, because we also show "<0.01" for everything below 0.01
MANOVA_table<-as.data.frame(MANOVA_pigeon$stats)
MANOVA_table$`Pr(>F)`<- round(MANOVA_table$`Pr(>F)`,3)
MANOVA_table$`Pr(>F)`<-ifelse( (MANOVA_table$`Pr(>F)`<0.01), "<0.01", MANOVA_table$`Pr(>F)`) 
MANOVA_table_<-MANOVA_table[-c(3),-c(1,4,5)]
rownames(MANOVA_table_)<-c("Intercept","Treatment")
tab_df(MANOVA_table_,file="MANOVA_Table_pigeon.doc",col.header = c("Pillai","approx. F", "p"),show.rownames=TRUE) #doesn't show the row name in output for some reason, added it by hand


##################
#the bat example
######################

#load file (this data sheet is also uploaded)
bats<-read.csv("rawdata_lindecke.csv") 
colnames(bats)[1]<-"treatment"

#define the variables needed 
age<-as.factor(bats$age)
sex<-as.factor(bats$sex)
ori.b<-rad(bats$takeoff.orientation) #again need to convert in radians 
trt.b<-as.factor(bats$treatment)
temp<-as.numeric(bats$temperature.at.release.site)
windspeed<-as.numeric(bats$wind.speed..m.s.)

#perform the MANOVA and model selection 
### model selection method based on AIC (function imported at the start of the script)

#set the response variable (cos and sine of direction)
RESPONSE<-cbind(cos(ori.b),sin(ori.b))

#set full model to be evaluated 
MANOVA_bats_full<-summary(manova(RESPONSE~ (age+sex+trt.b)^2+temp+windspeed)) #after eliminating non significant variables in a step-wise manner 

#use manova model selection function - first term variable response matrix (2 columns), second term the summary of full model - WITHOUT intercept
AIC_selection_bat<-manova_AIC_selection(RESPONSE,MANOVA_bats_full)

#use best model
MANOVA_bats<-AIC_selection_bat[[2]]

#Write the table to a doc file 
MANOVA_table<-as.data.frame(MANOVA_bats$stats)
MANOVA_table$`Pr(>F)`<- round(MANOVA_table$`Pr(>F)`,3)
MANOVA_table$`Pr(>F)`<-ifelse( (MANOVA_table$`Pr(>F)`<0.01), "<0.01", MANOVA_table$`Pr(>F)`) 
MANOVA_table_<-MANOVA_table[-c(5),-c(1,4,5)]
rownames(MANOVA_table_)<-c("Intercept","Age","Treatment","Age by treatment")
tab_df(MANOVA_table_,file="MANOVA_Table_bats.doc",col.header = c("Pillai","approx. F", "p"),show.rownames=TRUE) #doesn't show the row name in output for some reason, added it by hand


#####################
#Obleser data 
#####################

deer<-read_excel("Obleser_Data.xlsx")

hour<-as.numeric(deer$hour)
weather_abbrev<-as.factor(deer$weather)
temp<-as.numeric(deer$temp.)
light<-as.numeric(deer$`light intensity`)
wind_speed<-as.numeric(deer$`wind speed (Km/h)`) #NAs
wind_dir<-rad(as.numeric(deer$`wind direction`))
sun_dir<-rad(as.numeric(deer$`sun position`)) #NAss
hide_dir<-rad(as.numeric(deer$`direction to hide`))
flight_dir<-rad(as.numeric(deer$`flight direction`))
previous_dir<-rad(as.numeric(deer$`alignment before reaction`))
hide_dist<-as.numeric(deer$`distance to hide`)
group_size<-as.numeric(deer$`group size`)
sex<-as.factor(deer$sex)
age<-as.factor(deer$age)
obs_dir<-rad(as.numeric(deer$`direction to observer`))
veg_height<-as.factor(deer$`vegetation height (1 < 50 cm, 2 > 50 cm)`)

sun_axis<-DoublingRad(sun_dir)
hide_axis<-DoublingRad(hide_dir)
flight_axis<-DoublingRad(flight_dir)
previous_axis<-DoublingRad(previous_dir)

# perform the MANOVA and model selection 

# Prepare response 
RESPONSE_deer<-cbind(cos(flight_axis),sin(flight_axis))

# set the full model
MANOVA_deer_full<-summary(manova(RESPONSE_deer ~ 
                                    hour+weather_abbrev+temp+light+wind_speed+sin(wind_dir)+cos(wind_dir)+
                                    sin(hide_dir)+cos(hide_dir)+sin(previous_dir)+cos(previous_dir)+hide_dist+
                                    group_size+sex+age+sin(obs_dir)+cos(obs_dir)+cos(sun_axis)+
                              sin(sun_axis)+cos(hide_axis)+sin(hide_axis)+
                              sin(previous_axis)+
                              veg_height
                            +cos(previous_axis)
                                    ))


#use manova model selection function - first term variable response matrix (2 columns), second term the summary of full model - WITHOUT intercept
#here we use only the 10 highest ranked factors to be evaluated (third term in function)
#the factors are arranged according to effect size 
AIC_selection_deer<-manova_AIC_selection(RESPONSE_deer,MANOVA_deer_full,10)

#use best model
MANOVA_deer<-AIC_selection_deer[[2]]

#prepare table 
MANOVA_table<-as.data.frame(MANOVA_deer$stats)
MANOVA_table$`Pr(>F)`<- round(MANOVA_table$`Pr(>F)`,3)
MANOVA_table$`Pr(>F)`<-ifelse( (MANOVA_table$`Pr(>F)`<0.01), "<0.01", MANOVA_table$`Pr(>F)`) 
MANOVA_table_<-MANOVA_table[-c(nrow(MANOVA_table)),-c(1,4,5)]
rownames(MANOVA_table_)<-c("Intercept","Cosine  of previous alignment","Cosine  of hide axis","Sine of hide axis",
                           "Sine  of sun axis","Wind speed", "Cosine of wind direction", "Temperature")
tab_df(MANOVA_table_,file="MANOVA_Table_deer.doc",col.header = c("Pillai","approx. F", "p"),show.rownames=TRUE)












