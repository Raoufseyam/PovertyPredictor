
install.packages("ordinal")
install.packages("rpart")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("ISLR")
install.packages("class")
install.packages("varhandle")
install.packages("dplyr")
install.packages("tree")
install.packages("akima")
install.packages("gam")
install.packages("MASS")
install.packages("MLmetrics")
install.packages("Matrix")
install.packages("randomForest")
install.packages("data.table")
install.packages("ordinal")
install.packages("tidyverse")
install.packages("caret")
install.packages("lattice")
install.packages("tidyr")

library(ordinal)
library(rpart)
library(glmnet)
library(ggplot2)
library(ISLR)
library(class)
library(varhandle)
library(dplyr)
library(tree)
library(akima)
library(gam)
library(MASS)
library(MLmetrics)
library(Matrix)
library(randomForest)
library(data.table)
library(ordinal)
library(tidyverse)
library(caret)
library(lattice)
library(tidyr)


## Creating F1 macro according to the guidelines
## Poverty has four levels (1,2,3,4), so this code will
## calculate 4 F1 scores, then averages and lists them

compare<-function(true, pred,x=1) {
  
  # truex is to consider only x positive and others negative
  # predx is to perform the same task
  truex<-ifelse(true==x,1,0)
  predx<-ifelse(pred==x,1,0)
  
  True.P<-(truex==1) & (predx==1)
  False.P<-(truex==1) & (predx==0)
  False.N<-(truex==0) & (predx==1)
  
  # creating same equation from guidelines
  sum(True.P)*2/(sum(True.P)*2+sum(False.P)+sum(False.N))
  
}

f1macro<-function(true, pred){
  
  F1.1<-compare(true,pred,1)
  F1.2<-compare(true,pred,2)
  F1.3<-compare(true,pred,3)
  F1.4<-compare(true,pred,4)
  
  Ave.F1<-sum(F1.1,F1.2,F1.3, F1.4)/4
  
  list(F1.1=F1.1, F1.2=F1.2, F1.3=F1.3, F1.4=F1.4, Ave.F1=Ave.F1)
}

########################################################
# @user please change the path on line 83 to run the code
########################################################

file_path<-"C:/Users/Mohamed Seyam/Desktop/API222/Competition/"

# read input datasets
a_train<-read.csv(paste(file_path, "training_data.csv",sep=""),stringsAsFactors=F)
a_test<-read.csv(paste(file_path, "test_data.csv",sep=""), stringsAsFactors=F)

# adding Target col with '0' vals as a place marker for Target
# vals to be predicted; as well as to make sure both datasets
# have same # of variables (143)
a_test[,143]<-0
names(a_test)[143]<-paste("Target")

# combining both data sets. In the new dataset, rbind
# will keep train data on top, then insert 917 new rows
# below them for the test data.
a<-rbind(a_train,a_test)

################# data renaming, combining, and cleaning ##################

# rename the housing status variables from tipovivi1..5
a[,144]<-NA
names(a)[144]<-paste("Housing.Status")
a$Housing.Status[a$tipovivi1==1]<-"1:own and paid"
a$Housing.Status[a$tipovivi2==1]<-"2:own and install"
a$Housing.Status[a$tipovivi3==1]<-"3:rented"
a$Housing.Status[a$tipovivi4==1]<-"4:precarious"
a$Housing.Status[a$tipovivi5==1]<-"5:others"
a[,145]<-a$v2a1
names(a)[145]<-paste("Monthly.Rent")

# rename the room information
a[,146]<-a$hacapo
names(a)[146]<-paste("Over.Room")
a[,147]<-a$rooms
names(a)[147]<-paste("Num.Rooms")
a[,148]<-a$hacdor
names(a)[148]<-paste("Over.Bedroom")
a[,149]<-a$bedrooms
names(a)[149]<-paste("Num.Bedroom")
a[,150]<-a$overcrowding
names(a)[150]<-paste("Person.Bedroom")
a[,151]<-a$v14a
names(a)[151]<-paste("Has.Bathroom")

# rename household owning
a[,152]<-a$refrig
names(a)[152]<-paste("Has.Refrig")

# delete or combine own tablet
a[,153]<-a$v18q
names(a)[153]<-paste("Own.Tablet")
a[,154]<-a$v18q1
names(a)[154]<-paste("Num.Tablet")

a[,155]<-a$computer
names(a)[155]<-paste("Own.Computer")
a[,156]<-a$television
names(a)[156]<-paste("Own.TV")
a[,157]<-a$mobilephone
names(a)[157]<-paste("Own.Mphone")
a[,158]<-a$qmobilephone
names(a)[158]<-paste("Num.Phones")

# rename demographics information
a[,159]<-a$tamhog
names(a)[159]<-paste("Hhold.Size")
a[,160]<-a$age
names(a)[160]<-paste("Ind.Age")
a[,161]<-a$dis
names(a)[161]<-paste("Ind.Dis")
a[,162]<-NA
names(a)[162]<-paste("Ind.Gender")
a$Ind.Gender[a$male==1]<-"1:Male"
a$Ind.Gender[a$female==1]<-"2:Female"
a[,163]<-a$escolari
names(a)[163]<-paste("Ind.Schooling")
a[,164]<-a$rez_esc
names(a)[164]<-paste("Ind.Yearsbehind")
###Number of males 12 and older will be Hhold.Male-Hhold.Males12u; 
a[,165]<-a$r4h1
names(a)[165]<-paste("Hhold.Males12u") 
a[,166]<-a$r4h3
names(a)[166]<-paste("Hhold.Males")  
###Region
a[,167]<-NA
names(a)[167]<-paste("Region.Cat")
a$Region.Cat[a$lugar1==1]<-"1:Central"
a$Region.Cat[a$lugar2==1]<-"2:Chorotega"
a$Region.Cat[a$lugar3==1]<-"3:Pacifico Central"
a$Region.Cat[a$lugar4==1]<-"4:Brunca"
a$Region.Cat[a$lugar5==1]<-"5:Huetar Atlantica"
a$Region.Cat[a$lugar6==1]<-"6:Huetar Norte"
###Area
a[,168]<-NA
names(a)[168]<-paste("Area.Cat")
a$Area.Cat[a$area1==1]<-"1:Urbana"
a$Area.Cat[a$area2==1]<-"2:Rural"
###Education level
a[,169]<-NA
names(a)[169]<-paste("Edu.Cat")
a$Edu.Cat[a$instlevel1==1]<-"1:No education"
a$Edu.Cat[a$instlevel2==1]<-"2:Incomplete primary"
a$Edu.Cat[a$instlevel3==1]<-"3:Complete primary"
a$Edu.Cat[a$instlevel4==1]<-"4:Incomplete academic secondary"
a$Edu.Cat[a$instlevel5==1]<-"5:Complete academic secondary"
a$Edu.Cat[a$instlevel6==1]<-"6:Incomplete technical secondary"
a$Edu.Cat[a$instlevel7==1]<-"7:Complete technical secondary"
a$Edu.Cat[a$instlevel8==1]<-"8:Undergraduate"
a$Edu.Cat[a$instlevel9==1]<-"9:Postgraduate"
###Family status
a[,170]<-NA
names(a)[170]<-paste("Fam.Status")
a$Fam.Status[a$estadocivil1==1]<-"1:Less than 10"
a$Fam.Status[a$estadocivil2==1]<-"2:Free or coupled union"
a$Fam.Status[a$estadocivil3==1]<-"3:Married"
a$Fam.Status[a$estadocivil4==1]<-"4:Divorced"
a$Fam.Status[a$estadocivil5==1]<-"5:Separated"
a$Fam.Status[a$estadocivil6==1]<-"6:Widow/er"
a$Fam.Status[a$estadocivil7==1]<-"7:Single"
###Parent relationship
a[,171]<-NA
names(a)[171]<-paste("Fam.Rel")
a$Fam.Rel[a$parentesco1==1]<-"1:Household head"
a$Fam.Rel[a$parentesco2==1]<-"2:Spouse/partner"
a$Fam.Rel[a$parentesco3==1]<-"3:Son/daughter"
a$Fam.Rel[a$parentesco4==1]<-"4:Stepson/daughter"
a$Fam.Rel[a$parentesco5==1]<-"5:Son/daughter in law"
a$Fam.Rel[a$parentesco6==1]<-"6:Grandson/daughter"
a$Fam.Rel[a$parentesco7==1]<-"7:Mother/father"
a$Fam.Rel[a$parentesco8==1]<-"8:Father/mother in law"
a$Fam.Rel[a$parentesco9==1]<-"9:Brother/sister"
a$Fam.Rel[a$parentesco10==1]<-"10:Brother/sister in law"
a$Fam.Rel[a$parentesco11==1]<-"11:Other family member"
a$Fam.Rel[a$parentesco12==1]<-"12:Other non family member"

# rename house status related variables
###material for outside wall
a[,172]<-NA
names(a)[172]<-paste("Out.Wall")
a$Out.Wall[a$paredblolad==1]<-"1:Block or brick"
a$Out.Wall[a$paredzocalo==1]<-"2:Socket"
a$Out.Wall[a$paredpreb==1]<-"3:Prefabricated or cement"
a$Out.Wall[a$pareddes==1]<-"4:Waste material"
a$Out.Wall[a$paredmad==1]<-"5:Wood"
a$Out.Wall[a$paredzinc==1]<-"6:Zink"
a$Out.Wall[a$paredfibras==1]<-"7:Natural fibers"
a$Out.Wall[a$paredother==1]<-"8:Other"

###material for floor
a[,173]<-NA
names(a)[173]<-paste("Floor")
a$Floor[a$pisonotiene==1]<-"1:No floor"
a$Floor[a$pisomoscer==1]<-"2:Mosaic/ceramic/terrazo"
a$Floor[a$pisocemento==1]<-"3:Cement"
a$Floor[a$pisonatur==1]<-"4:Natural"
a$Floor[a$pisomadera==1]<-"5:Wood"
a$Floor[a$pisoother==1]<-"6:Other"

###ceiling
a[,174]<-NA
names(a)[174]<-paste("Ceiling")
a$Ceiling<-a$cielorazo

###water
a[,175]<-NA
names(a)[175]<-paste("Water")
a$Water[a$abastaguadentro==1]<-"1:Water provision inside"
a$Water[a$abastaguafuera==1]<-"2:Water provision outside"
a$Water[a$abastaguano==1]<-"3:No water provision"

###electricity
a[,176]<-NA
names(a)[176]<-paste("Electricity")
a$Electricity[a$public==1]="1:CNFL/ICE/ESPH/JASEC"
a$Electricity[a$planpri==1]="2:Private plant"
a$Electricity[a$coopele==1]="3:Cooperative"
a$Electricity[a$noelec==1]="4:No electricity"

###toilet
a[,177]<-NA
names(a)[177]<-paste("Toilet")
a$Toilet[a$sanitario1==1]<-"1:No toilet"
a$Toilet[a$sanitario2==1]<-"2:Sewer or cesspool"
a$Toilet[a$sanitario3==1]<-"3:Septic tank"
a$Toilet[a$sanitario5==1]<-"4:Black hole or letrine"
a$Toilet[a$sanitario6==1]<-"5:Other system"

###energy
a[,178]<-NA
names(a)[178]<-paste("Energy")
a$Energy[a$energcocinar1==1]<-"1:No energy"
a$Energy[a$energcocinar2==1]<-"2:Electricity"
a$Energy[a$energcocinar3==1]<-"3:Gas"
a$Energy[a$energcocinar4==1]<-"4:Wood charcoal"

###rubbish disposal
a[,179]<-NA
names(a)[179]<-paste("Disposal")
a$Disposal[a$elimbasu1==1]<-"1:Tanker truck"
a$Disposal[a$elimbasu2==1]<-"2:Botan hollow or buried"
a$Disposal[a$elimbasu3==1]<-"3:Burning"
a$Disposal[a$elimbasu4==1]<-"4:Thowing unoccupied space"
a$Disposal[a$elimbasu5==1]<-"5:Thowing into water"
a$Disposal[a$elimbasu6==1]<-"6:Other"

###walls Status
a[,180]<-NA
names(a)[180]<-paste("Walls")
a$Walls[a$epared1==1]<-"1:Bad"
a$Walls[a$epared2==1]<-"2:Regular"
a$Walls[a$epared3==1]<-"3:Good"

###Roof Status
a[,181]<-NA
names(a)[181]<-paste("Roofs")
a$Roofs[a$etecho1==1]<-"1:Bad"
a$Roofs[a$etecho2==1]<-"2:Regular"
a$Roofs[a$etecho3==1]<-"3:Good"

###Floor Status
a[,182]<-NA
names(a)[182]<-paste("Floors")
a$Floors[a$eviv1==1]<-"1:Bad"
a$Floors[a$eviv2==1]<-"2:Regular"
a$Floors[a$eviv3==1]<-"3:Good"

# rename household level variables
a[,183]<-a$idhogar
names(a)[183]<-paste("Hhold.Id")
a[,184]<-a$hogar_nin
names(a)[184]<-paste("Hhold.Young")
a[,185]<-a$hogar_mayor
names(a)[185]<-paste("Hhold.Elder")
a[,186]<-a$hogar_adul
names(a)[186]<-paste("Hhold.Adult")
a[,187]<-a$hogar_total
names(a)[187]<-paste("Hhold.Total")

# introducing a new feature!
# recalculating dependence rate = # of dependants/# of household numbers
# this recalculation is to avoid division by 0 in some cases
a$dep1<-(a$Ind.Age<=18)| (a$Ind.Age>=65)
a<-transform(a,countdep1=ave(dep1,Hhold.Id, FUN=sum))
a[,188]<-a$countdep1/a$Hhold.Size
names(a)[188]<-paste("Hhold.Dep")
a[,189]<-a$edjefe
names(a)[189]<-paste("Mhead.Edu")
a[,190]<-a$edjefa
names(a)[190]<-paste("Fhead.edu")
a[,191]<-a$meaneduc
names(a)[191]<-paste("Medu.Adult")

a[,192]<-a$Target
names(a)[192]<-paste("Poverty.Target")

# rename square of the variables
a[,193]<-a$SQBescolari
names(a)[193]<-paste("SQInd.Schooling")

a[,194]<-a$SQBage
names(a)[194]<-paste("SQInd.Age")

a[,195]<-a$SQBhogar_total
names(a)[195]<-paste("SQHhold.Total")

a[,196]<-a$SQBedjefe
names(a)[196]<-paste("SQMhead.Edu")

a[,197]<-a$SQBhogar_nin
names(a)[197]<-paste("SQHhold.Young")

a[,198]<-a$SQBovercrowding
names(a)[198]<-paste("SQPerson.Bedroom")

a[,199]<-a$SQBdependency
names(a)[199]<-paste("SQHhold.Dep")

a[,200]<-a$SQBmeaned
names(a)[200]<-paste("SQMedu.Adult")

a[,201]<-a$Id
names(a)[201]<-paste("Ind.Id")

###Roof
a[,202]<-NA
names(a)[202]<-paste("Roof")
a$Roof[a$techozinc==1]<-"1:Metal foil or zink"
a$Roof[a$techoentrepiso==1]<-"2:Fiber Cement or Mezzanine"
a$Roof[a$techocane==1]<-"3:Natural fiber"
a$Roof[a$techootro==1]<-"4:Other"

###Number of females 12 and older will be Hhold.Females-Hhold.Females12u; 
a[,203]<-a$r4m1
names(a)[203]<-paste("Hhold.Females12u") 
a[,204]<-a$r4m3
names(a)[204]<-paste("Hhold.Females")  

# Placing indicator column to mark test vs. train data;
# rows 1:7646 are the training data, and 7647:8563 is the test
a[1:7646,205]<-1
a[7647:8563,205]<-0
names(a)[205]<-paste("Indicator")


# b is the newly organized individual level dataset with all the new naming
b<-a[, 144:205]
# droping redundant variable (Hhold.Size) and variable
# with too many N/As (Monthly.Rent)
drops1<-c("Hhold.Size", "Monthly.Rent")
b<-b[ , !(names(b) %in% drops1)]

# converting NA values in Num.Tablet to 0, and
# removing Own.Tablet and Own.Mphone cols since these are
# already represented in Num.Tablet and Num.Mphone cols
b$Num.Tablet <- replace_na(b$Num.Tablet, 0)
b$Own.Tablet <- NULL
b$Own.Mphone <- NULL

# recaclulating years of education for male head of household
# as there were some NAs in the original col
b$Mhead.Edu <- sqrt(b$SQMhead.Edu)

# dropping Fhead.edu and Ind.Yearsbehind
# due to NAs
b$Fhead.edu <- NULL
b$Ind.Yearsbehind <- NULL

# dropping Hhold ID col
b$Hhold.Id <- NULL

# checking for NAs
sum(is.na(b))

# replacing 8 missing values with 0
b[is.na(b)] <- 0

# organizing on household level data;
# droping individual subject level
drops2<-c("Ind.Age","Ind.Dis", "Ind.Gender",
          "Ind.Schooling", "Edu.Cat","Fam.Status",
          "Fam.Rel", "SQInd.Schooling", "SQInd.Age")
c<-b[ , !(names(b) %in% drops2)]
dim(c)

############ end of test variable renaming and data cleaning ##########
#######################################################################
############################# random forest ###########################

##setting all categorical variables as factors (needed for RF formula)
c[,"Housing.Status"] <- factor(c[,"Housing.Status"])
c[,"Region.Cat"] <- factor(c[,"Region.Cat"])
c[,"Area.Cat"] <- factor(c[,"Area.Cat"])
c[,"Out.Wall"] <- factor(c[,"Out.Wall"])
c[,"Floor"] <- factor(c[,"Floor"])
c[,"Water"] <- factor(c[,"Water"])
c[,"Electricity"] <- factor(c[,"Electricity"])
c[,"Toilet"] <- factor(c[,"Toilet"])
c[,"Energy"] <- factor(c[,"Energy"])
c[,"Disposal"] <- factor(c[,"Disposal"])
c[,"Walls"] <- factor(c[,"Walls"])
c[,"Roofs"] <- factor(c[,"Roofs"])
c[,"Floors"] <- factor(c[,"Floors"])
c[,"Roof"] <- factor(c[,"Roof"])

# checking that there aren't any missing values
sum(is.na(c))

# using the indicator col to separate training and test data
c_train <- c[c$Indicator==1,]
c_train[,"Poverty.Target"] <- factor(c_train[,"Poverty.Target"])

# isolating ID col from train set only for RF to run
c_train <- c_train[, !(names(c_train) %in% "Ind.Id")]

c_test<-c[c$Indicator==0,]

# running RF
c_RF <- randomForest(Poverty.Target~., data = c_train, 
                     importance=TRUE)

# predicting using RF
c_RF_Pred        <- predict(c_RF, 
                            type="response",
                            newdata = c_test)

# combining test IDs and predictions
finalpred <- cbind(c_test$Ind.Id, as.numeric(c_RF_Pred))
colnames(finalpred) <-c ("Id","Prediction")

write.csv(finalpred,paste(file_path,"seyam_prediction.csv",sep=""),row.names=F)
