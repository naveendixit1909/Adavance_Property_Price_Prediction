#read data

train <- read.csv(file = "Property_price_Train.csv", stringsAsFactors = F)
test <- read.csv(file = "Property_price_Test.csv", stringsAsFactors = F)

head(property.train)

#column names

names(property.train)
names(property.test)

str(property.train)

train$IsTrain <- T
test$IsTrain <- F

test$Sale_Price <- NA

#combining data into one

all <- rbind(train,test)

summary(all)

#taking only numeric variables

num_Var <- which(sapply(all, is.numeric)) #indexing numeric
all.num <- all[,num_Var]

#taking only character variables

char_Var <- which(sapply(all, is.character)) #index character
all.char <- all[,char_Var]

#checking for missing variables

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

#pool_quality

all$Pool_Quality[is.na(all$Pool_Quality)] <- 'None'

#label encode the variables
table(as.factor(all$Pool_Quality))

Qualities <- c('None' = 0, 'Fa' = 1, 'Gd' = 2, 'Ex' = 3)
library(plyr)
all$Pool_Quality<-as.integer(revalue(all$Pool_Quality, Qualities))
table(all$Pool_Quality)

#checking pool quality with pool area

all[all$Pool_Area>0 & all$Pool_Quality==0, c('Pool_Area', 'Pool_Quality')]
all$Pool_Quality[2420]<-2
all$Pool_Quality[2503]<-2
all$Pool_Quality[2420]<-3
all$Pool_Quality[2599]<-2

#Miscellaneous_Feature

table(all$Miscellaneous_Feature)

all$Miscellaneous_Feature[is.na(all$Miscellaneous_Feature)] <- 'None'

all$Miscellaneous_Feature <- as.factor(all$Miscellaneous_Feature)
library(tidyverse)
ggplot(all[!is.na(all$Sale_Price),], aes(x=Miscellaneous_Feature, y=Sale_Price)) +
  geom_bar(stat = "summary", data = all, fill = 'blue')

#Lane_Type

table(all$Lane_Type)
all$Lane_Type[is.na(all$Lane_Type)] <- 'None'

all$Lane_Type <- as.factor(all$Lane_Type)
ggplot(all[!is.na(all$Sale_Price),], aes(x=Lane_Type, y=Sale_Price)) +
  geom_bar(stat = "summary", data = all, fill = 'blue')

#fence quality

table(all$Fence_Quality)

all$Fence_Quality[is.na(all$Fence_Quality)] <- 'None'

all[!is.na(all$Sale_Price),] %>% 
  group_by(Fence_Quality) %>% 
  summarise(median = median(Sale_Price), counts=n())

all$Fence_Quality <- as.factor(all$Fence_Quality)
ggplot(all[!is.na(all$Sale_Price),], aes(x=Fence_Quality, y=Sale_Price))+
  geom_bar(stat = "summary", data = all, fill = 'blue')

#Fireplace_Quality

table(all$Fireplace_Quality)

all$Fireplace_Quality[is.na(all$Fireplace_Quality)] <- 'None'

Quality <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

#labeling

all$Fireplace_Quality<-as.integer(revalue(all$Fireplace_Quality, Quality))

table(all$Fireplace_Quality)

table(all$Fireplaces)

#Lot_Extent

ggplot(all[!is.na(all$Lot_Extent),], aes(x=as.factor(Neighborhood), y=Lot_Extent)) +
  geom_bar(stat = "summary", data = all, fill = 'blue')

#replacing the na value by median per neighborhood

for (i in 1:nrow(all)){
  if(is.na(all$Lot_Extent[i])){
    all$Lot_Extent[i] <- as.integer(median(all$Lot_Extent[all$Neighborhood==all$Neighborhood[i]], na.rm=T)) 
  }
}

table(all$Lot_Configuration)

ggplot(all[!is.na(all$Sale_Price),], aes(x=Lot_Configuration, y= Sale_Price)) +
  geom_bar(stat = "summary", data = all, fill = 'blue')

all$Lot_Configuration <- as.factor(all$Lot_Configuration)

#garage

all$Garage_Built_Year[is.na(all$Garage_Built_Year)] <- all$Construction_Year[is.na(all$Garage_Built_Year)]

all<- all %>% rename(Garage_Finish = Garage_Finish_Year)

all[!is.na(all$Garage) & is.na(all$Garage_Finish), c('Garage',
                                                     'Garage_Finish',
                                                     'Garage_Size',
                                                     'Garage_Area',
                                                     'Garage_Quality',
                                                     'Garage_Condition')]

table(all$Garage)
all$Garage_Finish[2126] <- 'Unf'
all$Garage_Quality[2126] <- 'TA'
all$Garage_Condition[2126] <- 'TA'

#as column 2576 indicates it have no garage

all$Garage_Area[2576] <- 0
all$Garage_Size[2576] <- 0
all$Garage[2576] <- NA

#now all garage variables have same number of missing value

all$Garage[is.na(all$Garage)] <- 'No Garage'
all$Garage <- as.factor(all$Garage)
table(all$Garage)

all$Garage_Finish[is.na(all$Garage_Finish)] <- 'None'
table(all$Garage_Finish)
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)

all$Garage_Finish<-as.integer(revalue(all$Garage_Finish, Finish))
table(all$Garage_Finish)

all$Garage_Quality[is.na(all$Garage_Quality)] <- 'None'
table(all$Garage_Quality)
all$Garage_Quality<-as.integer(revalue(all$Garage_Quality, Quality))
table(all$Garage_Quality)

all$Garage_Condition[is.na(all$Garage_Condition)] <- 'None'
all$Garage_Condition<-as.integer(revalue(all$Garage_Condition, Quality))
table(all$Garage_Condition)

#Basement_variables

#checking how many variables have same no. of NA's

length(which(is.na(all$Basement_Condition) & is.na(all$Exposure_Level) & is.na(all$Basement_Height) & is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))

all[!is.na(all$BsmtFinType1) & 
      (is.na(all$Basement_Condition)|
         is.na(all$Basement_Height)|
         is.na(all$Exposure_Level)|
         is.na(all$BsmtFinType2)), c('Basement_Height', 'Basement_Condition', 'Exposure_Level', 'BsmtFinType1', 'BsmtFinType2')]

all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$Exposure_Level[c(949, 1487, 2348)] <- names(sort(-table(all$Exposure_Level)))[1]
all$Basement_Condition[c(2040, 2185, 2524)] <- names(sort(-table(all$Basement_Condition)))[1]
all$Basement_Height[c(2217, 2218)] <- names(sort(-table(all$Basement_Height)))[1]

all$Basement_Height[is.na(all$Basement_Height)] <- 'None'
table(all$Basement_Height)
all$Basement_Height<-as.integer(revalue(all$Basement_Height, Quality))
table(all$Basement_Height)

all$Basement_Condition[is.na(all$Basement_Condition)] <- 'None'
table(all$Basement_Condition)
all$Basement_Condition<-as.integer(revalue(all$Basement_Condition, Quality))
table(all$Basement_Condition)

all$Exposure_Level[is.na(all$Exposure_Level)] <- 'None'
table(all$Exposure_Level)
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)

all$Exposure_Level<-as.integer(revalue(all$Exposure_Level, Exposure))
table(all$Exposure_Level)

all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
table(all$BsmtFinType1)
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
table(all$BsmtFinType1)

all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'

all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
table(all$BsmtFinType2)

all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0

all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0

all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0

all$Total_Basement_Area[is.na(all$Total_Basement_Area)] <-0

#Brick vaneer variables

length(which(is.na(all$Brick_Veneer_Type) & is.na(all$Brick_Veneer_Area)))

all[is.na(all$Brick_Veneer_Type) & 
      !is.na(all$Brick_Veneer_Area), 
    c('Brick_Veneer_Type', 'Brick_Veneer_Area')]

table(all$Brick_Veneer_Type)

all$Brick_Veneer_Type[2610]<- 'BrkFace'

all$Brick_Veneer_Type[is.na(all$Brick_Veneer_Type)] <- 'None'

all[!is.na(all$Sale_Price),] %>% 
  group_by(Brick_Veneer_Type) %>% 
  summarise(median = median(Sale_Price), counts=n()) %>% 
  arrange(median)

#let say common bricks/none are same as their median is almost same
#others have significant difference than brkCmn/none

all$Brick_Veneer_Type<-as.integer(revalue(all$Brick_Veneer_Type, c('None'=0, 
                                                     'BrkCmn'=0, 
                                                     'BrkFace'=1, 
                                                     'Stone'=2)))
table(all$Brick_Veneer_Type)

all$Brick_Veneer_Area[is.na(all$Brick_Veneer_Area)] <-0

#zoning class

table(all$Zoning_Class)

all$Zoning_Class[is.na(all$Zoning_Class)] <- 'RLD'

all$Zoning_Class <- as.factor(all$Zoning_Class)

#utility type

table(all$Utility_Type)

#there are all values are same except one so its useless

all$Utility_Type <- NULL

#underground bathroom

length(which(is.na(all$Underground_Full_Bathroom) & is.na(all$Underground_Half_Bathroom)))

table(all$Underground_Full_Bathroom)
all$Underground_Full_Bathroom[is.na(all$Underground_Full_Bathroom)] <- 0

table(all$Underground_Half_Bathroom)
all$Underground_Half_Bathroom[is.na(all$Underground_Half_Bathroom)] <- 0

#Functional_Rate

table(all$Functional_Rate)

rate <- c('MajD1' = 1,
          'MajD2' =2,
          'MD' = 3,
          'MD1' = 4,
          'MD2' = 5,
          'Mod' = 6,
          'MS' = 7,
          'SD' = 8,
          'Sev' = 9,
          'TF' = 10)

all$Functional_Rate <- as.integer(revalue(all$Functional_Rate,rate))                                  
table(all$Functional_Rate)
all$Functional_Rate[is.na(all$Functional_Rate)] <- 10

#exterior variables

table(all$Exterior1st)
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]

all$Exterior1st <- as.factor(all$Exterior1st)

table(all$Exterior2nd)
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]

all$Exterior2nd <- as.factor(all$Exterior2nd)

#labeling
table(all$Exterior_Condition)
all$Exterior_Condition <- as.integer(revalue(all$Exterior_Condition,Quality))

table(all$Exterior_Material)
all$Exterior_Material <- as.integer(revalue(all$Exterior_Material,Quality))

#Electrical_System

table(all$Electrical)
all$Electrical_System[is.na(all$Electrical_System)] <- 'SBrkr'

all$Electrical_System <- as.factor(all$Electrical_System)

#kitchen

table(all$Kitchen_Quality)
all$Kitchen_Quality[is.na(all$Kitchen_Quality)] <- 'TA'
all$Kitchen_Quality <- as.integer(revalue(all$Kitchen_Quality,Quality))

#Sale_Type

table(all$Sale_Type)
table(all$Sale_Condition)

all$Sale_Type[is.na(all$Sale_Type)] <- 'WD'

all$Sale_Type <- as.factor(all$Sale_Type)
all$Sale_Condition <- as.factor(all$Sale_Condition)

#labeling and factorizing

names(all[,sapply(all, is.character)])

#roads

table(all$Road_Type)
all$Road_Type<-as.integer(revalue(all$Road_Type, c('Gravel'=0, 'Paved'=1)))

table(all$Pavedd_Drive)
all$Pavedd_Drive<-as.integer(revalue(all$Pavedd_Drive, c('N'=0, 'P'=1, 'Y'=2)))
table(all$Pavedd_Drive)

#foundation

table(all$Foundation_Type)
all$Foundation_Type <- as.factor(all$Foundation_Type)

#heating and air condition

table(all$Heating_Quality)
all$Heating_Quality <- as.integer(revalue(all$Heating_Quality, Quality))

table(all$Heating_Type)
all$Heating_Type <- as.factor(all$Heating_Type)

table(all$Air_Conditioning)
all$Air_Conditioning<-as.integer(revalue(all$Air_Conditioning, c('N'=0, 'Y'=1)))
table(all$Air_Conditioning)


#neighborhood

all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)

table(all$Condition1)
all$Condition1 <- as.factor(all$Condition1)

all$Condition2 <- as.factor(all$Condition2)

#roof

table(all$Roof_Design)
all$Roof_Design <- as.factor(all$Roof_Design)
table(all$Roof_Quality)
all$Roof_Quality <- as.factor(all$Roof_Quality)

#land

table(all$Land_Outline)
all$Land_Outline <- as.factor(all$Land_Outline)

table(all$Property_Shape)
all$Property_Shape <- as.integer(revalue(all$Property_Shape, c('Reg'=0,
                                                               'IR1'=1,
                                                               'IR2'=2,
                                                               'IR3'=3)))

table(all$Property_Slope)
all$Property_Slope <- as.integer(revalue(all$Property_Slope, c('GS'=0,
                                                               'MS'=1,
                                                               'SS'=2)))

#house

table(all$House_Design)
ggplot(all[!is.na(all$Sale_Price),], aes(x=House_Design, y= Sale_Price)) +
  geom_bar(stat = "summary", data = all, fill = 'blue')
all$House_Design <- as.factor(all$House_Design)

table(all$House_Type)
ggplot(all[!is.na(all$Sale_Price),], aes(x=House_Type, y= Sale_Price)) +
  geom_bar(stat = "summary", data = all, fill = 'blue')
all$House_Type <- as.factor(all$House_Type)

num_Var <- which(sapply(all, is.numeric)) #indexing numeric
all.num <- all[,num_Var]

cor_numVar <- cor(all.num, use="pairwise.complete.obs")
#sort on decreasing correlations with Sale Price
cor_sorted <- as.matrix(sort(cor_numVar[,'Sale_Price'], decreasing = TRUE))
cor_sorted
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
CorHigh
library(corrplot)
corrplot.mixed(cor_numVar)
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


#split back into train and test

all<- all[,-1]
drop <- c("Remodel_Year",
          "Garage_Area",
          "Kitchen_Above_Grade",
          "BsmtFinSF2",
          "Garage_Condition")
all <- all[,!(names(all) %in% drop)]

train <- all[all$IsTrain==T,]
test <- all[all$IsTrain==F,]

train <- train[ , -which(names(train) %in% c("IsTrain"))]
test <- test[ , -which(names(test) %in% c("IsTrain"))]

#removing outliers

hist(train$Basement_Height)

boxplot(train$Sale_Price)$stat
quantile(train$Sale_Price, seq(0,1,.01))
train <- train[train$Sale_Price<340000,]
train$Sale_Price[train$Sale_Price>313000] <- 313000
boxplot(train$Sale_Price)

library(randomForest)
model1 <- randomForest(Sale_Price~.,
                       data = train)
predct_train <- predict(model1,train)

check <- cbind(train$Sale_Price, predct_train)

importance(model1,decreasing = T)
varImpPlot(model1)

test$Sale_Price <- predict(model1,test)
plot(model1)
write.csv(test,file = "new_prediction.csv", row.names = F)
