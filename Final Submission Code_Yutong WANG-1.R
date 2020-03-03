#initial prepartion
install.packages('mice')
install.packages('MASS')
install.packages('nnet')
install.packages('raondomForest')
install.packages('data.table')

library(data.table)
library(MASS)
library(nnet)
library(mice)
library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(gbm)
library(stringr)

setwd('/Users/jolley/Desktop/Kaggle/pricelala2/8_处理原始数据')
data = fread('analysisData.csv',encoding = "UTF-8")
scoringData = fread('scoringData.csv',encoding = "UTF-8")

# explore data
str(data)
sum(is.na(data$square_feet))/nrow(data)
data = data[,-"square_feet"]

sum(is.na(data$weekly_price))/nrow(data)
data = data[,-"weekly_price"]

sum(is.na(data$monthly_price))/nrow(data)
data = data[,-"monthly_price"]

sum(is.na(data$security_deposit))/nrow(data)
sum(is.na(data$security_deposit))
ggplot(data, aes(x=security_deposit))+
  geom_freqpoly()
data = data[,-"security_deposit"]

sum(is.na(data$is_business_travel_ready))/nrow(data)
table(data$is_business_travel_ready)
data = data[,-"is_business_travel_ready"]

sum(is.na(data$requires_license))/nrow(data)
table(data$requires_license)
data = data[,-"requires_license"]

sum(is.na(data$require_guest_profile_picture))/nrow(data)
table(data$require_guest_profile_picture)

sum(is.na(data$require_guest_phone_verification))/nrow(data)
table(data$require_guest_phone_verification)

sum(is.na(data$license))/nrow(data)
table(data$license)
data = data[,-"license"]

sum(is.na(data))

sum(is.na(data$host_acceptance_rate))/nrow(data)
table(data$host_acceptance_rate)
data = data[,-"host_acceptance_rate"]

table(data$has_availability)
data = data[,-"has_availability"]

table(data$jurisdiction_names)
data = data[,-"jurisdiction_names"]

sum(is.na(data$data$guests_included))/nrow(data)
table(data$guests_included)

table(data$host_is_superhost)

table(data$country)
data = data[,-"country"]

table(data$country_code)
data = data[,-"country_code"]

table(data$state)
data = data[,-"state"]

table(data$host_identity_verified)

table(data$property_type)

table(scoringData$property_type)

table(data$smart_location)

table(data$calculated_host_listings_count_entire_homes)

table(data$calculated_host_listings_count)

table(data$host_response_rate)

table(data$host_is_superhost)

table(data$host_identity_verified)

table(data$neighbourhood_group_cleansed)

#clean data
data$room_type <- as.factor(data$room_type)
table(data$room_type)

data$host_is_superhost <- as.factor(data$host_is_superhost)
table(data$host_is_superhost)

data$host_identity_verified <- as.factor(data$host_identity_verified)
table(data$host_identity_verified)

data$bed_type <- as.factor(data$bed_type)
table(data$bed_type)

data$neighbourhood_group_cleansed <- as.factor(data$neighbourhood_group_cleansed)
table(data$neighbourhood_group_cleansed)

data$zipcode <- as.factor(data$zipcode)
table(data$zipcode)

#same to the scoring data
scoringData$room_type <- as.factor(scoringData$room_type)

scoringData$host_is_superhost <- as.factor(scoringData$host_is_superhost)

scoringData$host_identity_verified <- as.factor(scoringData$host_identity_verified)

scoringData$bed_type <- as.factor(scoringData$bed_type)

scoringData$neighbourhood_group_cleansed <- as.factor(scoringData$neighbourhood_group_cleansed)

scoringData$zipcode <- as.factor(scoringData$zipcode)

#clean again
data$amenities <- str_length(data$amenities)
data$summary <- str_length(data$summary)
data$description <- str_length(data$description)
data$access <- str_length(data$access)
data$host_about <- str_length(data$host_about)

#clean on scoring again
scoringData$amenities <- str_length(scoringData$amenities)
scoringData$summary <- str_length(scoringData$summary)
scoringData$description <- str_length(scoringData$description)
scoringData$access <- str_length(scoringData$access)
scoringData$host_about <- str_length(scoringData$host_about)


#process NA on analysis
sum(is.na(data$beds))
data$beds[is.na(data$beds)] <- round(mean(data$beds, na.rm = TRUE))

sum(is.na(data$reviews_per_month))
data$reviews_per_month[is.na(data$reviews_per_month)] <- round(mean(data$reviews_per_month, na.rm = TRUE))

sum(is.na(data$cleaning_fee))
data$cleaning_fee[is.na(data$cleaning_fee)] <- round(mean(data$cleaning_fee, na.rm = TRUE))

sum(is.na(data$host_about))
data$host_about[is.na(data$host_about)] <- round(mean(data$host_about, na.rm = TRUE))

#process NA on scoring
sum(is.na(scoringData$beds))
scoringData$beds[is.na(scoringData$beds)] <- round(mean(scoringData$beds, na.rm = TRUE))

sum(is.na(scoringData$reviews_per_month))
data$reviews_per_month[is.na(data$reviews_per_month)] <- round(mean(data$reviews_per_month, na.rm = TRUE))

sum(is.na(scoringData$cleaning_fee))
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- round(mean(scoringData$cleaning_fee, na.rm = TRUE))

sum(is.na(scoringData$host_about))
data$host_about[is.na(data$host_about)] <- round(mean(data$host_about, na.rm = TRUE))
data$host_about[is.na(data$host_about)]

#check NA
md.pattern(data[,c('review_scores_rating','accommodates','bathrooms','review_scores_value',
                          'extra_people','room_type','number_of_reviews','reviews_per_month','bedrooms',
                          'minimum_nights','amenities','summary','access','description','cleaning_fee',
                          'review_scores_location','review_scores_cleanliness','review_scores_checkin',
                          'review_scores_accuracy','review_scores_communication','host_is_superhost',
                          'host_identity_verified','availability_365','extra_people','beds','availability_90',
                          'availability_60','bed_type','neighbourhood_group_cleansed','zipcode')])
md.pattern(scoringData[,c('review_scores_rating','accommodates','bathrooms','review_scores_value',
                         'extra_people','room_type','number_of_reviews','reviews_per_month','bedrooms',
                         'minimum_nights','amenities','summary','access','description','cleaning_fee',
                         'review_scores_location','review_scores_cleanliness','review_scores_checkin',
                         'review_scores_accuracy','review_scores_communication','host_is_superhost',
                         'host_identity_verified','availability_365','extra_people','beds','availability_90',
                         'availability_60','bed_type','neighbourhood_group_cleansed','zipcode')])

#split data
set.seed(616)
split=createDataPartition(y=data$price,p=0.7,list=F,groups=100)
train=data[split,]
test=data[-split,]

#boosting model in train
set.seed(616)
boost = gbm(price~ review_scores_rating+accommodates+bathrooms+review_scores_value+
              extra_people+room_type+number_of_reviews+reviews_per_month+bedrooms+
              minimum_nights+amenities+summary+access+description+cleaning_fee+
              review_scores_location+review_scores_cleanliness+review_scores_checkin+
              review_scores_accuracy+review_scores_communication+host_is_superhost+
              host_identity_verified+availability_365+extra_people+beds+availability_90+availability_60+
              bed_type+neighbourhood_group_cleansed+zipcode,data=train,distribution = "gaussian",
            n.trees = 3500,interaction.depth=8,shrinkage=0.01)

predBoostTest = predict(boost,newdata=test,n.trees = 3500) 
rmseBoostTest = sqrt(mean((predBoostTest-test$price)^2)); rmseBoostTest

#boosting model in data
set.seed(616)
boost2 = gbm(price~ review_scores_rating+accommodates+bathrooms+review_scores_value+
              extra_people+room_type+number_of_reviews+reviews_per_month+bedrooms+
              minimum_nights+amenities+summary+access+description+cleaning_fee+
              review_scores_location+review_scores_cleanliness+review_scores_checkin+
              review_scores_accuracy+review_scores_communication+host_is_superhost+
              host_identity_verified+availability_365+extra_people+beds+availability_90+availability_60+
              bed_type+neighbourhood_group_cleansed+zipcode,data=data,distribution = "gaussian",
            n.trees = 3500,interaction.depth=8,shrinkage=0.01)
predBoostTest2 = predict(boost2,newdata=test,n.trees = 3500) 
rmseBoostTest2 = sqrt(mean((predBoostTest2-test$price)^2)); rmseBoostTest2

predBoostTest3 = predict(boost1,newdata=train,n.trees = 3500) 
rmseBoostTest3 = sqrt(mean((predBoostTest3-train$price)^2)); rmseBoostTest3

# Read scoring data and apply model to generate predictions
pred=predict(boost2,newdata=scoringData,n.trees = 3500)

# Construct submission from predictions
submissionFile=data.frame(id=scoringData$id, price=pred)
write.csv(submissionFile,'21_submission.csv',row.names = F)

sum(is.na(submissionFile$price))
