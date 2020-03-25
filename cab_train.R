# Remove the objects from the R environment
rm(list= ls(all=T))

# Set the working directory 
setwd("E:/documents")
getwd()

# Read the data
df = read.csv("train_cab.csv")

#copy the data
copy=df

#check the shape of data
dim(df)

# Check the structure of data
str(df)

#observe the top 10 observations 
head(df,10)

# Change the datatype to proper ones
df$fare_amount = as.numeric(as.character(df$fare_amount))

# Missing value Analysis
missing_value = data.frame(apply(df,2,function(x){sum(is.na(x))}))

# Impute the missing values in fare_amount using median
df$fare_amount[is.na(df$fare_amount)] = mean(df$fare_amount,na.rm =T)


# Replacing NAN values with 0
df$passenger_count[is.na(df$passenger_count)] = 0

# Replacing all the values above 7 with 0's
#I am assuming that 0 as the missing value
df[df$passenger_count>7,"passenger_count"]=0

df$passenger_count = as.integer(df$passenger_count)

unique(df$passenger_count) # Now it makes sence of the data
df$passenger_count = as.factor(df$passenger_count)

# Function to find the mode
getmode <- function(v) {
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
}

pass_mode = getmode(df$passenger_count)


df[df$passenger_count==0,"passenger_count"]=pass_mode


# Outlier analysis

boxplot(df$fare_amount)

df = df[!(df$fare_amount>3000),]

boxplot(df$pickup_longitude)

df=df[!(df$pickup_longitude>-40),]

boxplot(df$pickup_latitude)

df=df[!(df$pickup_latitude>100),]

boxplot(df$dropoff_latitude)

df=df[!(df$dropoff_latitude<25),]

boxplot(df$dropoff_longitude)

df=df[!(df$dropoff_longitude>-40),]



#Cleaning of the data 
df[df$fare_amount=='430-','fare_amount']=430

df=df[!(df$pickup_datetime==43),]

# Converting the pickup_datetime variable into timestamp datatype
df$pickup_datetime = as.Date(df$pickup_datetime,"%Y-%m-%d %H:%M:%S")

#Extract year from pickup_datetime variable
df$year = as.factor(format(df$pickup_datetime, "%Y"))

# Extract month from pickup_datetime variable
df$month = as.factor(format(df$pickup_datetime,"%m"))

#Extract the day from pickup_datetime variable
df$day = as.factor(format(df$pickup_datetime,"%d"))


# Delete pickup_datetime variable
df = df[,-c(2)]

#Calculate latitude distance
df$dist_latitude = df$pickup_latitude-df$dropoff_latitude

#Calculating longitude distance
df$dist_longitude = df$pickup_longitude-df$dropoff_longitude



# Correlation Analysis
num_index=sapply(df,is.numeric)
library(corrgram)
corrgram(df[,num_index],order = F,upper.panel = panel.pie,text.panel = panel.txt,main='correlation plot')
# There are no variables which are highly correlated


#chisquare Analysis
fac_index=sapply(df,is.factor)
fac_data=df[,fac_index]

for(i in 1:5){
  for(j in 1:5){
    print(chisq.test(table(fac_data[,i],fac_data[,j])))
  }
}

# Month and year variables are dependent on each other. So its better to remove one variable from our analysis.
df = df[,-c(8,9)]

df$passenger_count = as.integer(df$passenger_count)

#One Hot Encoding
library(caret)
dummy = dummyVars(" ~ .", data=df)
newdata = data.frame(predict(dummy, newdata = df))


# Scaling of data
num_index=sapply(df,is.numeric)
num_data=df[,num_index]
num_data_col=names(num_data)

for(i in num_data_col){
  newdata[,i]=((newdata[,i]-min(newdata[,i]))/(max(newdata[,i])-min(newdata[,i])))
}

#splitting of data
x=newdata[,-c(1)]
y=newdata[,c(1)]
y=data.frame(y)
names(y)[1]='fare'

train_size=0.7*nrow(newdata)
train_size=as.integer(train_size)

x_train=x[1:train_size,]
y_train=y[1:train_size,]


x_test=x[train_size:nrow(newdata),]
y_test=y[train_size:nrow(newdata),]

train=newdata[1:train_size,]
test=newdata[train_size:nrow(newdata),]



# Applying different models
library(randomForest)

RF_model = randomForest(fare_amount ~. , train, importance = TRUE, ntree =500)

RF_predictions = predict(RF_model,test[,-1])

#Model evaluation
#Root Mean Squared Error
rmse=RMSE(RF_predictions,test[,1])

#Mean Absolute Error
mae =MAE(RF_predictions,test[,1])


