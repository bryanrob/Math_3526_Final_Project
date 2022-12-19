#FINAL PROJECT
#Robert Bryant
#
#Repository available at:
#https://github.com/bryanrob/Math_3526_Final_Project

#CLEAR MEMORY
rm(list=ls())

setDir<-function(){
  require('rstudioapi')
  setwd(dirname(getSourceEditorContext()$path))
}
setDir()
rm(setDir)

readFirstLine<-function(fileDir){
  fileStream<-file(fileDir)
  returnThis<-readLines(fileStream)[1]
  close(fileStream)
  rm(fileStream)
  return(returnThis)
}
writeFirstLine<-function(fileDir,line){
  fileStream<-file(fileDir)
  writeLines(line,fileStream)
  close(fileStream)
  rm(fileStream)
}

callAPI<-function(ticker,outputSize,key){
  print(paste("Calling API for",ticker,"data."))
  url<-gsub(" ","",paste("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=",ticker,"&datatype=csv&outputsize=",outputSize,"&apikey=",key))
  #print("Calling API...")
  #print(url)
  #print("Reading Data...")
  results=read.csv(url)
  #returnThis<-results
  #print("Exporting Results...")
  exportFile<-gsub(" ","",paste("./stock_data/",tolower(ticker),".csv"))
  print(paste("Results retreived.  Exporting to file: (",exportFile,")"))
  write.csv(results,exportFile,row.names=FALSE)
  #rm(url,results,exportFile)
  #return(returnThis)
  rm(url,results)
  return(exportFile)
}

#Part 1) Acquiring Data
#Read file containing the API key.  The API will refuse service without providing a key.
if (!file.exists("keyFile.txt")){
  cat("KEY FILE NOT FOUND!!!\nPlease save your Alpha Vantage API key in 'keyfile.txt'\nYou can obtain an API key from: (https://www.alphavantage.co/support/#api-key).\n")
  file.create("keyFile.txt")
} 
key<-readFirstLine("keyFile.txt")
if(key==""){
  cat("KEY IS NULL!!!\nPlease save your Alpha Vantage API key in 'keyfile.txt' before continuing the program.\nYou can obtain an API key from: (https://www.alphavantage.co/support/#api-key).\n")
}

#Specify the ticker symbols of the stock data we want to obtain.
#Change the line below to include whatever companies that you want.
tickers<-c("AMZN","GOOGL","AAPL","MSFT","TSLA","NTDOY","SONY","RBLX")

#Set "outputSize" for either of the following:
# - "compact" for only past 100 days.
# - "full" for the past 20 years.
outputSize="full" 

#Declare the existence of {data}.  Information from the API will be put into {data}.
data<-list()

#First, check if the API was accessed today.  If not, get data from the API: put it into
#memory and save as files on drive.  Otherwise, access data from drive (if it exists).
#Operations reduce unecessary calls to the API.
today<-Sys.Date()
dateFileDir<-"./stock_data/_lastCallDate.txt"
if (!file.exists(dateFileDir)){
  dir.create("./stock_data")
  file.create(dateFileDir)
  writeFirstLine(dateFileDir,format(today+1,"%Y-%m-%d"))
} 
dateFromFile<-as.Date(readFirstLine(dateFileDir),"%Y-%m-%d")
if(dateFromFile!=today){
  writeFirstLine(dateFileDir,format(today,"%Y-%m-%d"))
  #For each ticker symbol, obtain the daily-adjusted reports for the respective symbol from
  #the online API.
  for(i in seq(1,length(tickers))){
    ticker=tickers[i]
    data$dirs[i]<-callAPI(ticker,outputSize,key)
    Sys.sleep(15)
  }
}else{
  #For each ticker symbol, obtain the daily-adjusted reports for the respective symbol from
  #the local files.  If file does not exist, get data from online API.
  for(i in seq(1,length(tickers))){
    ticker=tickers[i]
    stockFile=gsub(" ","",paste("./stock_data/",tolower(ticker),".csv"))
    if(!file.exists(stockFile)){
      data$dirs[i]<-callAPI(ticker,outputSize,key)
      Sys.sleep(15)
    }
    data$dirs[i]=stockFile
    if(is.null(data$dirs[i])){
      data$dirs[i]<-callAPI(ticker,outputSize,key)
      Sys.sleep(15)
    }
  }
  rm(stockFile)
}

#Cleanup Part 1...
rm(key,outputSize,i,ticker,today,dateFromFile,dateFileDir)
#Data acquisition complete!

#Part 2) Data analysis.
#For demonstration on the GLM model, the first dataset from the list of
#directories will be used...
frame<-read.csv(data$dirs[1])

#Get features & targets...
xFeatures<-as.data.frame(frame[2:4]) #Feature columns: open, high & low.
yTargets<-frame[5] #Closing price is the target.

#Dataset Preparation
#Split the dataset into a training & testing set to see the prediction accuracy.
splitBy=0.05 #For validation testing, split the set into a training & test set.
#trainFrame<-frame[seq((nrow(frame)*splitBy)+1,nrow(frame)),2:5]
#testFrame<-frame[seq(1,nrow(frame)*splitBy),2:5]
xTrain<-xFeatures[seq((nrow(xFeatures)*splitBy)+1,nrow(xFeatures)),]
xTest<-xFeatures[seq(1,nrow(xFeatures)*splitBy),]
yTrain<-yTargets[seq((nrow(yTargets)*splitBy)+1,nrow(yTargets)),]
yTest<-yTargets[seq(1,nrow(yTargets)*splitBy),]


#head(xTrain[,1])
#head(yTrain)

#GLM Model
#Initialize the training set...
y<-yTrain
x<-xTrain$open+xTrain$high+xTrain$low

#Create a GLM model...
model<-glm(y~x)

#Obtain prediction results...
testPrediction<-predict.glm(object=model,newdata=data.frame(x=xTest$open+xTest$high+xTest$low),type="response")
#head(testPrediction)

#Graph demonstration of GLM prediction in action.
#Black = Actual results.
terst<-data.frame(close=testPrediction,time=frame[seq(1,nrow(frame)*splitBy),1])
plot(x=as.Date(terst$time),y=yTest,pch=20,ylab="Closing Price",xlab="Date")
lines(x=as.Date(terst$time),y=yTest)
#Red = Predicted results.
points(x=as.Date(terst$time),y=terst$close,col="red")
lines(x=as.Date(terst$time),y=terst$close,col="red")

#Cleanup.
rm(testPrediction,terst,x,y,model)

#Try predicting without knowing the open/high/low values of each day...
daysToPredict=30

#Create the training dataset...
terstTrain<-list(
  timestamp=frame$timestamp[(daysToPredict+1):nrow(frame)],
  open=frame$open[(daysToPredict+1):nrow(frame)],
  high=frame$high[(daysToPredict+1):nrow(frame)],
  low=frame$low[(daysToPredict+1):nrow(frame)],
  close=frame$close[(daysToPredict+1):nrow(frame)]
)

#plot(y=frame$close[1:(daysToPredict*3)],x=as.Date(frame$timestamp[1:(daysToPredict*3)]),pch=20,xlab="Date",ylab="Closing Price")
#lines(y=frame$close[1:(daysToPredict*3)],x=as.Date(frame$timestamp[1:(daysToPredict*3)]))
#lines(y=terstTrain$close[(daysToPredict+1):(daysToPredict*3)],x=as.Date(terstTrain$timestamp[(daysToPredict+1):(daysToPredict*3)]),col="red",pch=20)

#Begin loop to predict next few days of stocks...
for(i in seq(1,daysToPredict)){
  #If the day is a weekend, set the next date to the upcoming Monday.
  nextDate<-as.Date(terstTrain$timestamp[1])+1
  day<-strftime(nextDate,"%A")
  if(day=="Saturday"){
    nextDate<-nextDate+2
  }else if(day=="Sunday"){
    nextDate<-nextDate+1
  }
  terstTrain$timestamp<-append(nextDate,terstTrain$timestamp)
  
  #Get the opening price of the next day (for improvement, will need to be
  #modified to read from API.)
  nextOpen<-terstTrain$close[1]
  
  #Predict the high value of the next day...
  x<-terstTrain$open
  y<-terstTrain$high
  model<-glm(y~x)
  terstTrain$high<-append(predict.glm(object=model,newdata=data.frame(x=nextOpen)),terstTrain$open)
  
  #Predict the low value of the next day...
  y<-terstTrain$low
  model<-glm(y~x)
  terstTrain$low<-append(predict.glm(object=model,newdata=data.frame(x=nextOpen)),terstTrain$low)
  
  #Save the next opening value...
  terstTrain$open<-append(nextOpen,terstTrain$open)
  
  #Predict the closing value of the next day...
  x<-terstTrain$open[2:length(terstTrain$open)]+terstTrain$high[2:length(terstTrain$high)]+terstTrain$low[2:length(terstTrain$low)]
  y<-terstTrain$close
  model<-glm(y~x)
  terstTrain$close<-append(predict.glm(object=model,newdata=data.frame(x=terstTrain$open[1]+terstTrain$high[1]+terstTrain$low[1])),terstTrain$close)
}
#Cleanup loop objects...
rm(i,nextDate,day,nextOpen,x,y)

#Plot results of this prediction method.
#Black = Actual results.
plot(y=frame$close[1:(daysToPredict*1.5)],x=as.Date(frame$timestamp[1:(daysToPredict*1.5)]),pch=20,xlab="Date",ylab="Closing Price",ylim=c(min(frame$close[1:(daysToPredict*3)])-(min(frame$close[1:(daysToPredict*3)])/8),max(frame$close[1:(daysToPredict*3)])+(max(frame$close[1:(daysToPredict*3)])/8)))
lines(y=frame$close[1:(daysToPredict*1.5)],x=as.Date(frame$timestamp[1:(daysToPredict*1.5)]))
#Red = Predicted results.
points(y=terstTrain$close[1:(daysToPredict)],x=as.Date(terstTrain$timestamp[1:(daysToPredict)]),col="red")
lines(y=terstTrain$close[1:(daysToPredict)],x=as.Date(terstTrain$timestamp[1:(daysToPredict)]),col="red")

#Why is it less accurate?
#So far, the program is set to believe that the opening value of the next day is
#the same as the closing value of the day before then.  In real life, this is
#not the case: the opening value is actually equal to the last traded value of
#that stock.  To get this data, more calls to the Alpha Vantage API are
#necessary, which I have not been able to implement yet.
#
#The fix is to change the API calls to use the intraday function, rather
#than the daily of the stock.  However, this function will greatly increase the
#time needed pulling data, since it's gathering nearly all trades that occurred
#during each day for up to 2 years.

#So...  How about just predicting the next day?
daysToPredict=1

terstTrain<-list(
  timestamp=frame$timestamp[(daysToPredict+1):nrow(frame)],
  open=frame$open[(daysToPredict+1):nrow(frame)],
  high=frame$high[(daysToPredict+1):nrow(frame)],
  low=frame$low[(daysToPredict+1):nrow(frame)],
  close=frame$close[(daysToPredict+1):nrow(frame)]
)

nextDate<-as.Date(terstTrain$timestamp[1])+1
day<-strftime(nextDate,"%A")
if(day=="Saturday"){
  nextDate<-nextDate+2
}else if(day=="Sunday"){
  nextDate<-nextDate+1
}
terstTrain$timestamp<-append(nextDate,terstTrain$timestamp)

#Get the opening price of the next day (for improvement, will need to be
#modified to read from API.)
nextOpen<-terstTrain$close[1]

#Predict the high value of the next day...
x<-terstTrain$open
y<-terstTrain$high
model<-glm(y~x)
terstTrain$high<-append(predict.glm(object=model,newdata=data.frame(x=nextOpen)),terstTrain$open)

#Predict the low value of the next day...
y<-terstTrain$low
model<-glm(y~x)
terstTrain$low<-append(predict.glm(object=model,newdata=data.frame(x=nextOpen)),terstTrain$low)

#Save the next opening value...
terstTrain$open<-append(nextOpen,terstTrain$open)

#Predict the closing value of the next day...
x<-terstTrain$open[2:length(terstTrain$open)]+terstTrain$high[2:length(terstTrain$high)]+terstTrain$low[2:length(terstTrain$low)]
y<-terstTrain$close
model<-glm(y~x)
terstTrain$close<-append(predict.glm(object=model,newdata=data.frame(x=terstTrain$open[1]+terstTrain$high[1]+terstTrain$low[1])),terstTrain$close)

#Actual values of most recent 3 days...
plot(y=frame$close[1:3],x=as.Date(frame$timestamp[1:3]),ylab="Closing Price",xlab="Date",pch=20)
lines(y=frame$close[1:3],x=as.Date(frame$timestamp[1:3]))
#Predicted value...
points(y=terstTrain$close[1],x=as.Date(terstTrain$timestamp[1]),col="red")

#Also not good, since the program thought that the stock would go up on Friday,
#when it in fact went down.

#Apply the prediction model to each stock to predict the next day...

#Prepare variable to insert the predictions into...
predictions=list()
counter<-1
for (directory in data$dirs){
  frame<-read.csv(directory)
  
 terstTrain<-list(
   timestamp=frame$timestamp[(daysToPredict):nrow(frame)],
   open=frame$open[(daysToPredict):nrow(frame)],
   high=frame$high[(daysToPredict):nrow(frame)],
   low=frame$low[(daysToPredict):nrow(frame)],
   close=frame$close[(daysToPredict):nrow(frame)]
 )
 
 nextDate<-as.Date(terstTrain$timestamp[1])+1
 day<-strftime(nextDate,"%A")
 if(day=="Saturday"){
   nextDate<-nextDate+2
 }else if(day=="Sunday"){
   nextDate<-nextDate+1
 }
 terstTrain$timestamp<-append(nextDate,terstTrain$timestamp)
 
 #Get the opening price of the next day (for improvement, will need to be
 #modified to read from API.)
 nextOpen<-terstTrain$close[1]
 
 #Predict the high value of the next day...
 x<-terstTrain$open
 y<-terstTrain$high
 model<-glm(y~x)
 terstTrain$high<-append(predict.glm(object=model,newdata=data.frame(x=nextOpen)),terstTrain$open)
 
 #Predict the low value of the next day...
 y<-terstTrain$low
 model<-glm(y~x)
 terstTrain$low<-append(predict.glm(object=model,newdata=data.frame(x=nextOpen)),terstTrain$low)
 
 #Save the next opening value...
 terstTrain$open<-append(nextOpen,terstTrain$open)
 
 #Predict the closing value of the next day...
 x<-terstTrain$open[2:length(terstTrain$open)]+terstTrain$high[2:length(terstTrain$high)]+terstTrain$low[2:length(terstTrain$low)]
 y<-terstTrain$close
 model<-glm(y~x)
 terstTrain$close<-append(predict.glm(object=model,newdata=data.frame(x=terstTrain$open[1]+terstTrain$high[1]+terstTrain$low[1])),terstTrain$close)
 
 #Add values to the {predictions} variable...
 predictions$ticker[counter]=tickers[counter]
 predictions$previous[counter]=terstTrain$close[2]
 predictions$expectedNext[counter]=terstTrain$close[1]
 if(predictions$previous[counter]<predictions$expectedNext[counter]){
   predictions$trend[counter]="Up"
 }else if(predictions$previous[counter]>predictions$expectedNext[counter]){
   predictions$trend[counter]="Down"
 }else{
   predictions$trend[counter]="Same"
 }
 
 #Increment counter for next iteration...
 counter<-counter+1
}
rm(counter,frame,xFeatures,yTargets,xTrain,xTest,yTrain,yTest,splitBy,terstTrain)

#Print the results of the predictions.
predictions<-as.data.frame(predictions)
for(i in seq(1:nrow(predictions))){
  cat(paste(
    "Ticker:",predictions$ticker[i],"\n\tClosing Price\nPrevious:",predictions$previous[i],"\tNext:",predictions$expectedNext[i],
    "\nTrend:",predictions$trend[i],"\n\n"
  ))
}

#Next goal: Use a different prediction algorithm to (hopefully)
#get better results, as well as configure the program to get
#stock intraday data.

#__CLEANUP__
rm(list=ls())
library(pacman)
p_unload(all)
graphics.off()
cat("\014")
