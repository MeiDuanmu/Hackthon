data_SDR<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/SDR.csv",na.strings=c(" ", "NA"),header=TRUE,sep='\t')

data_Move<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/MoveResri.csv",na.strings=c("", "NA"),header=TRUE,sep='\t')
data_TimeSeries<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/TimeSeries.csv",na.strings=c(" ", "NA"),header=TRUE,sep='\t')

data_Facility<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/HealthFacility.csv",na.strings=c("", "NA"),header=TRUE,sep='\t')
data_Wealth<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/SubNationIndicator.csv",na.strings=c("", "NA"),header=TRUE,sep='\t')
data_TreatCenter<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/TreatCenter.csv",na.strings=c("", "NA"),header=TRUE,sep='\t')
data_RedCrs<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/RedCross.csv",na.strings=c("", "NA"),header=TRUE,sep='\t')

data_Market<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/Market.csv",na.strings=c("", "NA"),header=TRUE,sep='\t')
for i in 
dtest<-data_TimeSeries[which(data_TimeSeries$sdr_id==20 & data_TimeSeries$category=='Cases'),]

dd1<-nrow(dtest)
dd2<-dd1-1
dtest[dd2:dd1,]
case_num<-dtest[dd2:dd1,6]
train_iid<-unique(data_TimeSeries$sdr_id)
train_id<-train_iid[train_iid>0]

data_TimeSeries2<-na.omit(data_TimeSeries)
data_Facility2<-na.omit(data_Facility)
data_Wealth2<-na.omit(data_Wealth)
data_Treat2<-na.omit(data_TreatCenter)
data_RedCrs2 <-na.omit(data_RedCrs)
data_M2<-merge(data_Facility2,data_Wealth2,by=c("sdr_id"))
LiberiaPopu<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/LiberiaPopu.csv",na.strings=c("", "NA"),header=TRUE,sep='\t')

intersect(data_TreatCenter$location,data_TimeSeries$sdr_name)
intersect(data_TreatCenter$sdr_id,data_Move$sdr_id)
intersect(intersect(data_TimeSeries$sdr_id,data_Wealth$sdr_id),data_TreatCenter$sdr_id)



#L1<-length(unique(data_Move$sdr_name))
L2<-length(unique(data_TimeSeries$sdr_name))
L3<-length(unique(data_Facility$sdr_name))
L4<-length(unique(data_Wealth$sdr_name))
L5<-length(unique(data_TreatCenter$sdr_name))
#L6<-length(unique(data_RedCrs$sdr_name))
data_M<-merge(data_Facility,data_Wealth,by=c("sdr_id"))

#########

dataSelet1<-data_SDR[c('country_code','gn_latitude','gn_longitude','gn_population','level','name')]
data_Selet<-na.omit(dataSelet1)
#dataSDRpopu<-data_Selet[which(data_Selet$gn_population>0),]
dataSDRpopu<-data_Selet
head(dataSDRpopu)
names(dataSDRpopu) <- c("country_code", "gn_latitude","gn_longitude","gn_population","sdr_level","sdr_name")
#save(dataSDRpopu,file='SDRpopu.csv')
summaryHarv<-read.csv("/Users/meiduanmu/Documents/DataFest_HackEbola/summarywithfeatures.csv",na.strings=c("", "NA"),header=TRUE,sep=',')
summaryHarv2<-summaryHarv[(is.na(summaryHarv$sdr_name)=='FALSE'),]
#To train
dataTime<-summaryHarv2[c('country_code','sdr_name','sdr_id','sdr_level','total.cases')]
mergeData<-merge(dataTime,dataSDRpopu,by=c("sdr_name","sdr_level"))
mergeData1<-mergeData[-c(37,44,45,55),]
#quantile quantile(mergeData1$total.cases,probs=0.5)
# 11,38,284
mergeData1$prev[mergeData1$total.cases>284]<-3
mergeData1$prev[mergeData1$total.cases<=284 & mergeData1$total.cases>11]<-2
#mergeData1$prev[mergeData1$total.cases<=38 & mergeData1$total.cases>11]<-2
mergeData1$prev[mergeData1$total.cases<=11]<-1
mergeData1$prev<-as.factor(mergeData1$prev)
mergeData1[c('total.cases','prev')]
trainData<-mergeData1[c('sdr_level','gn_latitude','gn_longitude')]
ebola.rf<-randomForest(mergeData1$prev~.,data=trainData)
testData<-dataSDRpopu[c('sdr_level','gn_latitude','gn_longitude')]
ebola.pred<-predict(ebola.rf,testData)





for (i in 1:61){
  temp_name<-dataTime$sdr_name[i]
 data_temp<-dataSDRpopu[which(dataSDRpopu$name==temp_name),] 
 dataTime$population[i]<-data_temp$gn_population  
dataTime$longitude[i]<-data_temp$gn_longitude
dataTime$lattitude[i]<-data_temp$gn_latitude
}







colnames(data_Move)
map <- createLeafletMap(session, "myMap")
map$setView(0, 0, 8)
data(quakes)
q.dat <- toGeoJSON(data=quakes[1:99,], dest=tempdir(), name="quakes")
q.style <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5),style.val=rev(heat.colors(5)), leg="Richter Magnitude",fill.alpha=0.7, rad=8)
q.map <- leaflet(data=q.dat, dest=tempdir(), title="Fiji Earthquakes",base.map="mqsat", style=q.style, popup="mag")
# view map in browser
# Attribute From Timeseries:
# Attriibute from Movement: restrictionscale,
# Attribute from 
unique(data_TimeSeries$sdr_name)
unique(data_TimeSeries$sdr_id)
unique(data_Move$sdr_id)
unique(data_Move$country)




#q.map
# USe the current data in some major regions and predicted other regions where there are possiblility to be in risk or not.
# Connect the regions most people increasing and decreasing, travling info, location, pop, health facility, education
#and predict increasing and decreasing likelihood. 
# The risk of the country
# Then plot in Google chart
# Label the data with several variable.
# 
data_ebola<-data.frame(data_TimeSeries$sdr_name,data_TimeSeries$value,data_TimeSeries$category，data_TimeSeries$date,check.names=TRUE)

## Load data and group cases by month
#ebola_dat <- read.delim("sub-national-time-series-data.csv")
ebola$date <- as.Date(data_TimeSeries$date, origin = "1900-01-01")
#ebola_dat$month <- month(ebola_dat$date)

guinea_dat <- ebola_dat %>%
  filter(country == "Guinea", sdr_name != "") %>%
  group_by(category, month, sdr_name) %>%
  summarise(value = sum(value))

## make sure that classes of columns are correct
guinea_dat$category <- as.character(guinea_dat$category)
guinea_dat$sdr_name <- as.character(guinea_dat$sdr_name)
guinea_dat$value <- as.numeric(guinea_dat$value)









head(data_SDR)
rm(list = ls())