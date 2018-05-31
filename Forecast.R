# Load packages
library(tidyverse)
library(ggplot2)
library(forecast)
library(corrplot)
# Read data
vdetail<-read.csv("temple.csv", encoding = "UTF-8")

# Clean data
head(vdetail)
str(vdetail)
vdetail$Timestamp<-strptime(vdetail$Timestamp, "%m/%d/%y %H:%M")
vdetail$Date<-as.Date(vdetail$Timestamp)

#Format data
vdetail<- vdetail[,2:6] %>% group_by(Date) %>% summarise_all(mean)
summary(vdetail)

#Plots: Overall and weekly

#Check correlations
C<-cor(vdetail[,-1])
corrplot(C, method=c("number"))
vdetail$Week<-format(vdetail$Date, "%W")

#Visitors
ggplot(vdetail, mapping=aes(Date, Visitors))+geom_line()+theme_classic()
ggplot(vdetail, mapping=aes(Date, Visitors))+geom_line()+scale_x_date(date_labels = "%W", date_breaks = "1 week")
#ggplot(vdetail, mapping=aes(Timestamp, Visitors))+geom_line()+scale_x_datetime(date_labels = "%p-%d", date_breaks = "72 hours")
# Milk
ggplot(vdetail, mapping=aes(Date, Milk))+geom_line()+theme_classic()
ggplot(vdetail, mapping=aes(Date, Milk))+geom_line()+scale_x_date(date_labels = "%W", date_breaks = "1 week")
#ggplot(vdetail, mapping=aes(Timestamp, Milk))+geom_line()+scale_x_datetime(date_labels = "%p-%d", date_breaks = "72 hours")
#Coconuts
ggplot(vdetail, mapping=aes(Date, Coconuts))+geom_line()+theme_classic()
ggplot(vdetail, mapping=aes(Date, Coconuts))+geom_line()+scale_x_date(date_labels = "%W", date_breaks = "1 week")
#ggplot(vdetail, mapping=aes(Timestamp, Coconuts))+geom_line()+scale_x_datetime(date_labels = "%p-%d", date_breaks = "72 hours")

#Trend analysis
ggplot(vdetail, mapping=aes(Date, Visitors))+geom_line()+scale_x_date(date_labels = "%d", date_breaks = "3 days") + geom_smooth(span=0.3)
ggplot(vdetail, mapping=aes(Date, Milk))+geom_line()+scale_x_date(date_labels = "%d", date_breaks = "3 days") + geom_smooth(span=0.3)
ggplot(vdetail, mapping=aes(Date, Coconuts, group = Week)) + scale_x_date(date_labels = "%W", date_breaks = "1 week")+ geom_line(aes(colour = Week))
ggplot(vdetail, mapping=aes(Date, Milk, group = Week)) + geom_line(aes(colour = Week))
vdetail$ts<-ts(vdetail$Visitors, frequency = 7)
vdetail$mts<-ts(vdetail$Milk, frequency = 7)
vdetail$cts<-ts(vdetail$Coconuts, frequency = 7)
vdetail_stl<-stl(vdetail$ts, s.window = "periodic")
vmilk_stl<-stl(vdetail$mts, s.window = "periodic")
vcoconuts_stl<-stl(vdetail$cts, s.window = "periodic")
plot(vdetail_stl)
plot(vmilk_stl)
plot(vcoconuts_stl)
monthplot(vdetail_stl)
monthplot(vmilk_stl)
monthplot(vcoconut_stl)

#Modeling: Visit, Milk
visit_train<-window(vdetail$ts, start=c(1),end=c(7.2))
visit_test<-window(vdetail$ts, start=c(7.21))
milk_train<-window(vdetail$mts, start=c(1),end=c(7.2))
milk_test<-window(vdetail$mts, start=c(7.21))
  
visit_model_mam<-ets(visit_train,model="MAM")
visit_model_aaa<-ets(visit_train,model="AAA")
visit_model<-ets(visit_train)
milk_model_zaa<-ets(milk_train,model="MMM")
milk_model_maz<-ets(milk_train,model="ZNM")
milk_model<-ets(milk_train)

visit_mam_fc<-forecast(visit_model_mam, h=15)
visit_aaa_fc<-forecast(visit_model_aaa, h=15)
milk_zaa_fc<-forecast(milk_model_zaa, h=15)
milk_maz_fc<-forecast(milk_model_maz, h=15)
milk_fc<-forecast(milk_model, h=15)
visit_fc<-forecast(visit_model, h=15)

Day<-14:28
milk_fc_df <- cbind(Day, as.data.frame(milk_fc))
rownames(milk_fc_df)<-Day
names(milk_fc_df) <- gsub(" ", "_", names(milk_fc_df))
milk_fc_df$Date <- as.Date(paste("18-02-", milk_fc_df$Day, sep = ""), format = "%y-%m-%d")
milk_fc_df$Model<-rep("ets")
head(milk_fc_df)

visit_fc_df <- cbind(Day, as.data.frame(visit_fc))
rownames(visit_fc_df)<-Day
names(visit_fc_df) <- gsub(" ", "_", names(visit_fc_df))
visit_fc_df$Date <- as.Date(paste("18-02-", visit_fc_df$Day, sep = ""), format = "%y-%m-%d")
visit_fc_df$Model<-rep("ets")
head(visit_fc_df)

visit_mam_fc_df<-cbind(Day, as.data.frame(visit_mam_fc))
rownames(visit_mam_fc_df)<-Day
names(visit_mam_fc_df) <- gsub(" ", "_", names(visit_mam_fc_df))
visit_mam_fc_df$Date <- as.Date(paste("18-02-", visit_mam_fc_df$Day, sep = ""), format = "%y-%m-%d")
visit_mam_fc_df$Model<-rep("mam")
head(visit_mam_fc_df)

milk_zaa_fc_df<-cbind(Day, as.data.frame(milk_zaa_fc))
rownames(milk_zaa_fc_df)<-Day
names(milk_zaa_fc_df) <- gsub(" ", "_", names(milk_zaa_fc_df))
milk_zaa_fc_df$Date <- as.Date(paste("18-02-", milk_zaa_fc_df$Day, sep = ""), format = "%y-%m-%d")
milk_zaa_fc_df$Model<-rep("zaa")
head(milk_zaa_fc_df)

visit_aaa_fc_df<-cbind(Day, as.data.frame(visit_aaa_fc))
rownames(visit_aaa_fc_df)<-Day
names(visit_aaa_fc_df) <- gsub(" ", "_", names(visit_aaa_fc_df))
visit_aaa_fc_df$Date <- as.Date(paste("18-02-", visit_aaa_fc_df$Day, sep = ""), format = "%y-%m-%d")
visit_aaa_fc_df$Model<-rep("aaa")
head(visit_aaa_fc_df)

milk_maz_fc_df<-cbind(Day, as.data.frame(milk_maz_fc))
rownames(milk_maz_fc_df)<-Day
names(milk_maz_fc_df) <- gsub(" ", "_", names(milk_maz_fc_df))
milk_maz_fc_df$Date <- as.Date(paste("18-02-", milk_maz_fc_df$Day, sep = ""), format = "%y-%m-%d")
milk_maz_fc_df$Model<-rep("maz")
head(milk_maz_fc_df)

forecast_visit_all <- rbind(visit_fc_df, visit_mam_fc_df, visit_aaa_fc_df)
forecast_milk_all <- rbind(milk_fc_df, milk_zaa_fc_df, milk_maz_fc_df)
head(forecast_visit_all)
tail(forecast_visit_all)
head(forecast_milk_all)
tail(forecast_milk_all)

ggplot() +  geom_line(data = vdetail, aes(x = Date, y = Visitors)) +  # Plotting original data
  geom_line(data = forecast_visit_all, aes(x = Date, y = Point_Forecast, colour = Model)) +  # Plotting model forecasts
  theme_classic()

ggplot() +  geom_line(data = vdetail, aes(x = Date, y = Milk)) +  # Plotting original data
  geom_line(data = forecast_milk_all, aes(x = Date, y = Point_Forecast, colour = Model)) +  # Plotting model forecasts
  theme_classic()

accuracy(milk_fc, milk_test)
accuracy(milk_zaa_fc, milk_test)
accuracy(milk_maz_fc, milk_test)

accuracy(visit_fc, visit_test)
accuracy(visit_aaa_fc, visit_test)
accuracy(visit_mam_fc, visit_test)
