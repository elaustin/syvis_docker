library(data.table)
#library(measurements)
#library(RSQLite)

#need data_wide and site_locations

#site_locations
site_locations<-fread("http://staff.washington.edu/elaustin/site_locations_nopred.csv")
site_locations<-site_locations[!is.na(longitude)]

# datechar<-("2017-01-01")
# 
# db.SYdata = dbConnect(SQLite(), dbname="SYdata.sqlite")
# 
# sqlcmd <- paste0("SELECT * FROM wideData WHERE date_day >=", "\"", datechar, "\"")
# 
# data_wide<-data.table(dbGetQuery(db.SYdata, sqlcmd))
# data_wide[,CO:=CO*1000]

data_wide<-fread("http://staff.washington.edu/elaustin/hourly_qa_calibrated_data.csv")
data_wide[,datetime:=as.POSIXct(datetime)]
data_wide[,date_day:=format(datetime, "%Y-%m-%d")]

data_wide[,NO_donovan:=NO_donovan*1000]
data_wide[,NO2_donovan:=NO2_donovan*1000]
data_wide[,NOX_donovan:=NOX_donovan*1000]
data_wide[,OZONE_donovan:=OZONE_donovan*1000]

data_wide[,site:=as.factor(site)]

data_wide<-data_wide[datetime>=as.POSIXct("2017-01-01"),]

data_summary1<-data_wide[,
                        lapply(.SD, FUN = function (x)
                         mean(as.numeric(as.character(x)), na.rm=T)), 
                        .SDcols=c("longitude","latitude",
                                  "pm25","CO","NO","NO2","O3",grep("_donovan",names(data_wide),value=T)),
                        by="site_short"]

setnames(data_wide, c("OZONE_donovan", "PM25HR_donovan"), c("O3_donovan", "pm25_donovan"))



missing_sites<-
 site_locations$site_short[!site_locations$site_short%in%data_summary1$site_short]
missing_sites<-site_locations[site_short%in%missing_sites]
missing_sites<-missing_sites[,c("site_short","latitude","longitude"),with=F]
data_summary1<-rbindlist(list(data_summary1,missing_sites),fill=T)
