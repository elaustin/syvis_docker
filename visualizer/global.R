library(data.table)
#library(measurements)
library(RSQLite)

#need data_wide and site_locations

#site_locations
site_locations<-fread("http://staff.washington.edu/elaustin/site_locations_nopred.csv")

# datechar<-("2017-01-01")
# 
# db.SYdata = dbConnect(SQLite(), dbname="SYdata.sqlite")
# 
# sqlcmd <- paste0("SELECT * FROM wideData WHERE date_day >=", "\"", datechar, "\"")
# 
# data_wide<-data.table(dbGetQuery(db.SYdata, sqlcmd))
# data_wide[,CO:=CO*1000]

data_wide<-fread("http://staff.washington.edu/elaustin/hourly_qa_calibrated_data_04_07_2017.csv")

data_summary1<-data_wide[,
                        lapply(.SD, FUN = function (x)
                         mean(as.numeric(as.character(x)), na.rm=T)), 
                        .SDcols=c("longitude","latitude",
                                  "pm25","CO","NO","NO2","O3"),
                        by="site_short"]
missing_sites<-
 site_locations$site_short[!site_locations$site_short%in%data_summary1$site_short]
missing_sites<-site_locations[site_short%in%missing_sites]
missing_sites<-missing_sites[,c("site_short","latitude","longitude"),with=F]
data_summary1<-rbindlist(list(data_summary1,missing_sites),fill=T)
