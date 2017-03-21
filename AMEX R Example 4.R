#####Reading in data
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(RCurl)
library(XML)
library(xts)
library(plyr)
library(reshape2)
library(ReporteRs)
require(lubridate)
library(scales)
library(grid)
library(gridExtra)
library(TTR)
library(RODBC)

#NA removal function
completeFunc <- function(data, desiredCols) 
{
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
} 

#Online MySQL Database
watSQLconn <- odbcConnect("MySQL",uid="waterin0_regutil",pwd="Longbeach04!")
sqlTables(watSQLconn, tableType = "TABLE")$TABLE_NAME
#Reading in MySQL DB Table
watsys <- sqlFetch(watSQLconn, "allwatersyst")
watviol <- sqlFetch(watSQLconn, "watsysviol")
odbcCloseAll()
#Cleaning the dataframes
watviol$ID <- NULL

#Overview of water systems
cws_db <- subset(watsys, PWS_TYPE_CODE == c("CWS") | PWS_TYPE_CODE == c("NP"))
cws_db <- subset(cws_db, PWS_ACTIVITY_CODE == c("A"))
#defining quintiles for customer connections
quantile(cws_db$SERVICE_CONNECTIONS_COUNT, c(.05,.30,.70,.95))
#categorical customers served
cws_db$catcust <- ""
cws_db$catcust[cws_db$SERVICE_CONNECTIONS_COUNT >= 7000] <- c("Very Large")
cws_db$catcust[cws_db$SERVICE_CONNECTIONS_COUNT < 7000 & cws_db$SERVICE_CONNECTIONS_COUNT >= 1000] <- c("Large")
cws_db$catcust[cws_db$SERVICE_CONNECTIONS_COUNT < 1000 & cws_db$SERVICE_CONNECTIONS_COUNT >= 35] <- c("Medium")
cws_db$catcust[cws_db$SERVICE_CONNECTIONS_COUNT < 35 & cws_db$SERVICE_CONNECTIONS_COUNT >= 15] <- c("Small")
cws_db$catcust[cws_db$SERVICE_CONNECTIONS_COUNT < 15] <- c("Very Small")
cws_db$catcust <- as.factor(cws_db$catcust)
#categorical customer type: public or investor owned
cws_db$custtypes <- ""
cws_db$custtypes[cws_db$OWNER_TYPE_CODE != c("P")] <- c("Government Owned")
cws_db$custtypes[is.na(cws_db$OWNER_TYPE_CODE)] <- c("Government Owned")
cws_db$custtypes[cws_db$OWNER_TYPE_CODE == c("P")] <- c("Investor Owned")

#complete bar plots for count of systems
#setting color for barplots
conn_color <- c("cadetblue","coral3","darkgoldenrod","darkorange2","khaki1")
own_color <- c("burlywood3","aquamarine3")
#reordering and naming specific factors
cws_db$catcust <- factor(cws_db$catcust, levels = c("Very Large","Large","Medium","Small","Very Small"))
cws_db <- within(cws_db, PRIMACY_AGENCY_CODE <- factor(PRIMACY_AGENCY_CODE, 
                                                       levels = names(sort(table(PRIMACY_AGENCY_CODE),
                                                                           decreasing = TRUE))))
write.csv(data.frame(cws_db),"E:/Commentaries/Water Systems/CWS_DB.csv")
#split by customer type
ggplot(cws_db, aes(x = PRIMACY_AGENCY_CODE, fill = catcust)) +
  geom_bar() +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Water Systems by State and Service Connection", 
       x = "", 
       y = "System Count\n")+
  scale_fill_manual(values = conn_color,
                    name = "Size of System by Connections",
                    breaks = c("Very Large","Large","Medium","Small","Very Small"),
                    labels = c("Very Large","Large","Medium","Small","Very Small"))
#split by customer size
ggplot(cws_db, aes(x = PRIMACY_AGENCY_CODE, fill = custtypes)) +
  geom_bar() +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Water Systems by State and Owner Type", 
       x = "", 
       y = "System Count\n")+
  scale_fill_manual(values = own_color,
                    name = "Owner Type",
                    breaks = c("Government Owned","Investor Owned"),
                    labels = c("Government Owned","Investor Owned"))

#complete bar plots for systems by populations served
#split by customer type
cws_db <- with(cws_db, cws_db[order(PRIMACY_AGENCY_CODE, catcust),])
ggplot(cws_db, aes(x = reorder(PRIMACY_AGENCY_CODE,-POPULATION_SERVED_COUNT, sum), 
                   y = (POPULATION_SERVED_COUNT/1000000), 
                   fill = catcust)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Population Served by State and Service Connection", 
       x = "", 
       y = "Population served (millions)\n")+
  scale_fill_manual(values = conn_color,
                    name = "Size of System by Connections",
                    breaks = c("Very Large","Large","Medium","Small","Very Small"),
                    labels = c("Very Large","Large","Medium","Small","Very Small"))
#split by customer size
cws_db <- with(cws_db, cws_db[order(PRIMACY_AGENCY_CODE, custtypes),])
ggplot(cws_db, aes(x = reorder(PRIMACY_AGENCY_CODE,-POPULATION_SERVED_COUNT, sum), 
                   y = (POPULATION_SERVED_COUNT/1000000), 
                   fill = custtypes)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Population Served by State and Owner Type", 
       x = "", 
       y = "Population served (millions)\n")+
  scale_fill_manual(values = own_color,
                    name = "Owner Type",
                    breaks = c("Government Owned","Investor Owned"),
                    labels = c("Government Owned","Investor Owned"))

#formating the date to plot time series
watviol$PWS_DEACTIVATION_DATE <- as.Date(as.character(watviol$PWS_DEACTIVATION_DATE), format="%Y-%m-%d")
watviol$DeacDmonth <- strftime(watviol$PWS_DEACTIVATION_DATE, "%m")
watviol$DeacDyear <- strftime(watviol$PWS_DEACTIVATION_DATE, "%Y")
watviol$COMPL_PER_BEGIN_DATE <- as.Date(as.character(watviol$COMPL_PER_BEGIN_DATE), format="%Y-%m-%d")
watviol$BegDmonth <- strftime(watviol$COMPL_PER_BEGIN_DATE, "%m")
watviol$BegDyear <- strftime(watviol$COMPL_PER_BEGIN_DATE, "%Y")
watviol$COMPL_PER_END_DATE <- as.Date(as.character(watviol$COMPL_PER_END_DATE), format="%Y-%m-%d")
watviol$EndDmonth <- strftime(watviol$COMPL_PER_END_DATE, "%m")
watviol$EndDyear <- strftime(watviol$COMPL_PER_END_DATE, "%Y")
watviol$RTC_DATE <- as.Date(as.character(watviol$RTC_DATE), format="%Y-%m-%d")
watviol$RTCDmonth <- strftime(watviol$RTC_DATE, "%m")
watviol$RTCDyear <- strftime(watviol$RTC_DATE, "%Y")

#new variables
#binary violation value
watviol$numvio <- ""
watviol$numvio[as.character(watviol$PWSID) == " "] <- 0
watviol$numvio[as.character(watviol$PWSID) != " "] <- 1
#categorical customers served
watviol$catcust <- ""
watviol$catcust[watviol$SERVICE_CONNECTIONS_COUNT >= 3000] <- c("Very Large")
watviol$catcust[watviol$SERVICE_CONNECTIONS_COUNT < 3000 & watviol$SERVICE_CONNECTIONS_COUNT >= 550] <- c("Large")
watviol$catcust[watviol$SERVICE_CONNECTIONS_COUNT < 550 & watviol$SERVICE_CONNECTIONS_COUNT >= 55] <- c("Medium")
watviol$catcust[watviol$SERVICE_CONNECTIONS_COUNT < 55 & watviol$SERVICE_CONNECTIONS_COUNT >= 20] <- c("Small")
watviol$catcust[watviol$SERVICE_CONNECTIONS_COUNT < 20] <- c("Very Small")
watviol$catcust <- as.factor(watviol$catcust)
#categorical customer type: public or investor owned
watviol$custtypes <- ""
watviol$custtypes[watviol$OWNER_TYPE_CODE != c("P")] <- c("Government Owned")
watviol$custtypes[watviol$OWNER_TYPE_CODE == c("P")] <- c("Investor Owned")
watviol$custtypes <- as.factor(watviol$custtypes)

#deleting unneeded columns
watviol$VIOLATION_ID <- NULL
watviol$FACILITY_ID <- NULL
watviol$LATEST_ENFORCEMENT_ID <- NULL

#creating new dataframe with specific dates
violdates <- watviol
violdates <- subset(violdates, EndDyear >= 1996 & EndDyear <= 2016)
violdates <- subset(violdates, violdates$SERVICE_CONNECTIONS_COUNT >= 1)
violdates <- subset(violdates, IS_HEALTH_BASED_IND == 'Y')
violdates <- subset(violdates, SERVICE_CONNECTIONS_COUNT >= 1)

#ordering plot for bars
violdates <- with(violdates, 
                  violdates[order(EndDyear, factor(catcust, 
                                                   levels = c("Very Large","Large",
                                                              "Medium","Small","Very Small"))),])
#complete bar plots
ggplot(violdates, aes(x = EndDyear, fill = catcust)) +
  geom_bar() +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Health Base Water Violations by Connections", 
       x = "", 
       y = "Violation Count\n")+
  scale_fill_manual(values = conn_color,
                    name = "Size of System by Connections",
                    breaks = c("Very Large","Large","Medium","Small","Very Small"),
                    labels = c("Very Large","Large","Medium","Small","Very Small"))

#flint like violations
flintlike <- violdates
flintlike <- subset(flintlike, VIOLATION_CATEGORY_CODE == 'MCL')
flintlike <- subset(flintlike, CONTAMINANT_CODE == '2950' | CONTAMINANT_CODE == '3100')
ggplot(flintlike, aes(x = EndDyear, fill = custtypes)) +
  geom_bar() +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Flint Like Health Violations by Owner Type", 
       x = "", 
       y = "Violation Count\n")+
  scale_fill_manual(values = own_color,
                    name = "Owner Type",
                    breaks = c("Government Owned","Investor Owned"),
                    labels = c("Government Owned","Investor Owned"))

#flint like violations by state
flintlike <- violdates
flintlike <- subset(flintlike, VIOLATION_CATEGORY_CODE == 'MCL')
flintlike <- subset(flintlike, CONTAMINANT_CODE == '2950' | CONTAMINANT_CODE == '3100')
ggplot(flintlike, aes(x = PRIMACY_AGENCY_CODE, fill = custtypes)) +
  geom_bar() +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Flint Like Violations by State and Owner Type", 
       x = "", 
       y = "Violation Count\n")+
  scale_fill_manual(values = own_color,
                    name = "Owner Type",
                    breaks = c("Government Owned","Investor Owned"),
                    labels = c("Government Owned","Investor Owned"))

#aggregating violations by year
yearviol <- aggregate(as.numeric(numvio) ~ EndDyear, violdates, FUN = sum)
yearviol$numvio <- yearviol$`as.numeric(numvio)`
yearviol$`as.numeric(numvio)` <- NULL
#aggregating violations by year and month
monthviol <- aggregate(as.numeric(numvio) ~ EndDmonth + EndDyear, violdates, FUN = sum)
monthviol$numvio <- monthviol$`as.numeric(numvio)`
monthviol$`as.numeric(numvio)` <- NULL
monthviol$date <- as.POSIXct(paste(monthviol$EndDyear, monthviol$EndDmonth, "01", sep = "-"))

#monthly plotting violations
violm_plot <- ggplot(monthviol, aes(x = as.Date(monthviol$date), y = monthviol$numvio))+ 
  geom_line(colour = 'black')+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  scale_x_date(date_breaks = "years", date_labels = "%Y")+
  xlab("\nYear")+
  ylab("Violations\n")+
  scale_y_continuous(limits = c(0,4000))+
  labs(title = "Health Violations/Month")+ 
  coord_cartesian(xlim=c(as.Date("1996-12-31"),as.Date("2015-12-31")))
plot(violm_plot)

#yearly plotting violations
violy_plot <- ggplot(yearviol, aes(x = EndDyear, y = numvio, group = 1)) +
  geom_line(colour = 'black')+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  xlab("\nYear")+
  ylab("Violations\n")+
  scale_y_continuous(limits = c(0,20000))+
  labs(title = "Health Violations/Year")
plot(violy_plot)



#Connecting to Access Database
#watconnect <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=E:/Commentaries/Water Systems/WatSysAccessDB.accdb")