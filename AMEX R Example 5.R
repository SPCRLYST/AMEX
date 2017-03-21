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
library(lubridate)
library(scales)
library(grid)
library(gridExtra)
library(TTR)
library(scatterplot3d)
library(cowplot)

#NA removal function
completeFunc <- function(data, desiredCols) 
{
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
} 
#Custom removal of legends
g_legend <- function(a.gplot)
{
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend) 
}

#coupon data
CoupURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/CouponProj/master/FinalFIUtilities.csv")
coupdata <- read.csv(text = CoupURL)
#US treasury data
USTreasURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/CouponProj/master/USTreasuries.csv")
USTreasdata <- read.csv(text = USTreasURL)

#renaming treasury columns and fixing dates
USTreas <- USTreasdata
USTreas$Date <- as.Date(as.character(USTreas$Date), format="%b-%d-%Y")
USTreas <- rename(USTreas, c("UST...20.Year"="UST20","UST...10.Year"="UST10","UST...5.Year"="UST5",
                             "UST...1.Year"="UST1"))

#US Companies Only
#coupdata <- subset(coupdata, Country == c("United States"))

#formating the date to plot time series
coupdata$MaturityDate <- as.Date(as.character(coupdata$MaturityDate), format="%m/%d/%Y")
coupdata$OfferingDate <- as.Date(as.character(coupdata$OfferingDate), format="%m/%d/%Y")
#formating offering amount as number
coupdata$OfferingAmount <- as.numeric(as.character(coupdata$OfferingAmount))

#creating date variables variable
coupdata$odyear <- strftime(coupdata$OfferingDate, "%Y")
coupdata$odmyear <- as.Date(cut(coupdata$OfferingDate, breaks = "month"))
coupdata$mdyear <- strftime(coupdata$MaturityDate, "%Y")
coupdata$mdmyear <- as.Date(cut(coupdata$MaturityDate, breaks = "month"))
#finding the term of an issuance
coupdata$term <- as.numeric(coupdata$mdyear) - as.numeric(coupdata$odyear)

#higher level rating
coupdata$oRating <- coupdata$Rating
coupdata$oRating[coupdata$oRating == "AAA-"] <- "AAA"
coupdata$oRating[coupdata$oRating == "AA+"] <- "AA"
coupdata$oRating[coupdata$oRating == "AA-"] <- "AA"
coupdata$oRating[coupdata$oRating == "A+"] <- "A"
coupdata$oRating[coupdata$oRating == "A-"] <- "A"
coupdata$oRating[coupdata$oRating == "BBB+"] <- "BBB"
coupdata$oRating[coupdata$oRating == "BBB-"] <- "BBB"
coupdata$oRating[coupdata$oRating == "BB+"] <- "BB"
coupdata$oRating[coupdata$oRating == "BB-"] <- "BB"
coupdata$oRating[coupdata$oRating == "B+"] <- "B"
coupdata$oRating[coupdata$oRating == "B-"] <- "B"

#keeping only issuances greater than 1 million
coupdata <- subset(coupdata, OfferingAmount >= 1)

#remove NA's
coupdata <- completeFunc(coupdata,"OfferingAmount")
hist(coupdata$OfferingAmount)
quantile(coupdata$OfferingAmount, c(.10,.30,.50,.70,.90))
#structuring the amount data
coupdata$fAmount <- ""
coupdata$fAmount[coupdata$OfferingAmount > 500] <- c("Very Large")
coupdata$fAmount[coupdata$OfferingAmount <= 500 & coupdata$OfferingAmount > 250] <- c("Large")
coupdata$fAmount[coupdata$OfferingAmount <= 250 & coupdata$OfferingAmount > 100] <- c("Medium")
coupdata$fAmount[coupdata$OfferingAmount <= 100 &  coupdata$OfferingAmount > 10] <- c("Small")
coupdata$fAmount[coupdata$OfferingAmount <= 10 &  coupdata$OfferingAmount > 0] <- c("Very Small")
coupdata$fAmount <- as.factor(coupdata$fAmount)
#structuring the term
coupdata$fterm <- ""
coupdata$fterm[coupdata$term > 19] <- c("Long-term")
coupdata$fterm[coupdata$term <= 19 & coupdata$term > 4] <- c("Medium-term")
coupdata$fterm[coupdata$term <= 4] <- c("Short-term")
coupdata$fterm <- as.factor(coupdata$fterm)

#structuring the subordination data
coupdata$level <- coupdata$SeniorityLevel
coupdata$level <- as.character(coupdata$level)
coupdata$level[coupdata$level == "Junior Subordinate"] <- "Other"
coupdata$level[coupdata$level == "Not Ranked"] <- "Other"
coupdata$level[coupdata$level == "Senior Subordinate"] <- "Other"
coupdata$level[coupdata$level == "Subordinate"] <- "Other"
coupdata$level <- as.factor(coupdata$level)

regutil <- coupdata

#creating offering amount per term
totoffamnt <- aggregate(OfferingAmount ~ odyear, regutil, FUN = sum)
aveterm <- aggregate(term ~ odyear, regutil, FUN = mean)
avecoup <- aggregate(CouponatOffer ~ odyear, regutil, FUN = mean)

#getting a data frame of main regulated industries
#regutil <- subset(coupdata, PrimaryIndustry == c("Electric Utilities") | PrimaryIndustry == c("Multi-Utilities") |
#                    PrimaryIndustry == c("Water Utilities") | PrimaryIndustry == c("Gas Utilities"))

#breaking out year to build the term variable
regutil$State <- regutil$State.RegionFromPrimaryAddress
regutil$State.RegionFromPrimaryAddress <- NULL

#adding yearly debt totals to regutil
regutil <- (merge(totoffamnt, regutil, by = 'odyear'))
regutil <- rename(regutil, c("OfferingAmount.x"="YearOffAmnt"))
regutil <- rename(regutil, c("OfferingAmount.y"="OfferingAmount"))

regutil$woamnt <- regutil$OfferingAmount/regutil$YearOffAmnt
#weighted term
regutil$woaterm <- regutil$woamnt*regutil$term
woterm <- aggregate(woaterm ~ odyear, regutil, FUN = sum)
woterm$woaterm <- round(woterm$woaterm, digits = 3)
woterm <- merge(aveterm, woterm, by = 'odyear')
woterm <- merge(totoffamnt, woterm, by = 'odyear')
write.csv(woterm,"E:/Commentaries/CouponComm/woterm.csv")
#weighted coupon
regutil$wocoup <- regutil$woamnt*regutil$CouponatOffer
wocoupn <- aggregate(wocoup ~ odyear, regutil, FUN = sum)
wocoupn$wocoup <- round(wocoupn$wocoup, digits = 2)
wocoupn <- merge(avecoup, wocoupn, by = 'odyear')
wocoupn <- merge(totoffamnt, wocoupn, by ='odyear')
write.csv(wocoupn,"E:/Commentaries/CouponComm/wocoupn.csv")
#complete bar plots for average weighted term
ggplot(woterm, aes(x = odyear, y = woaterm)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = woaterm), vjust = 1.6, 
            color = "white", position = position_dodge(0.9), size = 2.5)+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  xlab("\nCalendar Year")+
  ylab("Years")+
  ggtitle("Annual Average Weighted Duration")
#complete bar plots for average weighted coupon
ggplot(wocoupn, aes(x = odyear, y = wocoup)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = wocoup), vjust = 1.6, 
            color = "white", position = position_dodge(0.9), size = 2.5)+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  xlab("\nCalendar Year")+
  ylab("Average Weighted Coupon (%)")+
  ggtitle("Annual Average Weighted Coupon")

#adding weighted average to regutil data frame
regutil <- merge(regutil, wocoupn, by = 'odyear')
colnames(regutil)[colnames(regutil) == "OfferingAmount.y"] <- "TotalYDebt"
colnames(regutil)[colnames(regutil) == "OfferingAmount.x"] <- "OfferingAmount"
colnames(regutil)[colnames(regutil) == "wocoup.x"] <- "wocoup"
colnames(regutil)[colnames(regutil) == "wocoup.y"] <- "YearWCoup"
colnames(regutil)[colnames(regutil) == "CouponatOffer.x"] <- "CouponatOffer"
regutil$CouponatOffer.y <- NULL
regutil$YearOffAmnt <- NULL

#coloring for amount levels
#amnt_coloring <- c(large,medium,small,very large,very small)
amnt_coloring <- c("#6666FF","#FF6600","#99FF33","#0000CC","#FFFF66")
#coloring for amount levels
term_coloring <- c("#CC0000","#CC9900","#339999")
#coloring for amount rating level
rate_color <- brewer.pal(11,"Spectral")
#monthly rate color
# #3288BD , #5E4FA2 
month_color <- c("#F46D43","#FDAE61","#FEE08B","#3288BD")
#coloring for amount term
term_color <- brewer.pal(3,"Spectral")

#plot to get weighted average coupon legend
#http://stackoverflow.com/questions/13143894/how-do-i-position-two-legends-independently-in-ggplot
ywcoups <- ggplot(data = regutil)+
  geom_line(aes(x = OfferingDate, y = YearWCoup, lty = ''), 
            colour = "black", size = 1)+
  scale_linetype('Weigthed Average Coupon')+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  xlab("\nYear")+
  scale_y_continuous(limits = c(0,15))+
  ylab("Coupon at Offering (%)\n")+
  scale_x_date(date_breaks = "years", date_labels = "%Y")
scale_colour_manual(values = c("black"))
adj_ywcoups <-  ywcoups + coord_cartesian(xlim=c(as.Date("1995-01-01"),as.Date("2016-04-01")))
adj_ywcoups
wcoupleg <- g_legend(adj_ywcoups)
#ggplot of all coupon information by amount at offering
#yearly debt issues
debtiss <- ggplot(data = regutil, aes(x = OfferingDate))+
  geom_line(aes(y = (TotalYDebt/1000), fill = "Total Annual Debt"), 
            colour = "darkgoldenrod4", size = .75)+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 11))+
  scale_y_continuous(limits = c(0,65))+
  ylab("Debt Issued (USD billions)\n\n")+
  scale_x_date(date_breaks = "years")
adj_debtiss <- debtiss + coord_cartesian(xlim=c(as.Date("1995-01-01"),as.Date("2017-04-01")))
adj_debtiss
########################################################################################
#useful charts
########################################################################################
#scatter plot of coupons
aallcoups <- ggplot(data = regutil, aes(x = OfferingDate))+
  geom_point(aes(y = CouponatOffer, colour = fAmount))+
  scale_colour_manual(name  = "Offering Amount", 
                      breaks = c('Very Large','Large','Medium','Small','Very Small'),
                      labels = c('Very Large','Large','Medium','Small','Very Small'),
                      values = amnt_coloring)+
  geom_line(aes(y = YearWCoup), colour = "black", size = 1)+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  xlab("\nYear")+
  scale_y_continuous(limits = c(0,15))+
  ylab("Coupon at Offering (%)\n")+
  scale_x_date(date_breaks = "years", date_labels = "%Y")
adj_aall <- aallcoups + coord_cartesian(xlim=c(as.Date("1995-01-01"),as.Date("2016-08-01"))) 
adj_aall
acoupleg <- g_legend(adj_aall)
# setup legends grid 
wcoupleg_grid <- cowplot::plot_grid(wcoupleg, align = "h", nrow = 1)
# add second legend to grid, specifying its location 
coup_legends <- wcoupleg_grid + 
  ggplot2::annotation_custom(grob = acoupleg, xmin = 0.5, 
                             xmax = 0.5, ymin = 0.1, 
                             ymax = 0.1)
plot(coup_legends)
#combining the two graphs
grid.newpage()
grid.draw(rbind(ggplotGrob(adj_debtiss),ggplotGrob(adj_aall), size = "last"))
#debt issued by rating
#can us odyear (year) or odmyear (month year)
regutil <- with(regutil, regutil[order(odyear, oRating),])
ggplot(regutil, aes(x = odyear, 
                    y = OfferingAmount/1000, 
                    fill = oRating)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Annual Debt Issuance by Rating", 
       x = "", 
       y = "Debt Issuances (USD billions)\n")+
  scale_y_continuous(limits = c(0,65))+
  scale_fill_manual(values = rate_color,
                    name = "Rating of Issuances",
                    breaks = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","Unknown"),
                    labels = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","Unknown"))
write.csv(regutil,"E:/Commentaries/CouponComm/regutildebt.csv")
#monthly version
month_regutil <- subset(regutil, OfferingDate>="2014-01-01" & OfferingDate<="2017-02-16")
month_regutil <- with(month_regutil, month_regutil[order(odmyear, oRating),])
mondebt <- ggplot(month_regutil, aes(x = odmyear, 
                                     y = OfferingAmount/1000, 
                                     fill = oRating)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 11))+
  labs(title = "Monthly Debt Issuance by Rating", 
       x = "", 
       y = "Debt Issuances (USD billions)\n")+
  scale_y_continuous(limits = c(0,20))+
  scale_x_date(labels = date_format("%m-%Y"),date_breaks = "1 month")+
  scale_fill_manual(values = month_color,
                    name = "Rating of Issuances",
                    breaks = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","Unknown"),
                    labels = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","Unknown"))
mon_adj <- mondebt + coord_cartesian(xlim=c(as.Date("2014-02-01"),as.Date("2017-01-01"))) 
mon_adj
write.csv(month_regutil,"E:/Commentaries/CouponComm/monthlydebt.csv")
#debt issued by term
regutil <- with(regutil, regutil[order(odyear, fterm),])
ggplot(regutil, aes(x = odyear, 
                    y = OfferingAmount/1000, 
                    fill = fterm)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        text = element_text(size = 11))+
  labs(title = "Debt Issuance by Term", 
       x = "", 
       y = "Debt Issuances (USD billions)\n")+
  scale_y_continuous(limits = c(0,65))+
  scale_fill_manual(values = term_color,
                    name = "Term of Issuances",
                    breaks = c("Long-term","Medium-term","Short-term"),
                    labels = c("Long-term","Medium-term","Short-term"))
########################################################################################
