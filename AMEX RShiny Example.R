#####Reading in data
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(RCurl)
library(XML)
library(xts)
library(plyr)
library(reshape2)
library(lubridate)
library(scales)
library(grid)
library(gridExtra)
library(TTR)
library(cowplot)
library(DT)

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

#weighted coupon
regutil$wocoup <- regutil$woamnt*regutil$CouponatOffer
wocoupn <- aggregate(wocoup ~ odyear, regutil, FUN = sum)
wocoupn$wocoup <- round(wocoupn$wocoup, digits = 2)
wocoupn <- merge(avecoup, wocoupn, by = 'odyear')
wocoupn <- merge(totoffamnt, wocoupn, by ='odyear')

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

regutil2 <- regutil[, c("FI.Utilities.Corp","Issuer","OfferingDate","MaturityDate","CouponatOffer",
                        "YearWCoup","OfferingAmount","fAmount","Rating","Country")]
names(regutil2)[names(regutil2) == 'FI.Utilities.Corp'] <- 'CIQ FI ID'

ui <- fluidPage(
  verticalLayout(
      fluidRow(
      column(width = 4,
             plotOutput("plot1", height = 800, width = 1500,
                        # Equivalent to: click = clickOpts(id = "plot_click")
                        click = "plot1_click",
                        brush = brushOpts(id = "plot1_brush")
                        )
             )
      )
  ),
  fluidRow(
    column(width = 12, 
           h4("Single Selection Issuance"),
           dataTableOutput("click_info")
    ),
    column(width = 12, 
           h4("Grouped Issuances"),
           dataTableOutput("brush_info")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    #plot to get weighted average coupon legend
    #http://stackoverflow.com/questions/13143894/how-do-i-position-two-legends-independently-in-ggplot
    ywcoups <- ggplot(data = regutil2)+
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
    wcoupleg <- g_legend(adj_ywcoups)
    #scatter plot of coupons
    aallcoups <- ggplot(data = regutil2, aes(x = OfferingDate))+
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
  })
  
  output$click_info <- renderDataTable({
    nearPoints(regutil2, input$plot1_click, addDist = TRUE)
  })
  output$brush_info <- renderDataTable({
    brushedPoints(regutil2, input$plot1_brush)
  })
}

shinyApp(ui, server)