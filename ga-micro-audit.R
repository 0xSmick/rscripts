#POSSIBLE Google Analytics micro-audit

#GA setup
#RGoogleAnalytics - download RGoogleAnalytics_1.4.zip from 
#https://code.google.com/p/r-google-analytics/downloads
#click on install packages, navigate to download and install package

#Load packages
library(bitops)
library(plyr)
library(RCurl)
library(rjson)
library(reshape)
library(RGoogleAnalytics)
library(sqldf) 
library(XLConnect)
library(wordcloud)
library(lattice)
library(ggplot2)

#set working directory
wd <- "P:/Liz_Earle/Marketing Sciences"
setwd(wd)

#set filename
wbFilename <- "Liz Earle UK Analytics Micro-Audit (April 2014).xlsx"

#Include file
includeFile <- paste(wd, "/R Progs/includeGA.r", sep="")
source(file=includeFile)

#Authorize Google Analytics account and paste the accesstoken
#You need to be logged into the correct GA account in order to access the token
#Follow instructions on browser
query <- QueryBuilder()
access_token <- query$authorize()  #Paste accesstoken in R console

#Initialize configuration object
conf <- Configuration()

#Select GA reports

  #Retrieve list of accounts
  ga.account <- conf$GetAccounts() 
  ga.account
  
  #Retrieve list of web properties
  #Update Account ID with previous list
  ga.webProperty <- conf$GetWebProperty(ga.account$id[8])
  ga.webProperty
  
  #Retrieve list of web profiles
  #Update Web Profile ID with previous list
  ga.webProfile <- conf$GetWebProfile(ga.account$id[8],ga.webProperty$id[1])
  ga.webProfile

  #Retrieve list of goals
  #Update webProfile with previous table
  ga.goals <- conf$GetGoals(ga.account$id[8],ga.webProperty$id[1],ga.webProfile$id[1])
  ga.goals
  
  #Run fix and change 
  #Change 1st line -> unexpected.escape = "keep"
  fix(fromJSON)
  
  #Retrieve list of segments
  ga.segments <- conf$GetSegments()
  ga.segments

#Create GA API object
ga <- RGoogleAnalytics()

#Create Google Analytics micro audit

  #Set micro audit parameters
  profile <- ga.webProfile$id[1]
  startdate <- "2013-01-01"
  enddate <- "2014-03-31"
  segment <- ga.segments$segmentId[1]
  maxresults <- 20000

  #Session data

      #Report parameters
      dimension <- "ga:year,ga:month,ga:deviceCategory,ga:medium,ga:source"
      metric <- "ga:visits,ga:visitors,ga:newvisits,ga:pageviews,ga:TimeOnSite,ga:bounces,ga:transactions,ga:transactionRevenue"
      
      #Build query string
      query$Init(start.date = startdate,
                 end.date = enddate,
                 dimensions = dimension,
                 metrics = metric,
                 #sort = sort,
                 #filters=filter,
                 segment=segment,
                 max.results = maxresults,
                 table.id = paste("ga:",profile,sep="",collapse=","),
                 access_token=access_token)

      #Save to dataframe
      sessionRaw <- ga$GetReportData(query)

#Prep dataset
sessionPrep <- sessionRaw

#Format date
sessionPrep$newDate <- paste(sessionPrep$year,sessionPrep$month,sep="-")

#Search for "/" in medium, attribute to "Email"
for (i in 1:nrow(sessionPrep))
  if(length(grep("/", sessionPrep$medium[i]))>0)
    sessionPrep$medium[i] <- "Email"

#Search for "newsbites" in source, attribute to "Email"
for (i in 1:nrow(sessionPrep))
  if(length(grep("newsbites", sessionPrep$source[i]))>0)
    sessionPrep$medium[i] <- "Email"

#Define vectors
mDisplay <- c("Rocketfuel","RT","display", "adv", "advert", "advertising", "banner", "mpu", "leaderboard", "display advertising", "dis", "advertising")
mEmail <- c("email", "newsletter")
mDirect <- c("(none)")
mPPC <- c("cpc", "ppc")
mSEO <- c("organic")
mReferral <- c("referral")

#Apply to dataset
sessionPrep <- mediumRules(sessionPrep, mDisplay, mEmail, mDirect, mPPC, mSEO, mReferral)

#Aggregate dataset
sessionPrep <- ddply(sessionPrep,
                     c("newDate","deviceCategory","medium"),
                     summarise,
                     visits=sum(visits),
                     visitors=sum(visitors),
                     newvisits=sum(newvisits),
                     pageviews=sum(pageviews),
                     bounces=sum(bounces),
                     TimeOnSite=sum(TimeOnSite),
                     transactions=sum(transactions),
                     transactionRevenue=sum(transactionRevenue))

head(sessionPrep)

#Site overview
siteOverview <- sessionPrep
siteOverview$z <- 0
siteOverview <-  ddply(siteOverview,
                       c("z"),
                       summarise,
                       visits=sum(visits),
                       visitors=sum(visitors),
                       newvisits=sum(newvisits),
                       pageviews=sum(pageviews),
                       TimeOnSite=sum(TimeOnSite),
                       bounces=sum(bounces),
                       transactions=sum(transactions),
                       transactionRevenue=sum(transactionRevenue))

#Calculated metrics
siteOverview$transactionRate <- siteOverview$transactions/siteOverview$visits
siteOverview$avgOrderValue <- siteOverview$transactionRevenue/siteOverview$transactions
siteOverview$newvisits <- siteOverview$newvisits/siteOverview$visits
siteOverview$bounceRate <- siteOverview$bounces/siteOverview$visits
siteOverview$pagesPerVisit <- siteOverview$pageviews/siteOverview$visits
siteOverview$visitDuration <- siteOverview$TimeOnSite/siteOverview$visits

#Clean dataset
siteOverview <-  siteOverview[ , -which(names(siteOverview) %in% c("TimeOnSite","bounces"))]

head(siteOverview) 

#Output to worksheet
wb <- loadWorkbook(wbFilename)
clearSheet(wb, sheet = "siteOverview")
createSheet(wb, name = "siteOverview")
writeWorksheet(wb, siteOverview, sheet = "siteOverview")
saveWorkbook(wb)    

#Site overview graph
graphOverview <- sessionPrep
  
graphOverview <-  ddply(graphOverview,
                       c("newDate"),
                       summarise,
                       visits=sum(visits),
                       bounces=sum(bounces))

#Calculated metrics
graphOverview$bounces <- graphOverview$bounces/graphOverview$visits*100

head(graphOverview)

#Output to worksheet
wb <- loadWorkbook(wbFilename)
clearSheet(wb, sheet = "graphOverview")
createSheet(wb, name = "graphOverview")
writeWorksheet(wb, graphOverview, sheet = "graphOverview")
saveWorkbook(wb)   

#Traffic acquisition
trafficChannels <- ddply(sessionPrep,
                         c("medium"),
                         summarise,
                         transactionRevenue=sum(transactionRevenue),
                         visits=sum(visits),
                         transactions=sum(transactions))

trafficChannels <- trafficChannels[order(trafficChannels$visits,decreasing=TRUE),]

trafficChannels$cumVisits <- cumsum(trafficChannels$visits)
trafficChannels$cumPctVisits <- trafficChannels$cumVisits/sum(trafficChannels$visits)
trafficChannels <- trafficChannels[which(trafficChannels$cumPctVisits <= 0.9899),]

trafficChannels <- trafficChannels[order(trafficChannels$visits,decreasing=FALSE),]

trafficChannels <-  trafficChannels[ , -which(names(trafficChannels) %in% c("cumVisits","cumPctVisits"))]

trafficChannels$transactionRate <- trafficChannels$transactions/trafficChannels$visits
  
head(trafficChannels)

#Output to worksheet
wb <- loadWorkbook(wbFilename)
clearSheet(wb, sheet = "trafficChannels")
createSheet(wb, name = "trafficChannels")
writeWorksheet(wb, trafficChannels, sheet = "trafficChannels")
saveWorkbook(wb)  

#Devices
devices <- ddply(sessionPrep,
                 c("newDate","deviceCategory"),
                 summarise,
                 visits=sum(visits))

devices <- cast(devices, newDate ~ deviceCategory, sum, value="visits")

head(devices)

#Output to worksheet
wb <- loadWorkbook(wbFilename)
clearSheet(wb, sheet = "devices")
createSheet(wb, name = "devices")
writeWorksheet(wb, devices, sheet = "devices")
saveWorkbook(wb) 

#Ecommerce graph
graphEcommerce <- sessionPrep
graphEcommerce$date <- paste(graphEcommerce$year,graphEcommerce$month,sep="-")

graphEcommerce <-  ddply(graphEcommerce,
                        c("newDate"),
                        summarise,
                        visits=sum(visits),
                        transactions=sum(transactions))

#Calculated metrics
graphEcommerce$transactionRate <- graphEcommerce$transactions/graphEcommerce$visits*100

graphEcommerce <-  graphEcommerce[ , -which(names(graphEcommerce) %in% c("transactions"))]

head(graphEcommerce)

#Output to worksheet
wb <- loadWorkbook(wbFilename)
clearSheet(wb, sheet = "graphEcommerce")
createSheet(wb, name = "graphEcommerce")
writeWorksheet(wb, graphEcommerce, sheet = "graphEcommerce")
saveWorkbook(wb)

#PPC Keyword Cloud
  
      #Report parameters
      dimension <- "ga:keyword"
      metric <- "ga:visits"
      filter <-  "ga:medium==cpc"
      sort <- "-ga:visits"
      
      #Build query string
      query$Init(start.date = startdate,
                 end.date = enddate,
                 dimensions = dimension,
                 metrics = metric,
                 sort = sort,
                 filters=filter,
                 segment=segment,
                 #max.results = maxresults,
                 table.id = paste("ga:",profile,sep="",collapse=","),
                 access_token=access_token)

      #Save to dataframe
     ppcKeywords <- ga$GetReportData(query)

      #Check dataframe
      head(ppcKeywords)
      
      #Create variable lists
      ppckeywords <-ppcKeywords[,1]
      ppcvisits <-ppcKeywords[,2]

      #Output wordcloud
      wb <- loadWorkbook(wbFilename) 
      
          #Save as image
          png(filename = "ppc_cloud.png", res=100)
          ppccloud <- wordcloud(ppckeywords, ppcvisits, scale=c(3,1), max.words=30, random.order=FALSE, rot.per=0, use.r.layout=FALSE)
          dev.off()

          #Save to front sheet
          sheet <-"Audit"
          addImage(wb, filename = "ppc_cloud.png", name = "ppc_wordcloud", originalSize = FALSE)
         
      saveWorkbook(wb)

  #SEO Keyword Cloud
  
      #Report parameters
      dimension <- "ga:keyword"
      metric <- "ga:visits"
      filter <- "ga:medium==organic"
      sort <- "-ga:visits"
      
      #Build query string
      query$Init(start.date = startdate,
                 end.date = enddate,
                 dimensions = dimension,
                 metrics = metric,
                 sort = sort,
                 filters=filter,
                 segment=segment,
                 #max.results = maxresults,
                 table.id = paste("ga:",profile,sep="",collapse=","),
                 access_token=access_token)
      
      #Save to dataframe
      seoKeywords <- ga$GetReportData(query)

        #Remove organic ranking
        #Liz Earle specific

        seoKeywordsClean <- seoKeywords
        seoKeywordsClean[,1] <- gsub(" \\(.+\\)", "", seoKeywordsClean$keyword) 
        seoKeywordsClean <- ddply(seoKeywordsClean, c("keyword"), summarise, visits=sum(visits))
   
        #Check dataframe
        head(seoKeywordsClean)

        #Save to dataframe
        seoKeywords <- seoKeywordsClean       

        #Overwrite 'np - xxx'
        #Liz Earle specific

          #Save database into temp
          temp <- ddply(seoKeywords, c("keyword"), summarise, visits=sum(visits))
          temp

          #Search for "np -" in keyword, rewrite as (not provided)
          for (i in 1:nrow(seoKeywords))
            if(length(grep("np - ", seoKeywords$keyword[i]))>0)
              seoKeywords$keyword[i] <- "(not provided)"

          #Define vectors
          mNotProvided <- c("(not provided)")

          #Check keywords left
          #If keywords left amend includeGA.r
          temp <- temp[- which(temp$oldKeywords %in% c(mNotProvided)), ]
          temp

          #Apply to seoKeywords
          seoKeywords <- keywordRules(seoKeywords, mNotProvided)

          #Aggregate data
          seoKeywords <- ddply(seoKeywords, c("keyword"), summarise, visits=sum(visits))
          
          #Get the order
          seoKeywordsSort <- order(seoKeywords$visits)
          seoKeywordsSort 
          seoKeywords$visits[seoKeywordsSort]
          
          #Sort in decreasing order
          order(seoKeywords$visits, decreasing=TRUE)
          seoKeywords <- seoKeywords[order(seoKeywords$visits, decreasing=TRUE),]

      #Remove (not provided)
      seoKeywords <- seoKeywords[-which(seoKeywords$keyword=="(not provided)"), ]
    
      #Review data
      head(seoKeywords)

      #Create variable lists
      seokeywords <- seoKeywords[,1]
      seovisits <- seoKeywords[,2]
      
      #Output wordcloud
      wb <- loadWorkbook(wbFilename)

            #Save as image
            png(filename = "seo_cloud.png", res=100)
            seocloud <- wordcloud(seokeywords, seovisits, scale=c(3,1), max.words=30, random.order=FALSE, rot.per=0, use.r.layout=FALSE)
            dev.off()
            
            #Save to front sheet
            sheet <-"Audit"
            addImage(wb, filename = "seo_cloud.png", name = "seo_wordcloud", originalSize = FALSE)
            
      saveWorkbook(wb)  

#Top Content

#Report parameters
dimension <- "ga:pagePath"
metric <- "ga:pageviews,ga:uniquePageviews,ga:avgTimeOnPage,ga:exits"
sort <- "-ga:pageviews"
maxresults <- 10

#Build query string
query$Init(start.date = startdate,
           end.date = enddate,
           dimensions = dimension,
           metrics = metric,
           sort = sort,
           #filters=filter,
           segment=segment,
           max.results = maxresults,
           table.id = paste("ga:",profile,sep="",collapse=","),
           access_token=access_token)

#Save to dataframe
topContent <- ga$GetReportData(query)

#Check dataframe
head(topContent)

#Output to worksheet
wb <- loadWorkbook(wbFilename)
clearSheet(wb, sheet = "topContent")
createSheet(wb, name = "topContent")
writeWorksheet(wb, topContent, sheet = "topContent")
saveWorkbook(wb)  


#Ad hoc

#Traffic acquisition
monthTraffic <- sessionPrep[(sessionPrep$deviceCategory=="mobile"),]

monthTraffic <- ddply(monthTraffic,
                         c("newDate","medium"),
                         summarise,
                         visits=sum(visits))

monthTraffic <- cast(monthTraffic, newDate ~ medium, sum, value="visits")

head(monthTraffic)


