rm(list=ls())
library("data.table")
library("XML")
library("stringr")

#Scrape tutorial:
srts<-htmlParse("http://apps.saferoutesinfo.org/legislation_funding/state_apportionment.cfm")
class(srts)

srts.table<- readHTMLTable(srts,stringsAsFactors = FALSE)

money <- sapply(srts.table[[1]][,-1], FUN= function(x) 
  as.character(gsub(",", "", as.character(x), fixed = TRUE) ))
money<-as.data.frame(substring(money,2), stringsAsFactors=FALSE)

names(money)<-c("Actual.05","Actual.06","Actual.07","Actual.08",
                "Actual.09","Actual.10","Actual.11", "Actual.12", "total")
money$state<-srts.table[[1]][,1]
money<-money[,c(10,1:9)]
money

#=====================================================================

#Scrape tutorial 2:
library("httr")
library("XML")

# Define certicificate file
cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")

# Read page
page <- GET(
  "https://www.adac.de/infotestrat/", 
  path="autodatenbank/suchergebnis.aspx", 
  query="A-Klasse",
  config(cainfo = cafile)
)

# Use regex to extract the desired table
x <- text_content(page)
tab <- sub('.*(<table class="grid".*?>.*</table>).*', '\\1', x)

# Parse the table
readHTMLTable(tab)