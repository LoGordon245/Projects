"TEAM CODE for Word Cloud Text, Association Rule Mining, and Decision Tree
by - Greg Funaro, Shawn Anderson, and Lorenzo Gordon "
"2022-06-17 IST 707 Introduction to Machine Learning"
###########################################################
"Greg Funaro - Word Cloud Text Mining"
#####################################
library(wordcloud)
## Warning: package 'wordcloud' was built under R version 4.1.3
## Loading required package: RColorBrewer
library(RColorBrewer)
library(tm)
## Warning: package 'tm' was built under R version 4.1.2
## Loading required package: NLP
library(SnowballC)
options(warn=-1)
Retail <- read.csv("\\Users\\gregf\\Desktop\\Online Retail.csv")

"Sales Word Cloud"

Salestext <- readLines(file.choose())
Salesdocs <- Corpus(VectorSource(Salestext))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
Salesdocs <- tm_map(Salesdocs, toSpace, "/")
Salesdocs <- tm_map(Salesdocs, toSpace, "@")
Salesdocs <- tm_map(Salesdocs, toSpace, "\\|")

dtm <- TermDocumentMatrix(Salesdocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)

##                word  freq
## set             set 62524
## bag             bag 51219
## red             red 42604
## heart         heart 38350
## retrospot retrospot 34419
## vintage     vintage 33275
## design       design 29572
## pink           pink 29451
## christmas christmas 24844
## box             box 23898

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

"Returns Word Cloud"

Returntext <- readLines(file.choose())
Returndocs <- Corpus(VectorSource(Returntext))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
Returndocs <- tm_map(Returndocs, toSpace, "/")
Returndocs <- tm_map(Returndocs, toSpace, "@")
Returndocs <- tm_map(Returndocs, toSpace, "\\|")

rtm <- TermDocumentMatrix(Returndocs)
a <- as.matrix(rtm)
b <- sort(rowSums(a),decreasing=TRUE)
c <- data.frame(word = names(b),freq=b)

head(c, 10)

##                word freq
## set             set 1051
## red             red  798
## retrospot retrospot  707
## bag             bag  692
## box             box  549
## cake           cake  513
## design       design  494
## vintage     vintage  473
## heart         heart  463
## glass         glass  459

set.seed(4321)
wordcloud(words = c$word, freq = c$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

"Association"

#Sales
findAssocs(dtm, terms = "set", corlimit = 0.3)
## $set
## numeric(0)
findAssocs(dtm, terms = "bag", corlimit = 0.3)
## $bag
##     jumbo     lunch charlotte
##      0.55      0.41      0.31
findAssocs(dtm, terms = "red", corlimit = 0.3)
## $red
## retrospot
##      0.48
#Returns
findAssocs(rtm, terms = "set", corlimit = 0.3)
## $set
## tins
## 0.42
findAssocs(rtm, terms = "red", corlimit = 0.3)
## $red
## retrospot
##      0.54
findAssocs(rtm, terms = "retrospot", corlimit = 0.3)
## $retrospot
##  red
## 0.54
"Bar Plots"

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words - Sales",
        ylab = "Word frequencies")
barplot(c[1:10,]$freq, las = 2, names.arg = c[1:10,]$word,
        col ="lightblue", main ="Most frequent words - Returns",
        ylab = "Word frequencies")

########################################################################
"Shawn Anderson's Association Rule Mining Tasks"
########################################################################
# install.packages("plyr")
# install.packages("tidyverse")
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("readxml")
# install.packages("knitr")
# install.packages("lubridate")
# install.packages("RColorBrewer")

library(plyr) # must load plyr before dplyr (found in tidyverse) if need both fx's
library(tidyverse) # includes ggplot2 and dplyr
library(arules) # arules::write is masking base::write
library(arulesViz)
library(readxl)
library(knitr)
library(lubridate)
library(RColorBrewer)

setwd("D:/OneDrive/Syracuse/_Courses/_IST707 Applied Machine Learning/Final Project Ideas/Online Retail")

# code generated from RStudio import function
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"
destfile <- "Online_20Retail.xlsx"
curl::curl_download(url, destfile)
Online_20Retail <- read_excel(destfile)

# read excel into R dataframe
retail <- read_excel("Online_20Retail.xlsx")

# complete.cases pulls only rows with no missing values.  Putting this argument
# ... in the row section of retail tells it to apply to rows only
retail <- retail[complete.cases(retail), ]

# dplyr mutate() allows me to edit values in df - like mat desc a factor.
retail %>% mutate(InvoiceNo = as.factor(InvoiceNo))
retail %>% mutate(StockCode = as.factor(StockCode))
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(CustomerID = as.factor(CustomerID))

# converts character data to date and make InvoiceDate as date in new variable
retail$Date <- as.Date(retail$InvoiceDate)
# lubridate to extract time from InvoiceDate and store in another variable
#TransTime <- format(retail$InvoiceDate)
TransTime <- format(retail$InvoiceDate,"%H:%M:%S")

" dropping this and adding invoice as.factor above"
# convert and edit InvoiceNo into numeric
#InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))  "NAs introduced by coercion"

# bind new timecolumn into df
cbind(retail,TransTime)
#get a glimpse of your data
glimpse(retail)
# print(retail, n = 5) # here access only 5 rows....

# ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
# I hired a tutor through FIVERR to help me get started on this next line of code, then completed
# ... the line to meet my needs.I had spent nearly 20 hours trying ways to most efficiently
# ... restructure that data to meet the apriori() requirements.
transactionData <- ddply(retail,c("InvoiceNo","InvoiceDate","CustomerID","Country")
                         , function(df1)paste(c(df1$Description,df1$StockCode), collapse = ","))
# paste()) concatenates vectors to character and separated results using
# ... collapse=[any optional charcater string ] - such as ',' in this case
# show dfdata
head(transactionData)

"WWWWWWWW    REMOVED THIS NEXT SECTION   WWWWWWWW"
"# ??????????remove colnames in preparation for frequent item set lists??????????"
# #set column InvoiceNo of dataframe transactionData
# transactionData$InvoiceNo <- "Invoice"
# #set column Date of dataframe transactionData
# transactionData$Date <- "DateTime"
# ##Rename column to items
# # colnames(transactionData) <- c("")
# #Show Dataframe transactionData
# head(transactionData)
"WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"
write.csv(transactionData,"data_transactions.csv", quote = FALSE, row.names = FALSE)
# transactionData: data to be written
#"data_transactions.csv": location of file with file name to be written to
# If quote = TRUE it will surround character or factor column with double quotes.
# If FALSE nothing will be quoted.

"---ignorable warning follows - just means that last row needs to be empty-----"
tr <- read.transactions('data_transactions.csv', format = 'basket', sep=',', encoding="UTF-8")
# I got an error: Warning message in scan(text = l, what = "character", sep = sep, quote = quote, :
# “... EOF within quoted string”
# Try this solution: seems like final line is not ending in a blank new line.
# To potentially fix it, open file and look at last line...if it isn't empty,
# ....add a new emply line, save and close file.

summary(tr)

itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

"Mine Rules via apriori"
# Min Support as 0.001, confidence as 0.8.
arules <- apriori(tr, parameter = list(supp=0.001, conf=0.6,minlen=6, maxlen=8))
arules = apriori(tr, parameter = list(sup=0.01, minlen=6, maxlen=8, conf=0.1, target="rules"))

" Visualize Rules"
# filter rules with confidence greater than 0.4 or 40%
subRules<-arules[quality(arules)$confidence>0.4]
#Plot SubRules
plot(subRules, control = list(verbose = F))  # engine = "interactive" for more cool features!
"Now that the heavily lifting is done, the next step is to use the plots
... - especially 3-dim plot - to find interesting, high lift and actionable
... rules for business owner(s)."
inspect(arules)
###########################################################
"Lorenzo Gordon - Decision Tree"
#####################################
library(RWeka)
library(rpart)
library(caret)

Orginal_retail <-read.csv('/Users/bl_home/Downloads/Online Retail Dataset.csv', stringsAsFactors = TRUE)
Retail <- Orginal_retail

#Previous year data
Test_Retail2 <- read.csv('/Users/bl_home/Documents/OneDrive/Syracuse/IST-707 Machine Learning/Final_project/online_retail_09.csv', stringsAsFactors = TRUE)
Retail$Quantity <- as.factor(Retail$Quantity)

colnames(Test_Retail2)[1] <- "InvoiceNo"
colnames(Test_Retail2)[6] <- "UnitPrice"
colnames(Test_Retail2)[7] <- "CustomerID"


#remove duplicates
View(Retail[duplicated(Retail),])
Retail <- Retail[!duplicated(Retail),]


#Split Retail Database
train_list <- createDataPartition(y=Retail$Country, p=0.66, list= FALSE)
TrainDate <- Retail[train_list,]
testData <- Retail[-train_list,]

#Country Model
m <- J48(Country ~ ., data = TrainDate, control=Weka_control(U=FALSE, M=1, C=0.2, S=TRUE, O= TRUE))


#Testing model on Test Data
pred=predict(m, newdata = testData)
confusionMatrix(pred,testData$Country)
#accuracy 99% 


InfoGainAttributeEval(Country ~., data = TrainDate)
#InvoiceNo   StockCode Description    Quantity InvoiceDate   UnitPrice  CustomerID 
#0.74106813  0.14598618  0.14758778  0.06434006  0.72451073  0.05839267  0.71005607 


#using 09 Data
pred2<-predict(m, newdata = Test_Retail2)
confusionMatrix(pred2,Test_Retail2$Country)
#81accuracy on country guest. 


#Engineered Decision tree: Removing invoice, CustomerID, Invoice# as these can only directly realted back to a country
FeaturSelection <- c("StockCode","Description","Quantity","UnitPrice", "Country")
TrainDataFeature <- TrainDate[FeaturSelection]
testDataFeature <- testData[FeaturSelection]

m2 <- J48(Country ~., data = TrainDataFeature, control=Weka_control(U=FALSE, M=1, C=0.2, S=TRUE, O= TRUE))
#checked in R weka using 3 cross validation       





#Creating a new column to check frequency
#Group data
RetailEdit <- Retail
RetailEdit$CustomerID <- as.character(RetailEdit$CustomerID)
RetailEdit$Quantity <- as.numeric(RetailEdit$Quantity)
RetailEdit$Description <- as.character(RetailEdit$Description)

#find Frequency of buyers
CustomerFrequency <- aggregate(RetailEdit$CustomerID, list(Retail$CustomerID), FUN= length)
colnames(CustomerFrequency) <- c("CustomerID","Frequency")

#Find Range of Frequency 
quantile(CustomerFrequency$Frequency, probs = c(.25, .5, .75))

CustomerFrequency$CustomerBehavior <- ifelse(CustomerFrequency$Frequency <= 17, "Not Likely To Repeat", 
                                             ifelse(CustomerFrequency$Frequency > 17 & CustomerFrequency$Frequency <= 42, "May Repeat",
                                                    ifelse(CustomerFrequency$Frequency > 42 & CustomerFrequency$Frequency <= 101, "Likely to Repeat", 
                                                           ifelse(CustomerFrequency$Frequency>101, "Valued Customer", "null"))))


#join customer behavior to retail table
Retail <- merge(x = Retail, y= CustomerFrequency[, c("CustomerID", "CustomerBehavior")], by= "CustomerID", all.x=TRUE)
Retail$CustomerBehavior <- as.factor(Retail$CustomerBehavior)

#create invoice quarter tab by converting invoice date to posixit
#Pull month from invoice date
Retail$InvoiceQuarter <- month(as.Date(strptime(Retail$InvoiceDate,"%m/%d/%Y  %H:%M")))

#Create Quarter bins based on month 
Retail$InvoiceQuarter <- ifelse(Retail$InvoiceQuarter <=3 , "Q1", 
                                ifelse(Retail$InvoiceQuarter  > 3 & Retail$InvoiceQuarter <= 6, "Q2",
                                       ifelse(Retail$InvoiceQuarter  > 6 & Retail$InvoiceQuarter  <= 9, "Q3", 
                                              ifelse(Retail$InvoiceQuarter > 9, "Q4", "null"))))



#remove NA 
Retail <- Retail[Retail$CustomerID != "na",]



#export retail to complete in rweka
write.csv(Retail,"/Users/bl_home/Documents/OneDrive/Syracuse/IST-707 Machine Learning/Final_project/Retail_behavior.csv", row.names= FALSE)
