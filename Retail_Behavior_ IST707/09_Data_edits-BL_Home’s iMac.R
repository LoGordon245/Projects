#Past Data
Retail_09_edit <- Test_Retail2
Retail_09_edit$InvoiceQuarter <- month(as.Date(strptime(Test_Retail2$InvoiceDate,"%m/%d/%Y  %H:%M"))) 

Retail_09_edit$InvoiceQuarter <- ifelse(Retail_09_edit$InvoiceQuarter <=3 , "Q1", 
                                        ifelse(Retail_09_edit$InvoiceQuarter  > 3 & Retail_09_edit$InvoiceQuarter <= 6, "Q2",
                                               ifelse(Retail_09_edit$InvoiceQuarter  > 6 & Retail_09_edit$InvoiceQuarter  <= 9, "Q3", 
                                                      ifelse(Retail_09_edit$InvoiceQuarter > 9, "Q4", "null"))))

RetailEdit1 <- Retail_09_edit
RetailEdit1$CustomerID <- as.character(RetailEdit1$CustomerID)
RetailEdit1$Quantity <- as.numeric(RetailEdit1$Quantity)
RetailEdit1$Description <- as.character(RetailEdit1$Description)

#Frequency of buyers
CustomerFrequency09 <- aggregate(RetailEdit1$CustomerID, list(RetailEdit1$CustomerID), FUN= length)
colnames(CustomerFrequency09) <- c("CustomerID","Frequency")

#Range of Frequency 
quantile(CustomerFrequency09$Frequency, probs = c(.25, .5, .75))

CustomerFrequency09$CustomerBehavior <- ifelse(CustomerFrequency09$Frequency <= 18, "Not Likely To Repeat", 
                                               ifelse(CustomerFrequency09$Frequency > 18 & CustomerFrequency$Frequency <= 44, "May Repeat",
                                                      ifelse(CustomerFrequency09$Frequency > 44 & CustomerFrequency09$Frequency <= 103, "Likely to Repeat", 
                                                             ifelse(CustomerFrequency09$Frequency>103, "Valued Customer", "null"))))

#merge
Retail_09_edit <- merge(x = Retail_09_edit, y= CustomerFrequency09[, c("CustomerID", "CustomerBehavior")], by= "CustomerID", all.x=TRUE)
Retail_09_edit$CustomerBehavior <- as.factor(Retail_09_edit$CustomerBehavior)

write.csv(Retail,"/Users/bl_home/Documents/OneDrive/Syracuse/IST-707 Machine Learning/Final_project/Retail_behavior09.csv", row.names= FALSE)




















#Completed in r weka 
#model With Customer Behavior #remake training and test set

TrainDate <- Retail[train_list,]

testData <- Retail[-train_list,]

CustomerBehaviorModel <- rpart(CustomerBehavior ~ StockCode + Description + Quantity + UnitPrice, data = TrainDate, method = "class")

e <- evaluate_Weka_classifier(CustomerBehaviorModel, numFolds = 3, seed = 1, class = TRUE)

InfoGainAttributeEval(CustomerBehavior ~StockCode + Description + Quantity + UnitPrice, data = TrainDate)

plot(CustomerBehaviorModel)