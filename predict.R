################### Question Value prediction #######################
df <- read.csv(file.choose())
library(funModeling)
library(tidyverse)
status(df)

# variable q_zeros      p_zeros q_na p_na q_inf p_inf      type      unique
# 1 Show.Number       0 0.0000000000    0    0     0     0   integer   3640
# 2    Air.Date       0 0.0000000000    0    0     0     0 character   3640
# 3       Round       0 0.0000000000    0    0     0     0 character      4
# 4    Category       0 0.0000000000    0    0     0     0 character  27995
# 5       Value       0 0.0000000000    0    0     0     0 character    150
# 6    Question       0 0.0000000000    0    0     0     0 character 216124
# 7      Answer      24 0.0001106348    0    0     0     0 character  88269

head(df)
colnames(df1)
# "Show.Number" "Air.Date"    "Round"       "Category"    "Value"       "Question"    "Answer" 
###### converting data in proper formate
library(lubridate)
df1 <- df
df1$Air.Date <- as.Date(df1$Air.Date)
head(df1$Air.Date)
class(df1$Air.Date)
head(df1$Round)
head(df1[,-6])

# head(df1[,-6])
# Show.Number   Air.Date     Round                        Category Value     Answer
# 1        4680 2004-12-31 Jeopardy!                         HISTORY  $200 Copernicus
# 2        4680 2004-12-31 Jeopardy! ESPN's TOP 10 ALL-TIME ATHLETES  $200 Jim Thorpe
# 3        4680 2004-12-31 Jeopardy!     EVERYBODY TALKS ABOUT IT...  $200    Arizona
# 4        4680 2004-12-31 Jeopardy!                THE COMPANY LINE  $200 McDonald's
# 5        4680 2004-12-31 Jeopardy!             EPITAPHS & TRIBUTES  $200 John Adams
# 6        4680 2004-12-31 Jeopardy!                  3-LETTER WORDS  $200    the ant

### removing show.Number as It makes no contribution in model
df1 <- df1[,-1]
(unique(df1$Round))

x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-= /' /' " #or whatever
df1$Round <- str_replace_all(df1$Round, "[[:punct:]]", "")
df1$Round <- trimws(df1$Round, which = 'both')
head(df1$Round)
# > head(df1$Round)
# [1] "Jeopardy" "Jeopardy" "Jeopardy" "Jeopardy" "Jeopardy" "Jeopardy"

## lest do some EDA part 
head(df1[,-5])
# > head(df1$Category)
# [1] "HISTORY"                         "ESPN's TOP 10 ALL-TIME ATHLETES" "EVERYBODY TALKS ABOUT IT..."    
# [4] "THE COMPANY LINE"                "EPITAPHS & TRIBUTES"             "3-LETTER WORDS
df1$Category <- str_replace_all(df1$Category, "[[:punct:]]", "")
# > head(df1$Category)
# [1] "HISTORY"                       "ESPNs TOP 10 ALLTIME ATHLETES" "EVERYBODY TALKS ABOUT IT"     
# [4] "THE COMPANY LINE"              "EPITAPHS  TRIBUTES"            "3LETTER WORDS"        

df1$Value <- gsub("[^0-9A-Za-z///' ]","" , df$Value ,ignore.case = TRUE)
df1$Question <- str_replace_all(df1$Question, "[[:punct:]]", "")
df1$Answer <- str_replace_all(df1$Answer, "[[:punct:]]", "")
head(df1$Value)
# > head(df1$Value)
# [1] "'200" "'200" "'200" "'200" "'200" "'200"

class(df$Value)
df1$Value <- as.integer(df1$Value)
dim(df1)
df1 <- na.omit(df1)  ######### No na values found in data frame 
df2 <- df1
df2 <- filter(df2,df2$Value != 'None')  ### found 3634 value to eliminate
dim(df2)
head(rownames(df2))

valueFreq <- freq(df2$Value)
valueFreq
valueFreq1 <- valueFreq[order(valueFreq$frequency, decreasing = T),]
valueFreq1
############## Frequency Graph ##########
library(ggplot2)
library(dplyr)
library(ggpubr)
valuedata <- valueFreq1[1:22,1:2]
valuedata
ggplot(valuedata, aes(x = reorder(valuedata$var, -valuedata$frequency), y = valuedata$frequency)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = valuedata$frequency), vjust = -0.3) + 
  theme_pubclean() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

status(df3)
df3 <- df2[,-5]
df3$Air.Date <- as.numeric(as.factor(df3$Air.Date))
df3$Answer <- gsub("[^0-9A-Za-z///' ]","" , df3$Answer ,ignore.case = TRUE)
df3$Round <- as.numeric(as.factor(df3$Round))
df3$Category <- as.numeric(as.factor(df3$Category))
df3$Value <- as.numeric(as.factor(df3$Value))
df3$Answer <- as.numeric(as.factor(df3$Answer))
library(corrplot)
cormatrix <- cor(df3)
corrplot(cormatrix,method = 'number')
library(corrgram)
corrplot(cormatrix)
head(df3)
(freq(df3$Answer))

########### lets start with value and questions ############
colnames(df2)
df4 <- df2[,c(4,5)]
head(df4)
stop <- scan(file.choose(),what="character",comment.char = ";")
combine_corpus <- Corpus(VectorSource(df4$Question))
combine_corpus <- tm_map(combine_corpus,tolower)
combine_corpus <- tm_map(combine_corpus,removePunctuation)
combine_corpus <- tm_map(combine_corpus,removeNumbers)
combine_corpus <- tm_map(combine_corpus,removeWords,c(stopwords("en"),stop))
combine_corpus <- tm_map(combine_corpus,stripWhitespace)
combine_corpus <- tm_map(combine_corpus,stemDocument, language = "english")
combine_corpus <- tm_map(combine_corpus,PlainTextDocument)
inspect(combine_corpus)

############## creating document term matrix ####################
docterm_matrix <- DocumentTermMatrix(combine_corpus)
str(docterm_matrix)
docterm_matrix
checkTermsTemp <- as.data.frame(docterm_matrix$dimnames)
dim(checkTermsTemp)
library(tidytext)
dfTerm <- tidy(docterm_matrix)
str(dfTerm)
## Removing spare terms #######################
new_docterm_corpus <- removeSparseTerms(docterm_matrix,sparse = 0.95)
new_docterm_corpus
test <- as.matrix(docterm_matrix(docterm_matrix, .5))
head(docterm_matrix$dimnames)


adtm <- TermDocumentMatrix(combine_corpus, control = list(bounds = list(local = c(2,Inf))))
docterm_matrix1 <- as.DocumentTermMatrix(adtm)
docterm_matrix1$dimnames['Terms']
test <- findFreqTerms(docterm_matrix1, 200)
test
# "play"   "your"   "citi"   "mean"   "island" "number" "time"   "day"    "state"  "word"   "man"   
# [12] "river"  "clue"   "lake"   "call"   "presid" "king"   "love"   "war"    "year"   "film"   "type"
docterm_matrix1

test2 <- docterm_matrix1[,c("play","your","citi","mean","island","number","time","day","state","word","man"   
                            ,"river","clue","lake","call","presid","king","love","war","year","film","type")]
colS <- colSums(as.matrix(test2))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)
doc_features

test3 <- as.matrix(test2)
test4 <- as.data.frame(test3)
status(test4)
#test5 <- as.data.frame(as.matrix(docterm_matrix1))

dim(df4)
dim(test4)
dim(finaldf)
head(df4)
head(test4)
finaldf <- cbind(df4[,1],test4)
dim(finaldf)
head(finaldf)
colnames(finaldf)
(unique(finaldf$target))
names(finaldf)[1] <- 'target'  ##### changing the name 

#save.image(file = 'E:/data_scientist/my_work/text_mining_kaggle_competetion/predict.RData')
#load("E:/data_scientist/my_work/text_mining_kaggle_competetion/predict.RData")
str(finaldf)
cormatrix1 <- cor(finaldf[1:500,])
corrplot(cormatrix1,method = 'number')
finaldf$target <- as.numeric(as.factor(finaldf$target))
library(caret)
pdata <- createDataPartition(finaldf$target,p=.8,list = F)
traindata <- finaldf[pdata,]
testdata <- finaldf[-pdata,]
str(testdata)

multiModel <- lm(traindata$target~.,data = traindata)
summary(multiModel)
pred <- predict(multiModel,interval="predict")  
pred1 <- as.data.frame(pred)
(cor(pred1$fit,traindata$target))  #### 0.01015664

############## lets apply binning concepts ###############
finaldf1 <- cbind(df4[,1],test4)
names(finaldf1)[1] <- 'target'
finaldf1$target <- as.numeric(finaldf1$target)
head(finaldf1$target)
hist(finaldf1$target)
qqnorm(finaldf1$target)
boxplot(finaldf1$target)
summary(finaldf1$target)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.0   400.0   600.0   752.6  1000.0 18000.0 
## by looking the box plot we can see the outlirs are above 1900 as uper range is Q3+150%IQR
## 1000 + 900 = 1900 remove it 

rm(finaldf2)
finaldf2 <- filter(finaldf1, finaldf1$target <= 1900)
dim(finaldf2)            ## 197623     23
finaldf2$refinetarget <- NULL
(unique(finaldf2$target))
# [1]  200  400  600  800 1000 1200 1600  100  300  500 1500 1800 1100 1900  700 1400 1492 1300  900  367 1111
# [22]  750 1700 1020    5 1263  350  250 1246 1407 1183 1203 1809 1801  601   50 1777  796   20 1810   22 1512
# [43]  585 1534 1347
summary(finaldf2$target)

### function for binning the values 
binVal <-  function(value)
{
  for(i in seq(100,2000,100))
  {
    if(value <= i)
    {
      return(i)
    }
  }
}
rm(test)
finaldf2$refinetarget <- sapply(finaldf2$target,binVal)
finaldf2$target <- NULL
head(finaldf2)
multiMode2 <- lm(finaldf2$refinetarget~.,data = finaldf2)
summary(multiMode2)
pred <- as.data.frame(predict(multiMode2,interval = 'predict'))
(cor(pred$fit,finaldf2$refinetarget)) ### 0.02419002

####################### Applying different approach ###################
colnames(df2)
df5 <- df2[,c(4,5)]
df5$Value <- as.numeric(df5$Value)
df5 <- filter(df5, df5$Value <= 1900)
df5$Value <- sapply(df5$Value,binVal)

library(quanteda)
# Tokenize questions
reviewtokens=tokens(df5$Question,what="word",
                    remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=TRUE)
head(reviewtokens)
# Lowercase the tokens
reviewtokens=tokens_tolower(reviewtokens)
# remove stop words and unnecessary words
rmwords <- c("aren't","couldn't","didn't","doesn't","don't","hadn't","hasn't","haven't",
             "isn't","mightn't","mustn't","needn't","no","nor","not","shan't","shouldn't",
             "wasn't","weren't","wouldn't")
reviewtokens=tokens_select(reviewtokens, stopwords("english"),selection = "remove")
reviewtokens=tokens_remove(reviewtokens,rmwords)

# Stemming tokens
reviewtokens=tokens_wordstem(reviewtokens,language = "english")
reviewtokens=tokens_ngrams(reviewtokens,n=1:2)

# Creating a bag of words
reviewtokensdfm=dfm(reviewtokens,tolower = FALSE)
dim(reviewtokensdfm)
head(reviewtokensdfm)

### Calculation frequency of feature ################333
textstat_frequency(reviewtokensdfm,n=30)
# Create the dfm
dfm_trim(reviewtokensdfm, min_docfreq = 0.3)
x=dfm_trim(reviewtokensdfm, sparsity = 0.98)
view(head(x))
dim(x)
sort_dfm <- dfm_sort(x, decreasing = TRUE, margin = c("features"))
head(sort_dfm)
dim(sort_dfm)
y <- sort_dfm
dim(y)
colnames(y)
head(y)
class(y)
## Setup a dataframe with features
ndf2=convert(y,to="data.frame")
dim(ndf2)
colnames(ndf2)
head(ndf2)
ndf2$doc_id <- NULL
tokendf <- cbind(df5[,1],ndf2)
dim(tokendf)
colnames(tokendf)
head(tokendf)
names(tokendf)[1] <- 'target'
library(rpart)
tree=rpart(tokendf$target ~ ., data = tokendf, method="class",
           control = rpart.control(minsplit = 200,  minbucket = 30, cp = 0.0001))
summary(tree)
printcp(tree)
plotcp(tree)

ctree_pred <- predict(tree,tokendf[,-1],type = 'class')
ctree_pred

ctree_table <- table(ctree_pred,tokendf$target)
ctree_table

(accuracy <- sum(diag(ctree_table))/sum(ctree_table))  ## 0.2143425

################ split with train and test data #############
ind <- sample(2,nrow(tokendf), replace = T, prob = c(.8,.2))
trainData <- tokendf[ind == 1,]
testData <- tokendf[ind == 2,]
tree=rpart(trainData$target ~ ., data = trainData, method="class",
           control = rpart.control(minsplit = 200,  minbucket = 30, cp = 0.0001))

train_pred <- predict(tree,trainData[,-1],type = 'class')
train_pred

ctree_table <- table(train_pred,trainData$target)
ctree_table

(accuracy <- sum(diag(ctree_table))/sum(ctree_table))  ## 0.2143425

test_pred <- predict(tree,testData[,-1],type = 'class')
test_pred

ctree_table <- table(test_pred,testData$target)
ctree_table

(accuracy <- sum(diag(ctree_table))/sum(ctree_table))  ## 0.2125595

############### Random Forest #######################
library(randomForest)
str(trainData)
trainData$target <- as.factor(trainData$target)
testData$target <- as.factor(testData$target)
fit.forest <- randomForest(trainData$target~.,data=trainData, na.action=na.roughfix,importance=TRUE, ntree=100)
summary(fit.forest)
fit.forest$predicted
rf_pred <- predict(fit.forest,trainData)
rf_pred
dim(rf_pred1)
dim(trainData)

rf_pred_test <- predict(fit.forest,testData)
library(caret)
confusionMatrix(rf_pred,trainData$target)    ### Accuracy : 0.2199 
confusionMatrix(rf_pred_test,testData$target)    ###  Accuracy : 0.2122  
