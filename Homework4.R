rm(list=ls())

# Load packages
library(tm)
require(NLP)
require(openNLP)
library(topicmodels)
require(NLP)
require(openNLP)
library(text2vec)

#Step 1:
# tried 2013, 2014, 2015
fb2013 <- Corpus(DirSource("~/Desktop/fbpost/fb2013"))
fb2013 <- data.frame(text = sapply(fb2013, as.String), stringsAsFactors = FALSE)
fb2013 <- paste(fb2013,collapse = "")

#using technique from homework two: co-occurrence analysis 
####(1) ceate token
it = itoken(fb2013 , preprocessor=tolower, tokenizer=word_tokenizer) 
vocab <- create_vocabulary(ingredients)

###(2)
# vocab_vectorizer() creates an object defining how to transform list of tokens into vector space - i.e. how to map words to indices
vectorizer <- vocab_vectorizer(vocab) 

# Set context window size to 3. The suffix L indicates integer.
tcm <- create_tcm(it, vectorizer, skip_grams_window = 3L)
#tcm

####(3)
target1 = 'cauliflower'
target2 = "rice"

cosim=function(x,y) sum(x*y)/(norm(matrix(x,1),'f') * norm(matrix(y,1),'f'))

m = as.matrix(tcm) # step 1 > 
m = m + t(m) - diag(diag(m)) # step 2 > 
marginal = matrix(rowSums(m), dim(m)[1]) # step 3

####(4)find out the most associated word with cauliflower in 2013, 2014, 2015
sort(tcm[target1,], decreasing=TRUE)[1:5]
cosim(tcm[target1,],tcm[target2,]) 


rm(list=ls())
#Step 2:

docs <- Corpus(DirSource(c("Desktop/fbpost/fb2011", "Desktop/fbpost/fb2012", "Desktop/fbpost/fb2013", "Desktop/fbpost/fb2014", "Desktop/fbpost/fb2015")))
ingredients <- readLines('Desktop/fbpost/ingredients.txt')
ingredients <- tolower(ingredients)
#create dtm
FoodDTM <- DocumentTermMatrix(docs, control=list(tolower=T,dictionary=ingredients))
m <- as.matrix(FoodDTM ) 
d <- data.frame(m)


#create time series
timeseries <- data.frame(rownames(d))
colnames(timeseries)[1] <- "time"
timeseries$year <- substr(timeseries$time, 7,10)
timeseries$month <- as.numeric(substr(timeseries$time, 12,13))
timeseries <- timeseries[2:3]
d <- cbind(timeseries, d)
d <- aggregate(x = d, by = list(d$month, d$year), FUN = sort)
d$time <- paste(d$year,'-',d$month)


#plot 'cauliflower'

data=ts(d[ ,'cauliflower'],start=c(2011, 1), end=c(2015, 12), frequency=12)
plot(data,xlab = 'time',ylab = 'freq', type='l', main = 'Cauliflower Rice Time Trend')

# Validation: plot pumpkin pie

data2=ts(d[ ,'pumpkin'],start=c(2011, 1), end=c(2015, 12), frequency=12)
plot(data2,xlab = 'time',ylab = 'freq', type='l', main = 'Pumpkin Pies Time Trend')
