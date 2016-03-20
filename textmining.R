library(tm)

#Create Corpus
docs <- Corpus(DirSource('C:/Data/Development/datascience/coursera/R/data/TextMining'))

#inspecing a document
#writeLines(as.character(docs[[30]]))

#toSpace content transfermating
toSpace <- content_transformer(function(x, ptrn){return (gsub(ptrn,' ', x))})

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "`")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords('english'))
docs <- tm_map(docs, stripWhitespace)

library("SnowballC")
docs <- tm_map(docs, stemDocument)

docs <- tm_map(docs, content_transformer(gsub), pattern = 'organiz', replacement = 'organ')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'organis', replacement = 'organ')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'andgovern', replacement = 'govern')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'inenterpris', replacement = 'enterpris')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'team-', replacement = 'team')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'individu', replacement = 'individual')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'flexibl', replacement = 'flexible')

docs <- tm_map(docs, content_transformer(gsub), pattern = 'associ', replacement = 'associate')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'increas', replacement = 'increase')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'employe', replacement = 'employee')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'constrain', replacement = 'constraint')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'characterist', replacement = 'character')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'synergi', replacement = 'synergy')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'exercis', replacement = 'exercise')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'analys', replacement = 'analyst')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'platitud', replacement = 'platitude')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'organ', replacement = 'organization')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'worri', replacement = 'worry')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'earli', replacement = 'early')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'pictur', replacement = 'picture')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'definit', replacement = 'definate')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'eventu', replacement = 'eventually')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'resourc', replacement = 'resource')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'necessarili', replacement = 'necessary')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'impli', replacement = 'impliment')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'corp', replacement = 'corporate')
docs <- tm_map(docs, content_transformer(gsub), pattern = 'communiqu', replacement = 'commune')

dtm <- DocumentTermMatrix(docs)

dtm
inspect(dtm[1:3,1000:1005])

freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq, decreasing = TRUE)
freq[head(ord)]
freq[tail(ord)]

dtmr <- DocumentTermMatrix(docs, control=list(wordLengths=c(4,20), bounds = list(global = c(3,27))))
dtmr

freq2 <- colSums(as.matrix(dtmr))
length(freq2)
ordr <- order(freq2, decreasing = TRUE)
freq2[head(ordr)]
freq2[tail(ordr)]

findFreqTerms(dtmr, lowfreq = 80)
findAssocs(dtmr, 'project',0.6)
findAssocs(dtmr, 'enterpris',0.6)
findAssocs(dtmr, 'system',0.6)
wf = data.frame(term=names(freq2), occurrences=freq2)

library(ggplot2)
p <- ggplot(subset(wf, freq2>100), aes(term, occurrences))
p <- p + geom_bar(stat='identity')
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

library(wordcloud)
set.seed(42)

wordcloud(names(freq2),freq2, min.freq = 70, colors=brewer.pal(5, "Dark2"), random.order = FALSE)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]

wordcloud(names(freq2),freq2,min.freq = 70, scale=c(5,0.1), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=pal)




# Create a test data vector
testin <- c(
        "Arizona, Arizona, Arizona, Arizona, ",
        "Arizona, Arizona, Arizona, California Carmel Beach, California LBC, California Napa, Arizona",
        "Virginia, Virginia, Virginia"
)

# The names to remove if duplicated
kickDuplicates <- c("Arizona", "Virginia")


# create a list of vectors of place names
broken <- strsplit(testin, ",\\s*")

# paste each broken vector of place names back together
# .......kicking out duplicated instances of the chosen names
testout <- sapply(broken, FUN = function(x)  paste(x[!duplicated(x) | !x %in% kickDuplicates ], collapse = ", "))

# see what we did
testout

removeRepeatedChars <- content_transformer(function(x){return(gsub('([[:alpha:]])\\1+', '\\1', x))})
removeRepeatedChars(testin)
