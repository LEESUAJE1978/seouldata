text <- readLines(text)

exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse=" ")}
noun <- sapply(text, exNouns)
noun <- unlist(noun)
head(noun)


noun <-gsub('[~!@#$%&*()_+=?<>]','', noun)
noun <- gsub("\\[","", noun)
noun <- gsub("\\d+","",noun)
noun <- gsub("\"","",noun)
noun <- gsub(",","",noun)
noun <- gsub("mt","",noun)
noun <- gsub("wt","",noun)
noun <- gsub("N A","",noun)

install.packages('NLP4kec')


noun <- Filter(function(x){nchar(x)>=2}, noun)
wc <- table(noun)

corpus <- noun[1000:2000]
text[1000:2000]
corpus <- corpus %>% VectorSource() %>% VCorpus()
print(corpus)
str(object = corpus[1])
install.packages('RWeka')
library(RWeka)
bigram <- function(x) {
  NGramTokenizer(x = x, control = Weka_control(min = 2, max = 2))
}

bigramList <- corpus %>% 
  TermDocumentMatrix(control = list(tokenize = bigram)) %>% 
  apply(MARGIN = 1, FUN = sum) %>% 
  sort(decreasing = TRUE)

length(bigramList)


bigramNames <- names(bigramList)

top <- if (length(x = bigramNames) >= 100) bigramNames[1:100] else bigramNames
print(top)

write.table(
  x = top, 
  quote = FALSE, 
  file = 'spacing.txt', 
  row.names = FALSE, 
  col.names = FALSE)

spacing <- read.table(file = 'spacing.txt', sep = '\t')
colnames(x = spacing) <- 'before'
spacing <- unique(x = spacing)
spacing$after <- spacing$before %>% str_remove_all(pattern = ' ')
write.table(
  x = spacing$after,
  quote = FALSE,
  file = 'dictionary.txt',
  row.names = FALSE,
  col.names = FALSE)

library(NLP4kec)

parsed <- r_parser_r(
  contentVector = texts$content, 
  language = 'ko',
  korDicPath = 'dictionary.txt')
