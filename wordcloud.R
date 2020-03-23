####packages####
install.packages('KoNLP')
install.packages('wordcloud', 'RcolorBrewer')
install.packages('tm')
install.packages('rJava')
install.packages(".../path/to/package.tar.gz", type="source", repos=NULL)
if (!requireNamespace("KoNLP")) {
  install.packages("KoNLP", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("tidytext")) {
  install.packages("tidytext")
}

if(!requireNamespace('tidyverser')){
  install.packages('tidyverse')
}
install.packages('showtext')
install.packages('treemap')

library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)


library(RColorBrewer)
library(wordcloud)
library(tm)
library(rJava)
library(KoNLP)
library(readxl)
library(tidytext)
library(tidyverse)
library(qgraph)
library(showtext)
library(treemap)
library(htmlwidgets)
library(tidyverse)
library(stringr)
library(stringi)
library(magrittr)
library(NLP4kec)

useSejongDic()

####1.file loading####
src_dir <- setwd("C:/Users/tkpeo/Documents/마이데이터/columns")
src_file <- list.files(src_dir)
src_file_cnt <- length(src_file)
src_file_cnt
src_file

data = data.frame()

for (i in 1:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

read_excel(path = src_file[6], sheet = 1, col_names = T)

for (i in 4:15) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}


for (i in 16:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}


for (i in 31:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 45:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 48:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}



for (i in 51:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 59:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 63:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 70:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 76:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 85:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 91:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 93:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 98:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 100:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 103:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 114:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}


for (i in 119:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 121:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 123:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 127:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 130:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 148:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 162:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 173:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 177:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 179:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

for (i in 182:src_file_cnt) {
  print(i)
  temp <- read_excel(path = src_file[i], sheet = 1, col_names = T)
  data <- rbind(data, temp)
}

####3. text file 생성 ####
column_name <- data$`④컬럼명(한글)`
column_name  

column_name <- gsub("\n","", column_name)
column_name <- gsub("\r","", column_name)
column_name <- gsub("_","", column_name)
column_name <-gsub('[~!@#$%&*()_+=?<>]','', column_name)
column_name <- gsub("\\[","", column_name)
column_name <- gsub("\\d+","",column_name)
column_name <- gsub("\"","",column_name)
column_name <- gsub(",","",column_name)
column_name <- gsub("mt","",column_name)
column_name <- gsub("wt","",column_name)
column_name <- gsub("N A","",column_name)
column_name <- str_replace_all(column_name, "[^가-힣]", " ")
column_name


write.table(column_name, "text.txt", sep = ",")

texts <- as.data.frame(table(column_name))
dim(texts)#[4,316, 2]
texts <- texts[complete.cases(texts), ] #NA제거
nrow(x = texts)

texts <- unique(x = texts) #중복행 제거
nrow(x = texts)


generateIDs <- function(obj, index = 'id') {
  
  # 객체의 종류에 따라 길이를 계산합니다. 
  if (obj %>% class() == 'data.frame') {
    n <- nrow(x = obj)
  } else {
    n <- length(x = obj)
  }
  
  # id를 생성합니다. 
  id <- str_c(
    index, 
    str_pad(
      string = 1:n, 
      width = ceiling(x = log10(x = n)), 
      side = 'left', 
      pad = '0') )
  
  # 결과를 반환합니다. 
  return(id)
}

# texts 객체에 id 컬럼을 추가합니다. 
texts$id <- generateIDs(obj = texts, index = 'doc')
texts$column_name <- as.character(texts$column_name)
#column_name 컬럼의 글자수 확인
textRange <- texts$column_name %>% nchar() %>% range()
print(x = textRange)

#글자 수 구간을 2개로 나로 나눌 때 간격을 계싼
by <- ((textRange[2] - textRange[1]) / 3) %>% round(digits = -1L)
print(x = by)

#도수 분포표를 구한다
cuts <- Hmisc::cut2(
  x = texts$column_name %>% nchar(),
  cuts = seq(from = 0, to = textRange[2], by = by),
  minmax = TRUE)

freq <- table(cuts)
print(freq)

#첫번째 구간 글자 수 20개 미만 확인
texts$column_name[nchar(x = texts$column_name) >by]

####4.형태소 분석 ####
library(NLP4kec)
library(rJava)
parsed <- r_parser_r(contentVector = texts$column_name, language = 'ko')

length(x = parsed)
parsed[10:100]
parsed[is.na(x=parsed) ==T] #NA포함 여부 확인

parsed %>% nchar() %>% table #테이블 생성
duplicated(x = parsed) %>% sum()
unique(parsed) %>% sum()

####5. 말뭉치 (Corpus) 생성 ####
corpus <- parsed %>% VectorSource() %>% VCorpus()
print(corpus)
str(object = corpus[[20]])

####6. N-Gram 단어 만들기 ####
library(RWeka)
bigram <- function(x) {
  NGramTokenizer(x = x, control = Weka_control(min = 2, max = 2))
}

# 단어문서행렬(Term-Document matrix)을 생성합니다. 
bigramList <- corpus %>% 
  TermDocumentMatrix(control = list(tokenize = bigram)) %>% 
  apply(MARGIN = 1, FUN = sum) %>% 
  sort(decreasing = TRUE)

# bigram의 길이를 확인합니다. 
length(bigramList)

# 문서 개수의 1% 이상 발생하는 bigram만 남깁니다. 
# 빈도수가 작은 것은 굳이 관심을 가지지 않아도 됩니다. 
bigramList <- bigramList[bigramList >= (nrow(x = texts) * 0.01)]
length(x = bigramList)


# bigram의 컬럼명(글자)만 따로 추출하여 bigramNames에 할당합니다. 
bigramNames <- names(bigramList)

# bigramNames을 육안으로 확인하기 위해 최대 100개까지 출력합니다.
top <- if (length(x = bigramNames) >= 100) bigramNames[1:100] else bigramNames
print(top)

####7.Corpus재생성 ####
write.table(
  x = top, 
  quote = FALSE, 
  file = 'spacing.txt', 
  row.names = FALSE, 
  col.names = FALSE)

spacing <- read.table(file = 'spacing.txt', sep = '\t')
colnames(x = spacing) <- 'before'
spacing <- unique(x = spacing)#중복제거

# 띄어쓰기 없앤 문자벡터를 after 컬럼으로 추가합니다. 
spacing$after <- spacing$before %>% str_remove_all(pattern = ' ')

write.table(
  x = spacing$after,
  quote = FALSE,
  file = 'dictionary.txt',
  row.names = FALSE,
  col.names = FALSE)

####8. 형태소 분석 재실행 ####
parsed <- r_parser_r(
  contentVector = texts$column_name, 
  language = 'ko',
  korDicPath = 'dictionary.txt')

help(r_parser_r)
####9. 불용어 제거####

myStopwords <- read.table(
  file = 'https://raw.githubusercontent.com/MrKevinNa/TextMining/master/stopwords.txt') %>% 
  .$V1 
corpus <- tm_map(x = corpus, FUN = removeWords, myStopwords)
corpus <- tm_map(x = corpus, FUN = stripWhitespace) #화이트 스페이스 제거

parsedDf <- data.frame(
  id = generateIDs(obj = parsed, index = 'doc'),
  parsedContent = parsed, 
  corpusContent = sapply(X = corpus, FUN = `[[`, 'column_name'))


####10.DTM 생성 ####
# DTM을 생성합니다. 
dtm <- DocumentTermMatrix(x = corpus, control = list(wordLengths = c(2, Inf)))
colnames(x = dtm) <- trimws(x = colnames(x = dtm), which = 'both')
dim(x = dtm)
# dtm의 차원을 줄입니다. 
dtm <- removeSparseTerms(x = dtm, sparse = as.numeric(x = 0.99))
dim(x = dtm)

# 행의 합이 0인 건수를 확인합니다. 
rowSums(x = dtm %>% as.matrix()) %>% table()

# 문서 이름(row name)을 지정합니다. 나중에 생성할 parsedDf와 병합하기 위함입니다.
dtm$dimnames$Docs <- generateIDs(obj = dtm$dimnames$Docs, index = 'doc')


# dtm 객체를 육안으로 확인합니다. 
dtm$dimnames$Docs[1:40]
dtm$dimnames$Terms[1:40]

####11. TF-IDF ####
dtmTfIdf <- DocumentTermMatrix(
  x = corpus,
  control = list(
    removeNumbers = TRUE,
    wordLengths = c(2, Inf),
    weighting = function(x) weightTfIdf(x, normalize = TRUE) ))

colnames(x = dtmTfIdf) <- trimws(x = colnames(x = dtmTfIdf))
dtmTfIdf <- removeSparseTerms(x =  dtmTfIdf, sparse = as.numeric(x = 0.99))
dim(x = dtmTfIdf)
# 행의 합이 0인 건수를 확인합니다.
rowSums(x = dtmTfIdf %>% as.matrix() %>% round(digits = 1L)) %>% table()
# 문서 이름(row name)을 지정합니다. 나중에 생성할 parsedDf와 병합하기 위함입니다.
dtmTfIdf$dimnames$Docs <- generateIDs(obj = dtmTfIdf$dimnames$Docs, index = 'doc')

####12.고빈도 단어 시각화 ####
# dtm에 언급된 단어(term)별 빈도수를 생성합니다.
wordsFreq <- dtm %>% as.matrix() %>% colSums()

# 사용된 단어의 총 개수를 확인합니다.
length(x = wordsFreq)

# 내림차순으로 정렬하고, 상위 20개만 확인합니다.
wordsFreq <- wordsFreq[order(wordsFreq, decreasing = TRUE)]
head(x = wordsFreq, n = 20L)
str(wordsFreq)

# 단어 빈도를 막대그래프로 그리기 위해 데이터 프레임으로 변환한 다음 
# 내림차순으로 정렬합니다.
wordDf <- data.frame(
  word = names(x = wordsFreq),
  freq = wordsFreq,
  row.names = NULL) %>% 
  arrange(desc(x = freq))

# 건수를 확인합니다.
nrow(x = wordDf)

## [1] 8396
# 문서 개수의 1% 이상 발생하는 bigram만 남깁니다. 
# 빈도수가 작은 것은 굳이 관심을 가지지 않아도 됩니다. 
bigramList <- bigramList[bigramList >= (nrow(x = texts) * 0.01)]
length(x = bigramList)


column_name <- sapply(column_name, extractcolumn_name, USE.NAMES = F)
extractcolumn_name(column_name)
column_name
type(column_name)
text <- file("C:/Users/tkpeo/Documents/마이데이터/columns1/text.txt", encoding="cp949")
text <- readLines(text)

excolumn_names <- function(x) { paste(extractcolumn_name(as.character(x)), collapse=" ")}
column_name <- sapply(column_name, excolumn_names)
column_name <- unlist(column_name)
head(column_name)

#####13. 고빈도 단어 시각화 ####
mytheme <- theme(
  plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
  axis.title.x = element_text(color = 'blue', size = 12, face = 'bold'),
  axis.title.y = element_text(color = '#993333', size = 12, face = 'bold'),
  axis.text.x = element_text(family = 'NanumGothic', size = 10, face = 'bold'),
  axis.text.y = element_blank(), 
  axis.ticks.length = unit(0, 'cm'),
  panel.background = element_blank(),
  panel.grid = element_blank() )

wordDf <- wordDf %>% filter(between(wordDf$freq,  2, 4))

ggplot(
  data = head(x = wordDf %>% filter(freq<13), n= 20L), 
  mapping = aes(
    x = reorder(word, -freq), 
    y = freq)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = freq), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme 

display.brewer.pal(n = 8, name = 'Set2')
pal <- brewer.pal(n = 8, name = 'Set2')
#wordcloud
wordDf <- wordDf %>% filter(freq <10)
if (nrow(x = wordDf) >= 300) wordCloud <- wordDf[1:300, ] else wordCloud <- wordDf

wordcloud2(
  data = wordCloud,
  size = 0.1,
  fontFamily = 'NanumGothic',
  color = pal,
  backgroundColor = 'white',
  minRotation = -pi / 4,
  maxRotation = pi / 4,
  shuffle = TRUE,
  rotateRatio = 0.25,
  figPath = 'seoul.png',
  ellipticity = 0.6)

# Wordcloud를 그립니다. 
wordcloud2(
  data = wordCloud,
  size = 0.1,
  fontFamily = 'NanumGothic',
  color = pal,
  backgroundColor = 'white',
  minRotation = -pi / 4,
  maxRotation = pi / 4,
  shuffle = TRUE,
  rotateRatio = 0.25,
  shape = 'circle',
  ellipticity = 0.6)


# 고빈도 단어 트리맵을 그립니다. 
treemap(
  dtf = wordDf %>% filter(freq<2),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')


treemap(
  dtf = wordDf %>% filter(between(freq,2, 5)),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')

treemap(
  dtf = wordDf %>% filter(between(freq,2, 5)),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')

treemap(
  dtf = wordDf %>% filter(between(freq,5, 10)),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')

treemap(
  dtf = wordDf %>% filter(between(freq,10, 15)),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')

treemap(
  dtf = wordDf %>% filter(between(freq,15, 20)),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')

treemap(
  dtf = wordDf %>% filter(between(freq,20, 25)),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')

treemap(
  dtf = wordDf %>% filter(between(freq,25, 30)),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')

treemap(
  dtf = wordDf %>% filter(freq > 30),
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq',
  fontfamily.labels = 'NanumGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col = 'white')


####14. 단어 연관성 분석-dtmTfIdf ####
# 상관계수 행렬을 직접 생성합니다.
corTerms <- dtmTfIdf %>% as.matrix() %>% cor()

corTerms[1:10]
# 상관계수 행렬로 연관성 높은 단어를 확인합니다.
checkCorTerms <- function(n = 10, keyword) {
  
  # 키워드 유무를 확인합니다.
  corTerms %>% 
    colnames() %>% 
    str_subset(pattern = keyword) %>% 
    print()
  
  # 연관 키워드가 있는 컬럼의 전체 단어를 한꺼번에 출력합니다.
  corRef <- data.frame()
  
  # 상관계수 높은 순서로 정렬합니다.
  corRef <- corTerms[ , keyword] %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    set_colnames(c('corr'))
  
  # 미리보기 합니다. 
  head(x = corRef, n = n + 1)
}

checkCorTerms(n = 10, keyword = '납부')


# 차원을 확인합니다. 
dim(x = corTerms)
column_name <- Filter(function(x){nchar(x)>=2}, column_name)
wc <- table(column_name)

wc_df <- as.data.frame(wc)

wc_df$column_name <- as.character(wc_df$column_name)
names(wc_df)
wc_df<- wc_df %>%
  unnest_tokens(input = column_name, output = word)

wc_fin <- wc_df %>% count(word, sort = T)

font_add_google("Noto Sans", "notosans")
showtext_auto()
wc_fin %>% with(wordcloud(word, n, family ='notosans'))

wc_fin_1050 <- wc_fin %>% filter(between(n, 10, 50))
pal <- brewer.pal(12,"Paired") # 12가지 색상 pal <- brewer.pal(9,"Set1") # Set1~ Set3
pal1 <- brewer.pal(n = 8, name = 'Set2')
wc_fin_1050 %>% with(wordcloud(word, n, colors = pal, family ='notosan'))


head(wc_fin)
plot(wc_fin$n)
wc_fin %>% filter(between(n, 10, 30))
ggplot(data = wc_fin %>% filter(between(n, 0, 10)), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(between(n, 10, 30)), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(between(n, 30, 50)), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(between(n, 50, 70)), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(between(n, 70, 90)), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(between(n, 90, 120)), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(between(n, 120, 150)), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(n > 150), aes(x= n))+geom_histogram()
ggplot(data = wc_fin %>% filter(between(n,150,300)), aes(x= n))+geom_histogram()



####15. 단어 연관성 분석 findAssocs()####


#####mythem####
mytheme <- theme(
  plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
  axis.title.x = element_text(color = 'blue', size = 12, face = 'bold'),
  axis.title.y = element_text(color = '#993333', size = 12, face = 'bold'),
  axis.text.x = element_text(family = 'NanumGothic', size = 10, face = 'bold'),
  axis.text.y = element_blank(), 
  axis.ticks.length = unit(0, 'cm'),
  panel.background = element_blank(),
  panel.grid = element_blank() )


#### wc_010 wisualization####
wc_010 = wc_fin %>% filter(between(n, 0, 10)) 

#1. wc_010 고빈도단어
ggplot(
  data = head(x = wc_010, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_010 wordcloud
if (nrow(x = wc_010) >= 300) wc_010 <- wc_010[1:300, ] else wc_010 <- wc_010
wc_010 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_010, fontFamily = '나눔바른고딕')
letterCloud(data = wc_1030, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_010 treemap 
treemap(
  dtf = wc_010,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')



####wc_1030 visualization ####
wc_1030 = wc_fin %>% filter(between(n, 10, 30))

#1. wc_10300 고빈도단어
ggplot(
  data = head(x = wc_1030, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_1030 wordcloud
if (nrow(x = wc_1030) >= 300) wc_1030 <- wc_1030[1:300, ] else wc_1030 <- wc_1030
wc_1030 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_1030, fontFamily = '나눔바른고딕')
letterCloud(data = wc_1030, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_1030 treemap 
treemap(
  dtf = wc_1030,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')


####wc_5070 visualizatio####
wc_5070 = wc_fin %>% filter(between(n, 30, 50))

#1. wc_5070 고빈도단어
ggplot(
  data = head(x = wc_5070, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_5070 wordcloud
if (nrow(x = wc_5070) >= 300) wc_5070 <- wc_5070[1:300, ] else wc_5070 <- wc_5070
wc_5070 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_5070, fontFamily = '나눔바른고딕')
letterCloud(data = wc_5070, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_5070 treemap 
treemap(
  dtf = wc_5070,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')



####wc_5070 visualization####
wc_5070 = wc_fin %>% filter(between(n, 50, 70))

#1. wc_5070 고빈도단어
ggplot(
  data = head(x = wc_5070, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_5070 wordcloud
if (nrow(x = wc_5070) >= 300) wc_5070 <- wc_5070[1:300, ] else wc_5070 <- wc_5070
wc_5070 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_5070, fontFamily = '나눔바른고딕')
letterCloud(data = wc_5070, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_5070 treemap 
treemap(
  dtf = wc_5070,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')

####wc_7090 visualization####
wc_7090 = wc_fin %>% filter(between(n, 70, 90))

#1. wc_7090 고빈도단어
ggplot(
  data = head(x = wc_7090, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_7090 wordcloud
if (nrow(x = wc_7090) >= 300) wc_7090 <- wc_7090[1:300, ] else wc_7090 <- wc_7090
wc_7090 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_7090, fontFamily = '나눔바른고딕')
letterCloud(data = wc_7090, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_7090 treemap 
treemap(
  dtf = wc_7090,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')


####4. wc_90120 visualization####
wc_90120 = wc_fin %>% filter(between(n, 90, 120))

#1. wc_90120 고빈도단어
ggplot(
  data = head(x = wc_90120, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_90120 wordcloud
if (nrow(x = wc_90120) >= 300) wc_90120 <- wc_90120[1:300, ] else wc_90120 <- wc_90120
wc_90120 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_90120, fontFamily = '나눔바른고딕')
letterCloud(data = wc_90120, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_90120 treemap 
treemap(
  dtf = wc_90120,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')

####wc_120150 visualization####
wc_120150 = wc_fin %>% filter(between(n, 120, 150))

#1. wc_120150 고빈도단어
ggplot(
  data = head(x = wc_120150, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_120150 wordcloud
if (nrow(x = wc_120150) >= 300) wc_120150 <- wc_120150[1:300, ] else wc_120150 <- wc_120150
wc_120150 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_120150, fontFamily = '나눔바른고딕')
letterCloud(data = wc_120150, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_120150 treemap 
treemap(
  dtf = wc_120150,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')


####wc_150 visualization ####
wc_150 = wc_fin %>% filter(n > 150)

#1. wc_150 고빈도단어
ggplot(
  data = head(x = wc_150, n = 20L), 
  mapping = aes(
    x = reorder(word, -n), 
    y = n)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = n), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme

#2. wc_150 wordcloud
if (nrow(x = wc_150) >= 300) wc_150 <- wc_150[1:300, ] else wc_150 <- wc_150
wc_150 %>% with(wordcloud(word, n, colors = pal1, fontfamily ='notosan'))
wordcloud2(data = wc_150, fontFamily = '나눔바른고딕')
letterCloud(data = wc_150, word ='MAP', wordSize = 1, fontFamily = '나눔고딕')

#3. wc_150 treemap 
treemap(
  dtf = wc_150,
  title="고빈도 단어 트리맵",
  index = c('word'),
  vSize = 'n',
  fontfamily.labels = 'NanuGothic',
  fontsize.labels = 14,
  palette = pal,
  border.col= 'white')

wc_150[!(wc_150$word == "코드")]
wc_150 %>% filter(word != "코드",
                  word !="구분",
                  word !="id", 
                  word !="번호",
                  word !="여부") %>% 
  with(wordcloud(word, n, colors = pal, family ='notosan'))

n_10000<-column_name[1:100000]

ko.words <- function(doc){
  
  d <- as.character(doc)
  
  pos <- paste(SimplePos09(d))
  
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  
  keyword <- extracted[,2]
  
  keyword[!is.na(keyword)]
  
}


cps <- Corpus(VectorSource(n_10000))
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=ko.words,       
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6),
                                       weighting=weightBin))

column_name[1:256418]
dim(tdm)
tdm.matrix <-as.matrix(tdm)
rownames(tdm.matrix)[1:37]
tdm.matrix

word.count <- rowSums(tdm.matrix)  ##각 단어별 합계를 구함

word.order <- order(word.count, decreasing=T)  #다음으로 단어들을 쓰인 횟수에 따라 내림차순으로 정렬

freq.words <- tdm.matrix[word.order[1:20], ] #Term Document Matrix에서 자주 쓰인 단어 상위 20개에 해당하는 것만 추출

co.matrix <- freq.words %*% t(freq.words)  #행렬의 곱셈을 이용해 Term Document Matrix를 Co-occurence Matrix로 변경

co.matrix
word.order
#출처: https://datacookbook.kr/17 [DATA COOKBOOK]

qgraph(co.matrix,
       
       labels=rownames(co.matrix),   ##label 추가
       
       diag=F,                       ## 자신의 관계는 제거함
       
       layout='spring',              ##노드들의 위치를 spring으로 연결된 것 처럼 관련이 강하면 같이 붙어 있고 없으면 멀리 떨어지도록 표시됨
       
       edge.color='blue',
       
       vsize=log(diag(co.matrix))*2)




