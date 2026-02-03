# Social-Listening-Winning-Customers-Back-Using-Sentiment-Analysis-and-Topic-Modeling
# Introduction

Social listening is defined as “the process of monitoring social media channels for mentions of your brand, competitors, product, and more”. Nowadays many brands endeavor to do social listening to track its “pulse”, i.e., its image in consumers’ minds. Twitter is the most major channel through which brands conduct social listening. This tutorial uses the brand Victoria’s Secret as an example to demonstrate how brands can understand consumers’ perceptions and react to them through topic modeling, or more specifically, the Latent Dirichlet Allocation (LDA) method. Victoria’s Secret is the world’s leading lingerie, clothing, and beauty retailer. Needless to say the brand is well known for its high visibility marketing and branding, including an annual fashion show with supermodels dubbed Angels. However, in recent years, the brand has been described as “from glory to crisis”. The brand has been widely criticized for its lack of diversity, for having overly skinny models, and for cultural appropriation. The purpose of this tutorial is hence to help Victoria’s Secret understand (1) what customers have been tweeting about the brand, and (2) how to win customers back, i.e., to design marketing strategies in reaction to the most severe issues that VS is faced with.

<img width="1134" height="644" alt="image" src="https://github.com/user-attachments/assets/899715f2-4863-4abc-bbb4-86f5eeefa4c6" />
Example of Negative Tweets about VS

# Objectives

To use sentiment analysis to analyze people’s overall sentiment about a brand;
To use the LDA topic modeling approach to analyze customer perceptions (through Tweets) about a brand; and
To design business strategies to resolve negative customer perceptions to “win customers back”.
Before we roll out our analyses, let us make sure that all your files are organized and your R environment is set up.

# Preparation and Set Up
Please follow the following steps:

Create a folder and set working directory to that folder on your device,
Download data needed for this tutorial onto your working directory.
Let us create a folder in your computer and name the folder topic. And then let us download the data file (“tweet.csv”) needed for this tutorial. Please run only part of the following chunk of codes, depending on the operating system of your device. To avoid running the codes that are not applicable and receiving error messages, please add “#” before each line of non-applicable codes in the chunk below. For example, if you are using a Mac, you need to put “#” before code setwd(“H:/downloads/topic”).

```r
# Please run the following codes depending on your operating system.

# If you are using macOS and your folder is under "Downloads":
setwd("/Users/olivia/Downloads/Social Listening I")

# If you are using Windows and your folder is in H drive:
# setwd("H:/downloads/topic")

data <- readr::read_csv("tweets.csv")

## Rows: 2025 Columns: 2
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (1): text
## dbl (1): id
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
Now we are ready to go!

1. Data
The dataset we will be using for simplicity purpose will be a random sample of over 2000 tweets with the keyword “victoriassecret”, scrapped from Twitter during the period of 01/01/2020 to 31/12/2020. There are two columns in the data file, “id” and “text”.

Let us select a few of the tweets in the data file and see what consumers have been talking about the brand in the past year:

kable(data[c(20, 92, 100, 400, 990, 1500, 1700, 1800),])
id	text
20	Check out what I just added to my closet on Poshmark: Victoria’s Secret zip up hooded sweatshirt. https://t.co/R3f8jN47N0 via @poshmarkapp #shopmycloset
92	RT @HausofHilton: If you expect your girl to look like a Victoria secret model, make sure you look like a Calvin Klein model.
100	Check out what I just added to my closet on Poshmark: Victoria’s Secret Bras. https://t.co/Ix1dEuzg5D via @poshmarkapp #shopmycloset
400	Check out what I just added to my closet on Poshmark: Lady’s Victoria secret hoodie sweatshirt Sz small. https://t.co/nwao7qTh1z via @poshmarkapp #shopmycloset
990	being told you’re wearing the wrong size bra at Victoria’s secret is a part of the female experience
1500	RT @_scentsng: VICTORIA’S SECRET body mists available Price:3000 each Location:kaduna Nationwide delivery Please retweet 😊 https://t.co/5mM…
1700	
RT @rarasells: Bismillah

When you have a bad day, you have to smell something sweet to calm down your mind and soul✨

Bcs i have not so go…
1800	RT @ma_ad_: Can’t tell if after we collided is sponsored by Victoria’s Secret or pornhub
Two things that we notice from the above raw data:

Consumer sentiments are quite diverse and divided towards Victoria’s Secret. Tweet 20, 100, and 400, for example, are consumers sharing about their recent purchase or product news (e.g., new launches). Tweet 1700 is about consumers enjoying and appreciating the brand. However, Tweet 92, 990, and 1800 are quite negative in terms of sentiment. Tweet 92 and 990, in particular, are a very typical tweets criticizing body shame and gender inequality issues arising from Victoria’s Secret’s marketing communications.
In terms of the data, we observe many tweets consisting of information irrelevant to our analysis, such as “RT”, the twitter handle, punctuation, stopwords (i.e., and, or, the, etc), numbers, and emojis. These will add unnecessary noise to our data set which we need to remove during the pre-processing stage.
# Raw data cleaning--you can further adapt the codes below and add more rows to further clean the raw texts beyond what are done below. 

# removing Tweet-specific terms like "RT" and "@"
data$text <- sub("RT.*:", "", data$text)
data$text <- sub("@.* ", "", data$text)

text_cleaning_tokens <- data %>% 
  tidytext::unnest_tokens(word, text)
# removing some weird symbols and stopwords, etc. 
text_cleaning_tokens$word <- gsub('œ', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('â', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('ð', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('ÿ', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('http', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('t.co', '', text_cleaning_tokens$word)
# removing digits and punctuation
text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)

text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word)<=1))%>% 
  anti_join(stop_words)
## Joining with `by = join_by(word)`
tokens <- text_cleaning_tokens %>% filter(!(word==""))
tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""
tokens <- tidyr::unite(tokens, text,-id,sep =" " )
tokens$text <- trimws(tokens$text)

# Take a look at the processed texts for tweet 100 to tweet 120 in the new file: 
#kable(tokens[c(100:120),])
2. Sentiment Analysis: Lexicon-based (Dictionary-based) Approach
Find the top 10 commonly used words in the set of tweets; this will give an overall picture of what the populations are most concerned about, and the extent to which they are engaged on these topics.

text_cleaning_tokens %>%
  count(word,sort=TRUE) %>%
  top_n(10) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x=word, y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  theme_classic()+
  labs(x="Count",
       y="Unique words",
       title="Unique word counts found in the dataset")
## Selecting by n
 Perform sentiment analysis using the Bing lexicon and get_sentiments function from the tidytext package. There are many libraries, dictionaries and packages available in R to evaluate the emotion prevalent in a text. The tidytext and textdata packages have such word-to-emotion evaluation repositories. Three of the general purpose lexicons are Bing, AFINN and nrc (from the textdata package, as introduced in the lecture as well). To take a look at what each package contains, you can run the get_sentiments() function in R.

Let’s first take a look at the Bing lexicons:

get_sentiments("bing") %>% filter(sentiment=="positive")
## # A tibble: 2,005 × 2
##    word        sentiment
##    <chr>       <chr>    
##  1 abound      positive 
##  2 abounds     positive 
##  3 abundance   positive 
##  4 abundant    positive 
##  5 accessable  positive 
##  6 accessible  positive 
##  7 acclaim     positive 
##  8 acclaimed   positive 
##  9 acclamation positive 
## 10 accolade    positive 
## # ℹ 1,995 more rows
get_sentiments("bing") %>% filter(sentiment=="negative")
## # A tibble: 4,781 × 2
##    word        sentiment
##    <chr>       <chr>    
##  1 2-faces     negative 
##  2 abnormal    negative 
##  3 abolish     negative 
##  4 abominable  negative 
##  5 abominably  negative 
##  6 abominate   negative 
##  7 abomination negative 
##  8 abort       negative 
##  9 aborted     negative 
## 10 aborts      negative 
## # ℹ 4,771 more rows
In contrast to Bing, the AFINN lexicon assigns a “positive” or “negative” score to each word in its lexicon; further sentiment analysis will then add up the emotion score to determine overall expression. A score greater than zero indicates positive sentiment, while a score less than zero would mean negative overall emotion. A calculated score of zero indicates neutral sentiment (neither positive or negative).

# You can change the values as you like; note that all values should be integers within the range of [-5,5]

get_sentiments("afinn") %>% filter(value==5)
## # A tibble: 5 × 2
##   word         value
##   <chr>        <dbl>
## 1 breathtaking     5
## 2 hurrah           5
## 3 outstanding      5
## 4 superb           5
## 5 thrilled         5
get_sentiments("afinn") %>% filter(value==-5)
## # A tibble: 16 × 2
##    word           value
##    <chr>          <dbl>
##  1 bastard           -5
##  2 bastards          -5
##  3 bitch             -5
##  4 bitches           -5
##  5 cock              -5
##  6 cocksucker        -5
##  7 cocksuckers       -5
##  8 cunt              -5
##  9 motherfucker      -5
## 10 motherfucking     -5
## 11 niggas            -5
## 12 nigger            -5
## 13 prick             -5
## 14 slut              -5
## 15 son-of-a-bitch    -5
## 16 twat              -5
To perform sentiment analysis using Bing dictionary on the tweets, we can run the following codes.

bing_vs = text_cleaning_tokens%>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort=TRUE) %>%
  ungroup()
## Joining with `by = join_by(word)`
# Let us take a look at the top 20 words, ranked by their frequency (note that not all the words in the tweets are captured by the Bing dictionary; what we are listing here only include those words that are in Bing's dictionary, hence, with a sentiment polarity score or categorization)
bing_vs[1:20,]
## # A tibble: 20 × 3
##    word       sentiment     n
##    <chr>      <chr>     <int>
##  1 top        positive     58
##  2 sexy       positive     49
##  3 love       positive     33
##  4 shit       negative     20
##  5 angel      positive     19
##  6 mist       negative     18
##  7 cute       positive     16
##  8 gold       positive     16
##  9 free       positive     13
## 10 adore      positive     12
## 11 fat        negative     12
## 12 smell      negative     12
## 13 compatible positive     11
## 14 pretty     positive     11
## 15 tank       negative     11
## 16 limited    negative     10
## 17 bitch      negative      8
## 18 classic    positive      7
## 19 damn       negative      7
## 20 hot        positive      7
Then to visually depict the word counts, you can filter and plot the words side-by-side to compare the positive vs negative emotion.

bing_vs %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales="free_y") +
  labs(title="Tweets on Victoria's Secret", y="Contribution to sentiment", x=NULL)+
  coord_flip()+
  theme_bw()
## Selecting by n
 What interpretations/inferences can you make from the above results?

For example, Victoria’s Secret is appraised probably because of the appearance of the products and the super models (or angels) that are typically associated with words like “sexy:,”cute”, “pretty”, “gold”. It seems that the public also associate the word “free”, which probably refers to “freedom”, and “love”, with the brand. However, on the dark side, the public have been quite harsh on the brand because very aggressive and sometimes offensive words are used in Tweets about the brand. People seem to be not happy with the smell of the products, and maybe were complaining about the limited sizes or styles (judging from the word “limited”). More importantly, we see words like “fat” and “pathetic”, which could hint that the public are starting to associate body shame and female inequality issues with the brand.

If you are interested, you can further extend the codes by generating sentiment scores for each tweet, so that you can rank the sentiment score by tweets instead of by word. Then you might be able to pick out the tweets with the most positive and most negative sentiments and take a further look into the specific contents in each tweet, so that you understand what people are really talking about you.

# get nrc sentiment for each tweet
nrc_vs<-get_nrc_sentiment(tokens$text)
# take a quick look at the first few tweets and their nrc score
head(nrc_vs)
##   anger anticipation disgust fear joy sadness surprise trust negative positive
## 1     0            0       0    0   0       0        0     0        0        0
## 2     0            0       0    0   0       0        0     1        0        0
## 3     0            1       0    0   1       0        1     1        1        1
## 4     0            0       0    0   0       0        0     1        0        0
## 5     0            1       0    0   1       0        0     2        0        1
## 6     0            1       0    0   1       0        0     2        0        1
barplot(colSums(nrc_vs),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')


Question: What are your comments on the sentiment analysis results using NRC dictionary?

3. Topic Modeling: LDA
First you will have to create a DTM (Document Term Matrix), which is a sparse matrix containing all the terms (or words, and combinations of words) and documents (in our case, documents refer to tweets) as the two dimensions. A DTM is a matrix that describes the frequency of terms that occur in a collection of documents. In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms. When building the DTM, you can select how you want to tokenise (i.e., break up a sentence into 1 word or 2 words or more) your text. This will depend on how you want the LDA to read your words. You will need to ask yourself if singular words or phrases makes more sense in your context. For instance, if your texts contain words such as “failed processing” or “not appreciating”, then you will have to let the algorithm choose a window of maximum 2 words to correctly capture the meaning. In our case, because we are analyzing Twitter sentiment, we will go with a window size of 1–3 words, and let the algorithm decide which are the more important phrases to concatenate together. We will also explore the term frequency matrix, which shows the number of times the word/phrase is occurring in the entire corpus of text. If the term is less than 2, we discard them, as it does not add any value to the algorithm, and it will help to reduce computation time as well. We do the same for words that appear in more than 80% the documents (Q:what might be a good motivation for this?).

#create DTM
dtm <- CreateDtm(tokens$text, 
                 doc_names = tokens$id, 
                 ngram_window = c(1, 3))
## as(<dgTMatrix>, "dgCMatrix") is deprecated since Matrix 1.5-0; do as(., "CsparseMatrix") instead
#explore the frequency of each term across all documents/Tweets. 
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

# Eliminate words appearing less than 2 times or in more than 80% of the documents. Then the "vocabulary" will be our corpus in the analysis
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) *0.8 ]
dtm = dtm
DTM is a large matrix–if you click on it in the Global Environment, you will then be able to see that its dimensions are 1937 and 15850. 1937 refers to the number of “documents” or tweets that we have, and 15850 refers to the total number of terms (i.e., tokenised texts, with a window size between 1 and 3) across the 1937 documents. If you type “max(dtm)” in your Console, you will see that the maximum term frequency is 7, meaning that a term appears 7 (max) times in the same Tweet.

The so-called term frequency matrix, or “tf”, is actually like the column sum of the DTM: for each of the 15850 terms, what is the total frequency across all the 1937 Tweets? You can see in the Global Environment that “tf” is a data frame that has 15850 rows, each referring to a term.

In “tf”: (1) “term_freq”, or term frequency, refers to the total number of times that a term appears in documents, (2) “document_freq”, or document frequency, refers to the total number of documents that contain this term. Hence for a few terms, “term_freq” is greater than “document_freq”, because a term can appear more than 1 times in a document.

With your DTM, you run the LDA algorithm for topic modeling.

You will have to manually assign a number of topics, 𝑘
. Next, the algorithm will calculate a coherence score to allow us to choose the best topics from 1 to 𝑘
.

Coherence gives the probabilistic coherence of each topic. Coherence score is a score that calculates if the words in the same topic make sense when they are put together. This gives us the quality of the topics being produced. The higher the score for the specific number of k, it means for each topic, there will be more related words together and the topic will make more sense. In the chunk of codes below, the function “CalcProbCoherence()” calculates the probabilistic coherence of the topics. This approximates semantic coherence or human understandability of a topic.

In our example, we set 𝑘=1,2,3,...,8
 and run the LDA on each value of k, and plot the coherence score. It’s up to you to define how many topics you want. The optimal number of k should correspond to the highest coherence score, unless you have better reasons to justify other k as your final choice.

k_list <- seq(1, 8, by = 1)

# from here onwards, we have some standard codes to set up the LDA model. You do not have to understand what each line of codes mean.
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500) #here we fit the LDA model with 500 epochs
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5) #calculate topic coherence using the top 5 words of each topic (M=5 is the default)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) 

#choosing the best model based on the coherence score
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,8,1)) + ylab("Coherence")


Upon plotting of the k, we realize that 𝑘=3
 gives us the highest coherence score.

In this case, note that the coherence score is not high and there will definitely be a need to tune the model, such as increasing k to achieve better results or have more texts. But for explanation purpose, we will ignore the value and just go with the highest coherence score. After understanding the optimal number of topics, we want to have a peek of the different words within the topic. Each topic will have each word/phrase assigned a phi value (pr(word|topic)) — probability of word given a topic. So we only take into account the top 30 values per word in each topic. The top 20 terms will then describe what the topic is about.

model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 30)
top30_wide <- as.data.frame(model$top_terms)

kable(top30_wide)
t_1	t_2	t_3
secret	secret	secret
victorias	model	check
victorias_secret	victoria	added
victoria	victoria_secret	poshmark
victoria_secret	secret_model	shopmycloset
ebay	girl	added_closet
sale	victoria_secret_model	added_closet_poshmark
check	expect	check_added
panties	calvin	check_added_closet
fashion	calvin_klein	closet
im	girl_victoria	closet_poshmark
bra	girl_victoria_secret	victorias_secret
victoriassecret	klein	victorias
semi	calvin_klein_model	poshmark_victorias
annual	klein_model	poshmark_victorias_secret
check_victorias	expect_girl	closet_poshmark_victorias
check_victorias_secret	expect_girl_victoria	pink
semi_annual	model_calvin	victoria_secret
secret_fashion	model_calvin_klein	victoria
annual_sale	secret_model_calvin	pink_victorias
semi_annual_sale	victorias	pink_victorias_secret
amp	victorias_secret	poshmark_pink
secret_semi	swiiqtuov	closet_poshmark_pink
victorias_secret_fashion	mm	poshmark_victoria
secret_semi_annual	body	poshmark_victoria_secret
bras	amp	closet_poshmark_victoria
size	underwear	poshmark_pink_victorias
airpods	bath	secret_pink
cover	im	bra
set	dont	victorias_secret_pink
The above picture shows the top 30 words for each of the 3 topics. The words are in ascending order of phi-value, namely the probability of a word belonging to a topic, given the topic, or Pr(word|topic). The higher the ranking, the more probable the word will belong to the topic. Before we try to make sense out of each topic, from a mere visual inspection, it seems like some of the topics (e.g., Topic 2 and 3) are kind of overlapping with each other. In such a case, you can either use your judgment and experience to think if we should combine the different topics together. Or, we can run a Dendogram to see which topics should be grouped together. A Dendogram uses Hellinger distance (i.e., distance between 2 probability vectors) to decide if the topics are closely related. For instance, the Dendogram below suggests that there is indeed greater similarity between topic 2 and 3. In our case, we only have 3 topics, hence we should be less worried about the issue of topic overlapping. When you work with a large number of topics (e.g., larger than 5), you might want to pay attention to the Dendrogram. We can keep our three topics as they are, for now.

model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)


Looking at the top 30 words involved in each topic we can roughly summarize that:

Topic 1 (i.e., t_1) is the most distinct from others. You must have noticed that topic 1 contains quite a few “weird” or unexpected combinations between the VS brand and Calvin Klein, and the word “expect”. A curious analyst could either go back and investigate the tweets in the dataset, or search on Twitter by typing in the keywords such as “Victoria’s Secret Calvin Klein”. I am sure you will end up seeing a lot of Tweets like the ones in the below screenshots from Twitter:
Example of Tweets belonging to Topic 1 It turns out that the public were actually blaming the VS brand, for its body shaming, i.e., the pressure that the brand, through the super models, sets on females, and the inequality in terms of expectations on body appearance between males and females. Interesting!

Topic 2 and 4 seems to be about sales-related information, since we observe words like “sales”, “annual sales”, “semi-annual” quite often. These topics seem to be about price promotion of the brand.
Topic 3 seems to be more more about the advertising or the collaboration of the VS brand via an online fashion merchandising platform named “Poshmark”, which allows people to buy, sell, and discover fashion products. Specifically, we can see that the collaboration or advertising efforts seem to be focused on the PINK sub-line instead of the mother brand. Meanwhile, it could also be that consumers or influences are sharing their purchases via Twitter, since we see words like “add_to_closet”.
4. Result Visualisation of Topic Modeling
We can create word cloud to see the words belonging to the certain topic, based on the probability. (Note that if you are not able to view the wordcloud output in your RMD file, you can copy and paste the codes into your console and click Enter to run.)

#visualizing topics of words based on the max value of phi

set.seed(1234)

final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)
final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename( c("value" = "word")) %>% select(-variable)
#final_summary_words <- left_join(final_summary_words,allterms)
final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
  arrange(desc(word))
final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))


for(i in 1:length(unique(word_topic_freq$topic)))
{  wordcloud(words = subset(word_topic_freq ,topic == i)$word, freq = subset(word_topic_freq ,topic == i)$term_freq, min.freq = 1,  max.words=200, random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))}


5. Conclusions
From the above analyses, we find that Victoria’s Secret, probably together with other brands in the same industry with a similar marketing program and brand positioning, is now faced with an increasingly important challenge from the general public regarding gender discrimination, body shame, and female self-esteem, to name a few. Through topic modeling results, it is very clear that such topic has been heated in the social media, making it urgent for the brand to react to.

Recently, we have seen Victoria’s Secret’s efforts to rebrand, for example, initiating the campaign of “what women want” in 2021. Specifically, the brand is trying to “redefine ‘sexy’”, by focusing on “women known for their accomplishments, not their appearance”. According to news (https://www.forbes.com/sites/kimelsesser/2021/06/17/in-new-rebrand-victorias-secret-focuses-on-whats-sexy-for-women/?sh=357e53210440), celebrities signed on the campaign include soccer star Megan Rapinoe who is fighting for equal pay for female athletes; Eileen Gu, a 17-year-old Chinese American freestyle skier; actor, producer and author Priyanka Chopra Jonas; and Paloma Elsesser, a plus-size model who hopes to inspire Victoria’s Secret to carry larger sizes. Had the brand done social listening via topic modeling techniques, it would be portrayed as a pioneer fighting for gender inequality and redefining the industry (hence receiving a lot of compliments and support and purchases from the consumers) instead of being “forced” to react to the pressure from public opinions.

Example of VS Rebranding Effort Example of VS Rebranding Effort

Interestingly, another valuable insight that can be derived from the above analyses is that the brand Calvin Klein may be also accused of very similar issues by the consumers, since the name of the brand frequently appear in Topic 4. Calvin Klein definitely needs to be alerted about such trend in consumer sentiment, and preferably design effective marketing campaigns that showcase their recognition of issues like gender inequality and their serious efforts to rebrand.
