library(tidyverse)
library(plotly)
library(gridExtra)
library(lubridate)
library(hrbrthemes)
library(tidytext)
library(tm)
library(ggraph)
library(igraph)
library(textstem)
library(BTM)
library(spacyr)
spacy_initialize("en_core_web_sm")

#t <- read_csv('C:/Users/GLaDOS/Documents/Python Scripts/ellistweet_inp.csv')
t <- read_csv('C:/Users/GLaDOS/Documents/Python Scripts/phase1.csv')

t_c <- t%>%
  filter(Label == 'C')
t_s <- t%>%
  filter(Label == 'S')

compute_logodds <- function(c1,c2,a1=1,a2=1){
  c1_s = c1 + a1
  c2_s = c2 + a2
  d1 = log( c1_s / (sum(c1) + a1*length(c1)- c1_s ))
  d2 = log( c2_s / (sum(c2) + a2*length(c2)- c2_s ))
  d = d1 - d2
  sigma = (1./c1_s) + (1./c2_s)
  val = d/sqrt(sigma)
  return(data.frame(d=d,sigma=sigma,val=val))
}


tidycrit <- spacy_parse(t_c$renderedContent)

tidycrit <- tidycrit%>%
  filter(pos != 'PUNCT' & pos != 'SPACE')

tidycrit <- tidycrit%>%
  select(lemma)%>%
  rename(word = lemma)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)


tidycrit <- tidycrit%>%
  rename(critcount = n)


tidysup <- spacy_parse(t_s$renderedContent)

tidysup <- tidysup%>%
  filter(pos != 'PUNCT' & pos != 'SPACE')

tidysup <- tidysup%>%
  select(lemma)%>%
  rename(word = lemma)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)

tidysup <- tidysup%>%
  rename(supcount = n)

overall <- tidycrit%>%
  full_join(tidysup)

replace_reg <- "@[A-Za-z_\\d]+"
overall <- overall %>%
  mutate(word = str_replace_all(word, replace_reg, ""))

replace_reg <- "twitter.com/[A-Za-z_\\d]+"
overall <- overall %>%
  mutate(word = str_replace_all(word, replace_reg, ""))

overall <- overall%>%
  replace(is.na(.), 0)

overall <- overall%>%
  filter(word != '')

# overall <- overall%>%
#   anti_join(stop_words)

scores <- (compute_logodds(overall$critcount, overall$supcount))


scores$word <- overall$word

scores <- scores%>%
  select(word, val, d, sigma)

scores <- scores%>%
  arrange(desc(val))

sup_words_plot <- ggplot(data = subset(top_n(scores,30,-val)), aes(x = reorder(word,-val), y = val)) + 
  geom_col() + 
  coord_flip() +
  labs(title = "Popular with Ellis-supportive")+
  theme(text = element_text(size=20)) +
  xlab('words') +
  ylab('score')

crit_words_plot <- ggplot(data = subset(top_n(scores,40,val)), aes(x = reorder(word,val), y = val)) + 
  geom_col() + 
  coord_flip() +
  labs(title = "Popular with Ellis-critical")+
  theme(text = element_text(size=20)) +
  xlab('Hashtags') +
  ylab('score')

grid.arrange(sup_words_plot, crit_words_plot, ncol = 2, nrow = 1)


tidycrit_raw <- as_tibble(spacy_extract_nounphrases(t_c$renderedContent))

tidycrit <- tidycrit_raw%>%
  filter(length >= 2)%>%
  rename(word = text)%>%
  count(word)%>%
  rename(critcount = n)

tidysup_raw <- as_tibble(spacy_extract_nounphrases(t_s$renderedContent))

tidysup <- tidysup_raw%>%
  filter(length >= 2)%>%
  rename(word = text)%>%
  count(word)%>%
  rename(supcount = n)

overall <- tidycrit%>%
  full_join(tidysup)


overall <- overall%>%
  replace(is.na(.), 0)




# overall <- overall%>%
#   filter(word != ' ' | word != '')

# overall <- overall%>%
#   anti_join(stop_words)

scores <- (compute_logodds(overall$critcount, overall$supcount))


scores$word <- overall$word

scores <- scores%>%
  select(word, val, d, sigma)

scores <- scores%>%
  arrange(desc(val))

sup_words_plot <- ggplot(data = subset(top_n(scores,20,-val)), aes(x = reorder(word,-val), y = val)) + 
  geom_col() + 
  coord_flip() +
  labs(title = "Popular with Ellis-supportive")+
  theme(text = element_text(size=20)) +
  xlab('words') +
  ylab('score')

crit_words_plot <- ggplot(data = subset(top_n(scores,30,val)), aes(x = reorder(word,val), y = val)) + 
  geom_col() + 
  coord_flip() +
  labs(title = "Popular with Ellis-critical")+
  theme(text = element_text(size=20)) +
  xlab('Hashtags') +
  ylab('score')

grid.arrange(sup_words_plot, crit_words_plot, ncol = 2, nrow = 1)

t_s%>%
  filter(likeCount>9)%>%
  arrange(desc(likeCount))%>%
  select(date, renderedContent)

tst <- t%>%
  count(date, renderedContent)%>%
  select(date, n)

tst$date <- as_datetime(tst$date)

ggplot(t, aes(date)+
  geom_freqpoly(binwidth = 3600)

  
p <- t%>%
  select(date, Label)%>%
  ggplot(aes(date))+
  geom_freqpoly(binwidth = 3600, size = 2, color="#69b3a2")+
  annotate(geom = 'text', x = as_datetime("2021-03-25T02:17:09+00:00"), y = 20, label = 'Raya Tweets', size = 6)+
  annotate(geom = 'point',x = as_datetime("2021-03-26T02:17:09+00:00"), y = 0, size=10, shape=21, fill="transparent" )+
  annotate(geom = 'point',x = as_datetime("2021-03-26T18:03:52+00:00"), y = 500, size=10, shape=21, fill="transparent" )+
  annotate(geom = 'text', x = as_datetime("2021-03-26T18:03:52+00:00"), y = 530, label = 'Clarification Thread', size = 6)+
  annotate(geom = 'point',x = as_datetime("2021-03-27T01:52:57+00:00"), y = 90, size=8, shape=21, fill="transparent" )+
  annotate(geom = 'text', x = as_datetime("2021-03-31T11:52:57+00:00"), y = 90, label = 'Ellis deactivates her acccount', size=6)+
  annotate(geom = 'point',x = as_datetime("2021-04-15T16:06:35+00:00"), y = 0, size=8, shape=21, fill="transparent" )+
  annotate(geom = 'text', x = as_datetime("2021-04-10T16:06:35+00:00"), y = 50, label = '"Mask Off" and return to Twitter', size=6)+
  ylab("No of tweets")+
  ggtitle('Overall Hourly Frequency Of Tweet')+
  theme_ipsum()

ggplotly(p)

p1 <- t%>%
  select(date, Label)%>%
  ggplot(aes(date, color =  Label))+
  geom_freqpoly(binwidth = 300, size = 1.1, alpha = 0.7, position = "identity")+
  ggtitle('Phase 1 - Frequency of tweets every 5 minutes')+
  theme_ipsum()

ggplotly(p1, tooltip = c("all"))

as_tibble(tidycrit)%>%
  filter(token == '@thelindsayellis')%>%
  select(token)%>%
  count(token, sort = TRUE)

api_create(p1, filename = 'p1_labs', sharing = 'public')
Sys.setenv('plotly_api_key'='ywQsWaqOGPe5oIfR20W2')
