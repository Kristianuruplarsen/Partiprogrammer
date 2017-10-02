
library("tidyverse")
library("pdftools")
library("tidytext")
library("magrittr")
library("zoo")
library("reshape2")
library("scales")
library("ggpage")
library("topicmodels")

# read stopwords and sentiment

stopwords <- read.table("stopwords-da.txt", encoding = "UTF-8") %>%
  set_colnames("word") %>%
  mutate(word = as.character(word))


more_stopwords <- c("derfor",
                    "ligeså",
                    "afgå",
                    "måde",
                    "nok",
                    "egentlig",
                    "bør",
                    "burde",
                    "mellem",
                    "både",
                    "gennem",
                    "forslag",
                    "muligt",
                    "nye",
                    "bl.a.",
                    "dag",
                    "altid",
                    "del",
                    "mener",
                    "både",
                    "samt",
                    "uden",
                    "gøre",
                    "år",
                    "vigtigt",
                    "langt",
                    "hele",
                    "mest",
                    "både",
                    "give",
                    "gennem",
                    "dag",
                    "både",
                    "bør",
                    "gennem",
                    "mener",
                    "således",
                    "dag",
                    "ønsker",
                    "både",
                    "udkast",
                    "mest",
                    "land",
                    "gør",
                    "nye",
                    "del",
                    "år",
                    "forslag")


stopwords <- as.data.frame(c(stopwords$word, more_stopwords)) %>% 
  set_colnames(c("word"))


# read sentiments from AFINN
sentiments <- read.table("AFINN-da-32.txt", sep ="\t") %>%
  set_colnames(c("word","score"))


# party colors 
ehl <- "#a30117"
sof <- "#f71836"
df <- "#f4df53"
alt <- "#29f43a"
con <- "#106d18"
soc <- "#ff790c"
lib <- "#18c6f2"
ven <- "#1851a5"
rad <- "#de5df4"

# load in all the pdf files
source("read_pdf.R")

# simply bind them all together in one dataframe

all_parties <- rbind(alternativet,
                     sf,
                     socialdemokratiet,
                     venstre,
                     liberal_alliance,
                     konservative,
                     dansk_folkeparti,
                     radikale,
                     enhedslisten
                     )

# remove the individual parties
rm(alternativet,
   sf,
   socialdemokratiet,
   venstre,
   liberal_alliance,
   konservative,
   dansk_folkeparti,
   radikale,
   enhedslisten)  


# expand to get each word on its own
tokenized_parties <- all_parties %>%
  # first let's add a linenumber for each word
  group_by(letter) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  # then unnest_tokens
  unnest_tokens(word, text) %>%
  # then remove stopwords and add sentiment score where possible
  anti_join(stopwords, by = "word") %>%
  left_join(sentiments, by = "word") %>%
  filter(!grepl("[0-9]", word))


source("cumulative_plots.R", encoding = "utf-8")


# how often does which party use which word
counted_words <- tokenized_parties %>%
  #lets first count number of occurences in each document and generate proportion
  group_by(letter, word) %>%
  mutate(n = n()) %>%
  ungroup() %>% group_by(letter) %>%
  mutate(proportion = n/sum(n)) %>%
  # then drop the duplicates within parties
  distinct(letter, word, .keep_all = TRUE) 


source("party_freq_crossplots.R", encoding = "utf-8")


tokenized_parties %>%
  group_by(letter, word) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  bind_tf_idf(word, letter, n) %>%
  ggplot(aes(x = tf, y = tf_idf,color = letter)) +
  geom_jitter(alpha = 0.3) +
  xlab("Ord-frekvens i tekst") +
  ylab("Invers ord-frekvens imellem dokumenter") +
  labs(title = "Ordbrug pr. parti, vs. i alle dokumenter") +
  geom_text(aes(label = word), check_overlap = TRUE, size = 3) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title=element_blank(),
            legend.text = element_text(size = 7),
            legend.background = element_rect(fill=NA)) +
      scale_color_manual(values = c("Alternativet" = alt,
                                  "Dansk Folkeparti" = df,
                                  "Konservative" = con,
                                  "Liberal Alliance" = lib,
                                  "SF" = sof,
                                  "Socialdemokratiet" = soc,
                                  "Venstre" = ven,
                                  "Radikale" = rad,
                                  "Enhedslisten" = ehl))


ggsave("figs/tf_idf.png")



# then plot the full text with ggpage, colored by its topic
# this is essentially ripped from the readme for 
# EmilHvitfeldt/ggpage - go check it out!

party = "Radikale"

all_parties %>% 
  filter(letter == party) %>%
  ggpage_build() %>%
  left_join(sentiments, by = "word") %>% 
          mutate(score = ifelse(is.na(score), 0, score), 
                     score_smooth = zoo::rollmean(score, 50, 0),
                     score_smooth = score_smooth / max(score_smooth),
                     rolls = 50) %>%
  ggpage_plot(aes(fill = score_smooth)) +
    scale_fill_gradient2(low = "red", high = "green", mid = "grey", midpoint = 0) +
    guides(fill = "none") +
    labs(title = paste("Smoothed sentiments score:", party)) 

ggsave("figs/avg_sent_page.png")


#################################################################################
# stemmed words from here
#
#


# stem words for future analysis
tokenized_stems <- tokenized_parties %>%
  mutate(stem = wordStem(word, language = "danish")) %>%
  group_by(letter, stem) %>%
  mutate(n = n()) %>%
  ungroup()


library("ggraph")
library("igraph")
library("widyr")

tokenized_stems %>%
  pairwise_cor(letter, stem, n,sort = TRUE) %>%
  filter(correlation > 0.2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(alpha = correlation),color = "black", width = 2) +
  geom_node_point(size = 4, color = "black") +
  geom_node_text(aes(label = name), color = "blue", repel = TRUE) +
  theme_void()

    
# convert data to dtm format
termmatrix <- tokenized_stems %>%
  group_by(letter, stem) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  cast_dtm(letter, stem, n)


# fit model
lda_model <- LDA(termmatrix, k = 6)

# tidy to get beta parameter
tidy_lda <- tidy(lda_model, matrix = "beta")

# plot it
tidy_lda %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# tidy to get gamma
tidy_lda_gamma <- tidy(lda_model, matrix = "gamma")

#plot by party
ggplot(data = tidy_lda_gamma) +
  geom_col(aes(x = topic, y = gamma+1, fill = as.factor(topic))) +
  scale_y_log10() +
  facet_wrap( ~document)



