
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
                    "både")

more_stopwords[more_stopwords %in% sentiments$word]

stopwords <- rbind(stopwords, more_stopwords)


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
  anti_join(stopwords) %>%
  left_join(sentiments) %>%
  filter(!grepl("[0-9]", word))


# cumulative sentiment through the texts (x is letter number)
tokenized_parties %>%
  arrange(letter, linenumber) %>%
  group_by(letter) %>%
  mutate(letter_number = row_number()) %>%
  filter(!is.na(score)) %>%
  mutate(csum = ave(score, letter, FUN = cumsum)) %>%
  ggplot(aes(x = letter_number,y = csum, color = letter)) +
  geom_line(size = 1) +
  xlab("Bogstav #") +
  ylab("Akkumuleret sentiment score") +
  ggtitle("Sentiment pr. politisk parti") +
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

ggsave("figs/csum_per_letter.png")



# # cumulative sentiment through the texts (x is line number)
# tokenized_parties %>%
#   arrange(letter, linenumber) %>%
#   group_by(letter) %>%
#   filter(!is.na(score)) %>%
#   mutate(csum = ave(score, letter, FUN = cumsum)) %>%
#   ggplot(aes(x = linenumber,y = csum, color = letter)) +
#   geom_line(size = 1) +
#   xlab("Line number") +
#   ylab("Cumulative sentiment score") +
#   ggtitle("Sentiment by political party") +
#   theme(legend.position = "bottom",
#           legend.title=element_blank(),
#           legend.text = element_text(size = 7),
#           legend.background = element_rect(fill=NA)) +
#   scale_color_manual(values = c("Alternativet" = alt,
#                                 "Dansk Folkeparti" = df,
#                                 "Konservative" = con,
#                                 "Liberal Alliance" = lib,
#                                 "SF" = sof,
#                                 "Socialdemokratiet" = soc,
#                                 "Venstre" = ven,
#                                 "Radikale" = rad,
#                                 "Enhedslisten" = ehl))
# 
# # and the same(ish) with a rolling mean
# tokenized_parties %>%
#   arrange(letter, linenumber) %>%
#   group_by(letter) %>%
#   filter(!is.na(score)) %>%
#   mutate(csum = ave(score, letter, FUN = function(x) rollmean(x,k = 50))) %>%
#   ggplot(aes(x = linenumber,y = csum, color = letter)) +
#   geom_line(size = 1) +
#   xlab("Line number") +
#   ylab("Average sentiment score over 50 lines") +
#   ggtitle("Rolling sentiment by political party") +
#   theme(legend.position = "bottom",
#           legend.title=element_blank(),
#           legend.text = element_text(size = 7),
#           legend.background = element_rect(fill=NA)) +
#   scale_color_manual(values = c("Alternativet" = alt,
#                                 "Dansk Folkeparti" = df,
#                                 "Konservative" = con,
#                                 "Liberal Alliance" = lib,
#                                 "SF" = sof,
#                                 "Socialdemokratiet" = soc,
#                                 "Venstre" = ven,
#                                 "Radikale" = rad,
#                                 "Enhedslisten" = ehl)) +
#   facet_wrap(~ letter)





# how often does which party use which word
counted_words <- tokenized_parties %>%
  #lets first count number of occurences in each document and generate proportion
  group_by(letter, word) %>%
  mutate(n = n()) %>%
  ungroup() %>% group_by(letter) %>%
  mutate(proportion = n/sum(n)) %>%
  # then drop the duplicates within parties
  distinct(letter, word, .keep_all = TRUE) 


aggrement_words <- counted_words %>%
  subset(select = c(letter, word, proportion)) 



####
# common-word plots
#


# Konservative
aggrement_words %>%
  spread(letter, proportion) %>%
  ggplot(aes(Konservative, `Liberal Alliance`, color = Konservative - `Liberal Alliance`)) +
    geom_jitter(alpha = 0.2,size = 2.5, width = 0.3, height = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 3) +
    scale_color_gradient(low = lib, high = con) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    xlab("Konservative") +
    ylab("Liberal Alliance") +
    ggtitle("Ord-frekvens, log-log skala (C, I)") +
    theme_bw() +
    theme(legend.position = "none",
            legend.title=element_blank(),
            legend.text = element_text(size = 7),
            legend.background = element_rect(fill=NA)) 
  
ggsave("figs/CI.png")

# ehl og liberal alliance
aggrement_words %>%
  spread(letter, proportion) %>%
  ggplot(aes(Enhedslisten, `Liberal Alliance`, color = Enhedslisten - `Liberal Alliance`)) +
    geom_jitter(alpha = 0.2,size = 2.5, width = 0.3, height = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 3) +
    scale_color_gradient(low = lib, high = ehl) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    xlab("Enhedslisten") +
    ylab("Liberal Alliance") +
    ggtitle("Ord-frekvens, log-log skala (Ø, I)") +
    theme_bw() +
    theme(legend.position = "none",
            legend.title=element_blank(),
            legend.text = element_text(size = 7),
            legend.background = element_rect(fill=NA))

ggsave("figs/ØI.png")

# venstre and soc
aggrement_words %>%
  spread(letter, proportion) %>%
  ggplot(aes(Venstre, Socialdemokratiet, color = Venstre - Socialdemokratiet)) +
    geom_jitter(alpha = 0.2,size = 2.5, width = 0.3, height = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 3) +
  #  scale_color_viridis() +
    scale_color_gradient(low = soc, high = ven) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
    xlab("Venstre") +
    ylab("Socialdemokratiet") +
    ggtitle("Ord-frekvens, log-log skala (V, S)") +
    theme_bw() +
    theme(legend.position = "none",
            legend.title=element_blank(),
            legend.text = element_text(size = 7),
            legend.background = element_rect(fill=NA)) 
  

ggsave("figs/SV.png")


# alternativet and enhedslisten
aggrement_words %>%
  spread(letter, proportion) %>%
  ggplot(aes(Alternativet, Enhedslisten, color = Alternativet - Enhedslisten)) +
  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 3) +
#  scale_color_viridis() +
  scale_color_gradient(low = ehl, high = alt) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
  xlab("Alternativet") +
  ylab("Enhedslisten") +
  ggtitle("Ord-frekvens, log-log skala (Å, Ø)") +
  theme_bw() +
  theme(legend.position = "none",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) 

ggsave('figs/ÅØ.png')




##########################################################################################
# various, zipfs law and 
#
#

# counted_words %>%
#   group_by(letter) %>%
#   arrange(desc(n), .by_group = TRUE) %>%
#   mutate(rank = row_number()) %>%
#   ggplot(aes(x = rank, y = proportion, color = letter)) +
#     geom_line(size = 1) +
#     scale_x_log10() +
#     scale_y_log10() +
#     xlab("Word rank (1 = most used word)") +
#     ylab("Word frequency") +
#     ggtitle("Zipf's law in political manifests") +
#     theme(legend.position = "bottom",
#             legend.title=element_blank(),
#             legend.text = element_text(size = 7),
#             legend.background = element_rect(fill=NA)) +
#     scale_color_manual(values = c("Alternativet" = alt,
#                                   "Dansk Folkeparti" = df,
#                                   "Konservative" = con,
#                                   "Liberal Alliance" = lib,
#                                   "SF" = sof,
#                                   "Socialdemokratiet" = soc,
#                                   "Venstre" = ven,
#                                   "Radikale" = rad,
#                                   "Enhedslisten" = ehl))

# td-idf plot
#
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





# topicmodelling

# these words are of no use in classifying n topics as they represent the parties themselves
# and/or should be in the original stoplist
non_words <- c("danmark",
               "dansk",
               "folkeparti",
               "danske",
               "bør",
               "alliance",
               "konservative",
               "alternativet",
               "sf",
               "principprogram",
               "gennem",
               "enhedslisten",
               "radikale",
               "mener",
               "således",
               "dag",
               "ønsker",
               "både",
               "udkast",
               "mest",
               "land",
               "del",
               "år",
               "forslag")


# convert data to dtm format
termmatrix <- tokenized_parties %>%
  group_by(letter, word) %>%
  filter(!(word %in% non_words)) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  cast_dtm(letter, word, n)


# fit model
lda_model <- LDA(termmatrix, k = 2, control = list(seed = 1234))

# # tidy to get beta parameter
# tidy_lda <- tidy(lda_model, matrix = "beta")
# 
# # plot it
# tidy_lda %>%
#   group_by(topic) %>%
#   top_n(15, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta) %>%
#   mutate(term = reorder(term, beta)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()
# 
# # tidy to get gamma
# tidy_lda_gamma <- tidy(lda_model, matrix = "gamma")
# 
# #plot by party
# ggplot(data = tidy_lda_gamma) +
#   geom_col(aes(x = topic, y = gamma+1, fill = as.factor(topic))) +
#   scale_y_log10() +
#   scale_fill_brewer() +
#   facet_wrap( ~document)



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