
library("tidyverse")
library("pdftools")
library("tidytext")
library("magrittr")
library("zoo")
library("reshape2")


# read stopwords and sentiment

stopwords <- read.table("stopwords-da.txt", encoding = "UTF-8") %>%
  set_colnames("word") %>%
  mutate(word = as.character(word))


temp <- tokenized_parties %>%
  group_by(word) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  distinct(word, .keep_all = T)

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
                    "samt",
                    "uden",
                    "gøre",
                    "år",
                    "vigtigt",
                    "langt",
                    "hele")


stopwords <- rbind(stopwords, more_stopwords)

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
  filter(grepl("[:digit:]", word))


# cumulative sentiment through the texts (x is letter number)
tokenized_parties %>%
  arrange(letter, linenumber) %>%
  group_by(letter) %>%
  mutate(letter_number = row_number()) %>%
  filter(!is.na(score)) %>%
  mutate(csum = ave(score, letter, FUN = cumsum)) %>%
  ggplot(aes(x = letter_number,y = csum, color = letter)) +
  geom_line(size = 1) +
  xlab("Letter number") +
  ylab("Cumulative sentiment score") +
  ggtitle("Sentiment by political party") +
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


# cumulative sentiment through the texts (x is line number)
tokenized_parties %>%
  arrange(letter, linenumber) %>%
  group_by(letter) %>%
  filter(!is.na(score)) %>%
  mutate(csum = ave(score, letter, FUN = cumsum)) %>%
  ggplot(aes(x = linenumber,y = csum, color = letter)) +
  geom_line(size = 1) +
  xlab("Line number") +
  ylab("Cumulative sentiment score") +
  ggtitle("Sentiment by political party") +
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

# and the same(ish) with a rolling mean
tokenized_parties %>%
  arrange(letter, linenumber) %>%
  group_by(letter) %>%
  filter(!is.na(score)) %>%
  mutate(csum = ave(score, letter, FUN = function(x) rollmean(x,k = 50))) %>%
  ggplot(aes(x = linenumber,y = csum, color = letter)) +
  geom_line(size = 1) +
  xlab("Line number") +
  ylab("Average sentiment score over 50 lines") +
  ggtitle("Rolling sentiment by political party") +
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
                                "Enhedslisten" = ehl)) +
  facet_wrap(~ letter)


# how often does which party use which word
aggrement_words <- tokenized_parties %>%
  #lets first count number of occurences in each document and generate proportion
  group_by(letter, word) %>%
  mutate(n = n()) %>%
  ungroup() %>% group_by(letter) %>%
  mutate(proportion = n/sum(n)*100) %>%
  # then drop the duplicates within parties
  distinct(letter, word, .keep_all = TRUE) %>%
  subset(select = c(letter, word, proportion)) %>%
  spread(letter, proportion)


  

####
# common-word plots
#

aggrement_words %>%
  ggplot(aes(Konservative, `Liberal Alliance`, color = Konservative - `Liberal Alliance`)) +
  geom_jitter(alpha = 0.2,size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 3) +
  scale_color_gradient(low = lib, high = con) +
  xlab("Konservative (%)") +
  ylab("Liberal Alliance (%)") +
  ggtitle("Ord-frekvens (C, I)") +
  theme(legend.position = "none",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) 
  
ggsave("figs/CI.png")

# ehl SF
aggrement_words %>%
  ggplot(aes(Enhedslisten, SF, color = Enhedslisten - SF)) +
  geom_jitter(alpha = 0.2,size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_color_gradient(low = sof, high = ehl) +
  xlab("Enhedslisten (%)") +
  ylab("SF (%)") +
  ggtitle("Ord-frekvens (EHL, SF)") +
  theme(legend.position = "none",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) +
  ylim(c(0,0.3)) +
  xlim(c(0,0.3))
  


# venstre and soc
aggrement_words %>%
  ggplot(aes(Venstre, Socialdemokratiet, color = Venstre - Socialdemokratiet)) +
  geom_jitter(alpha = 0.2,size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 3) +
#  scale_color_viridis() +
  scale_color_gradient(low = soc, high = ven) +
  xlab("Venstre (%)") +
  ylab("Socialdemokratiet (%)") +
  ggtitle("Ord-frekvens (V, S)") +
  theme(legend.position = "none",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) 


ggsave("figs/SV.png")


# venstre and df
aggrement_words %>%
  ggplot(aes(Venstre, `Dansk Folkeparti`, color = Venstre - `Dansk Folkeparti`)) +
  geom_jitter(alpha = 0.2,size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
#  scale_color_viridis() +
  scale_color_gradient(low = df, high = ven) +
  xlab("Venstre (%)") +
  ylab("Dansk Folkeparti (%)") +
  ggtitle("Ord-frekvens (V, S)") +
  theme(legend.position = "none",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) +
  ylim(c(0,0.15)) +
  xlim(c(0,0.15))
  

# Soc and df
aggrement_words %>%
  ggplot(aes(Socialdemokratiet, `Dansk Folkeparti`, color = Socialdemokratiet - `Dansk Folkeparti`)) +
  geom_jitter(alpha = 0.2,size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
#  scale_color_viridis() +
  scale_color_gradient(low = df, high = soc) +
  xlab("Socialdemokratiet (%)") +
  ylab("Dansk Folkeparti (%)") +
  ggtitle("Ord-frekvens (V, S)") +
  theme(legend.position = "none",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) 





aggrement_words %>%
  ggplot(aes(Alternativet, Enhedslisten, color = Alternativet - Enhedslisten)) +
  geom_jitter(alpha = 0.3, size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 3) +
#  scale_color_viridis() +
  scale_color_gradient(low = ehl, high = alt) +
  xlab("Alternativet (%)") +
  ylab("Enhedslisten (%)") +
  ggtitle("Ord-frekvens (Å, Ø)") +
  theme(legend.position = "none",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) 

ggsave('figs/ÅØ.png')


