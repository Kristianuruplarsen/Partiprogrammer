# this file runs scatterplots of the type
# party A word frequency vs. party B word frequency
# for all words in the sample
#

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



