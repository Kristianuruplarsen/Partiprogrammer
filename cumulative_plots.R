
# cumulative sentiment through the texts (x is letter number)
tokenized_parties %>%
  arrange(letter, linenumber) %>%
  group_by(letter) %>%
  mutate(letter_number = row_number()) %>%
  filter(!is.na(score)) %>%
  mutate(csum = ave(score, letter, FUN = cumsum)) %>%
  ggplot(aes(x = letter_number,y = csum, color = letter)) +
  geom_line(size = 1) +
  xlab("Ord #") +
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

