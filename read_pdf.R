
# libraries loaded in main file
#
# read party programs from downloaded pdf files
# note: we're missing enhedslisten and radikale venstre b.c. they
# dont supply a pdf version of their programs


  enhedslisten <- data_frame(pdf_text("pdf/enhedslisten.pdf")) %>%
    mutate(party = "Enhedslisten") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
  alternativet <- data_frame(pdf_text("pdf/alternativet.pdf")) %>%
    mutate(party = "Alternativet") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
  sf <- data_frame(pdf_text("pdf/SF.pdf")) %>%
    mutate(party = "SF") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
  socialdemokratiet <- data_frame(pdf_text("pdf/socialdemokratiet.pdf")) %>%
    mutate(party = "Socialdemokratiet") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
  radikale <- data_frame(pdf_text("pdf/radikale.pdf")) %>%
    mutate(party = "Radikale") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)

  
  venstre <- data_frame(pdf_text("pdf/venstre.pdf")) %>%
    mutate(party = "Venstre") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
  liberal_alliance <- data_frame(pdf_text("pdf/liberal_alliance.pdf")) %>%
    mutate(party = "Liberal Alliance") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
  konservative <- data_frame(pdf_text("pdf/konservative.pdf")) %>%
    mutate(party = "Konservative") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
  dansk_folkeparti <- data_frame(pdf_text("pdf/dansk_folkeparti.pdf")) %>%
    mutate(party = "Dansk Folkeparti") %>%
    set_colnames(c("text","letter")) %>%
    mutate(text = strsplit(text, "\r\n")) %>%
    unnest(text)
  
 