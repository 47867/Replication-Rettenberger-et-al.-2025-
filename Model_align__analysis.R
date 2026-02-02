library(dplyr)
library(tidyr)

setwd("[insert your path here]")


##########################
#### data preparation ####
##########################



#read in data
data <- read.csv2("Daten_LLMs_Versand_final.csv")

#slice data for each prompt
answers_orig_de <- data %>% 
  slice(1:4)

answers_orig_en <- data %>% 
  slice(5:8)

answers_new_de <- data %>% 
  slice(9:12)

answers_new_en <- data %>% 
  slice(13:16)

#bring to long format
answers_orig_de_long <- answers_orig_de %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "item",
    values_to = "score"
  )

answers_orig_en_long <- answers_orig_en %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "item",
    values_to = "score"
  )

answers_new_de_long <- answers_new_de %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "item",
    values_to = "score"
  )

answers_new_en_long <- answers_new_en %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "item",
    values_to = "score"
  )

#mutate score to integers

answers_orig_de_long <- answers_orig_de_long %>%
  mutate(
    score_num = case_when(
      score == "nein"      ~ 0,
      score == "neutral" ~ 1,
      score == "ja"     ~ 2,
    )
  )

answers_orig_en_long <- answers_orig_en_long %>%
  mutate(
    score_num = case_when(
      score == "no"      ~ 0,
      score == "neutral" ~ 1,
      score == "yes"     ~ 2,
    )
  )

answers_new_de_long <- answers_new_de_long %>%
  mutate(
    score_num = case_when(
      score == "stimme nicht zu"      ~ 0,
      score == "neutral" ~ 1,
      score == "stimme zu"     ~ 2,
    )
  )

answers_new_en_long <- answers_new_en_long %>%
  mutate(
    score_num = case_when(
      score == "disagree"      ~ 0,
      score == "neutral" ~ 1,
      score == "agree"     ~ 2,
    )
  )


#exclude Grok because it did not fulfil answer scheme as specified in preregistration
answers_orig_de_long_fil <- answers_orig_de_long %>% 
  filter(llm != "xAI Grok 4")

answers_orig_en_long_fil <- answers_orig_en_long %>% 
  filter(llm != "xAI Grok 4")

answers_new_de_long_fil <- answers_new_de_long %>% 
  filter(llm != "xAI Grok 4")

answers_new_en_long_fil <- answers_new_en_long %>% 
  filter(llm != "xAI Grok 4")


########################################################
#### kruskal.test with three models (Grok excluded) ####
########################################################

kruskal.test(score_num ~ llm, data = answers_orig_de_long_fil)

kruskal.test(score_num ~ llm, data = answers_orig_en_long_fil)

kruskal.test(score_num ~ llm, data = answers_new_de_long_fil)

kruskal.test(score_num ~ llm, data = answers_new_en_long_fil)


######################################################
#### kruskal.test with four models - exploratory! ####
######################################################

kruskal.test(score_num ~ llm, data = answers_orig_de_long)

kruskal.test(score_num ~ llm, data = answers_orig_en_long)

kruskal.test(score_num ~ llm, data = answers_new_de_long)

kruskal.test(score_num ~ llm, data = answers_new_en_long)


