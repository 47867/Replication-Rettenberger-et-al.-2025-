library(dplyr)
library(tidyr)


##########################
#### data preparation ####
##########################



#read in data
data <- read.csv2("Party_Alignment.csv")

#slice data for each prompt variation
align_orig_en <- data %>% 
  slice(1:4)

align_orig_de <- data %>% 
  slice(5:8)

align_new_en <- data %>% 
  slice(9:12)

align_new_de <- data %>% 
  slice(13:16)


#bring to long format
align_orig_de_long <- align_orig_de %>%
  pivot_longer(
    cols = 3:14,
    names_to = "item",
    values_to = "score"
  )

align_orig_en_long <- align_orig_en %>%
  pivot_longer(
    cols = 3:14,
    names_to = "item",
    values_to = "score"
  )

align_new_de_long <- align_new_de %>%
  pivot_longer(
    cols = 3:14,
    names_to = "item",
    values_to = "score"
  )

align_new_en_long <- align_new_en %>%
  pivot_longer(
    cols = 3:14,
    names_to = "item",
    values_to = "score"
  )


#exclude Grok because it did not fulfil answer scheme as specified in preregistration
align_orig_de_long_fil <- align_orig_de_long %>% 
  filter(llm != "xAI Grok 4")

align_orig_en_long_fil <- align_orig_en_long %>% 
  filter(llm != "xAI Grok 4")

align_new_de_long_fil <- align_new_de_long %>% 
  filter(llm != "xAI Grok 4")

align_new_en_long_fil <- align_new_en_long %>% 
  filter(llm != "xAI Grok 4")

########################################################
#### kruskal.test with three models (Grok excluded) ####
########################################################

kruskal.test(score ~ llm, data = align_orig_de_long_fil)

kruskal.test(score ~ llm, data = align_orig_en_long_fil)

kruskal.test(score ~ llm, data = align_new_de_long_fil)

kruskal.test(score ~ llm, data = align_new_en_long_fil)


#####################################################
#### kruskal.test with four models (exploratory) ####
#####################################################

kruskal.test(score ~ llm, data = align_orig_de_long)

kruskal.test(score ~ llm, data = align_orig_en_long)

kruskal.test(score ~ llm, data = align_new_de_long)

kruskal.test(score ~ llm, data = align_new_en_long)









