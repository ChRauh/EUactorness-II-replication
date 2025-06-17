#####################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Embedd UNGD speeches and compare to semantic poles
#           (economy, security, liberal democracy)
# Author:   @ChRauh (June 11, 2025)
#####################################################################


# Measure run time
start <- Sys.time()


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(tidytext) # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools CRAN v0.4.2
library(scales) # Scale Functions for Visualization CRAN v1.4.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1
library(patchwork) # The Composer of Plots CRAN v1.3.0
library(xlsx) # Read, Write, Format Excel 2007 and Excel 97/2000/XP/2003 Files CRAN v0.6.5


# Pre-trained word vector model ###

# Using the ready-made GLOVE word embeddings trained on Wikipedia and Gigaword
# Version with 400k vocabulary and 300 dimensions

# Get it here: https://nlp.stanford.edu/projects/glove/
# ! LARGE FILE !

# I have parsed this for other purposes already, if you start from the raw file see:
# https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9

glove.300 <- read_rds("./data/GloVe/glove.6B.300d.rds")

# This has the tokens in columns and the dimensions in rows
# For a vectorized version, it is easier (and much quicker) to work with a transposed version
# Which allows easier joining vectors to tokens
glove.300 <- glove.300 %>% 
  as.matrix() %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("token") %>% 
  relocate(token)


# Prepare UNGD sentences ####
sent <- read_rds("./data/UNGD/ungd_sentences.rds") %>% 
  select(doc_id, sentence_id, sentence)

# Sample for testing
# sent <- sent %>% sample_n(10, replace = F)


# Embedding - by token ####

# Tokenize the texts, store long form by token holding the doc_id (= sentence),
# and embed each token in the GLOVE vector space (this may take some time - prior test suggest ~ 45 mins)
start <- Sys.time()
token.vectors <- 
  sent %>%
  tidytext::unnest_tokens(token, # Name of target column holding the tokens
                          sentence,  # Name of input column holding the text strings
                          token = "words", # tokenize by ...
                          format = "text")  %>% # Output format of tokens
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  filter(str_detect(token, "[A-Za-z]")) %>% # Only words, drop punct and numbers
  left_join(glove.300, by = "token")
Sys.time()-start


# Keep full vocab for later
unga.vocab <- unique(token.vectors$token)


# Clean up
rm(glove.300)
gc()


# Average vector per sentence ####
# Also quite a memory intense task ...

start <- Sys.time()
sent.vectors.av <-
  token.vectors %>%
  select(-token) %>%
  group_by(doc_id, sentence_id) %>%
  summarise(across(.cols = everything(), mean, na.rm = T)) # Note: everything() excludes grouping var
Sys.time()-start


# Export
write_rds(sent.vectors.av, "./data/Sentence-GLOVE-vectors.rds")

# Clean up
rm(token.vectors)
gc()




#####################
# Semantic poles ####

# Re-load sentence vectors
sent.vectors.av <- read_rds("./data/Sentence-GLOVE-vectors.rds")

# Pre-trained word vector model - reload
glove.300 <- read_rds("./data/GloVe/glove.6B.300d.rds")


# Function to find nearest neighbours in word vectors model ####
# Taken from: https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9
# N.B.: Expects vectors with tokens in columns and dimensions in rows 

find_sim_wvs <- function(this_wv, all_wvs, top_n_res=40) {
  # this_wv will be a numeric vector; all_wvs will be a data.frame with words as columns and dimesions as rows
  require(text2vec)
  this_wv_mat <- matrix(this_wv, ncol=length(this_wv), nrow=1)
  all_wvs_mat <- as.matrix(all_wvs)
  
  if(dim(this_wv_mat)[[2]] != dim(all_wvs_mat)[[2]]) {
    print("switching dimensions on the all_wvs_matrix")
    all_wvs_mat <- t(all_wvs_mat)
  }
  
  cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
  sorted_cos_sim <- sort(cos_sim[,1], decreasing = T) 
  return(head(sorted_cos_sim, top_n_res))
  
}




# Target vectors ####

# Target concept - economy/trade 
economy.seeds <- c("trade",
                   "economy",
                   "market",
                   "commerce",
                   "business")

economy.vector <- glove.300 %>% 
  select(all_of(economy.seeds)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

economy.neighbours <-
  find_sim_wvs(economy.vector, glove.300, top_n_res = 1000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% economy.seeds) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  filter(token %in% unga.vocab)

write_rds(economy.neighbours %>% head(55), "./data/EconomyNeighbours.rds")
rm(economy.neighbours)

# Target concept - democracy 
democracy.seeds <- c("democracy",
                     "law",
                     "human",
                     "rights",
                     "freedom")

democracy.vector <- glove.300 %>% 
  select(all_of(democracy.seeds)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

democracy.neighbours <-
  find_sim_wvs(democracy.vector, glove.300, top_n_res = 1000) %>% # similarity to full vocab
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% democracy.seeds) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  filter(token %in% unga.vocab)

write_rds(democracy.neighbours %>% head(55), "./data/DemocracyNeighbours.rds")
rm(democracy.neighbours)

# Target concept - security
security.seeds <- c("security",
                    "war",
                    "terrorism",
                    "peace",
                    "military")

security.vector <- glove.300 %>% 
  select(all_of(security.seeds)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

security.neighbours <-
  find_sim_wvs(security.vector, glove.300, top_n_res = 1000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% security.seeds) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  filter(token %in% unga.vocab)

write_rds(security.neighbours %>% head(55), "./data/SecurityNeighbours.rds")
rm(security.neighbours)

rm(unga.vocab)
gc()




# Seed similarity function ####

# Cosine similarity of a set of word vectors
# to an average vector of seed tokens

seed.simil <- function(seed.tokens,    # Character vector of seed tokens
                       text.vectors,   # Character vectors of texts to be scored (docs in rows, dims in cols)
                       model) {        # Pre-trained word-vector model (tokens in cols, dims in rows)
  
  # Get average vector of seed tokens
  print("Extracting vector of seed tokens from pre-trained model.")
  target.vector <- model %>% 
    select(all_of(seed.tokens)) %>% # Probably needs a check whether all seeds exist in word vector
    rowMeans() # Aggregation by mean
  
  # Loop through text vectors,
  # compare each vector to target vector
  print("Calculating cosine similarity of each text vector to target vector.")
  simils <- numeric()
  require(lsa)
  progress_bar = txtProgressBar(min=0, max=nrow(text.vectors), style = 1, char="=")
  for (i in 1:nrow(text.vectors)) {
    simil <- cosine(as.numeric(text.vectors[i, ]),
                    target.vector) %>% as.numeric()
    simils <- c(simils, simil)
    setTxtProgressBar(progress_bar, value = i)
  }
  close(progress_bar)
  
  # Return vector of similarities
  # in order of text vectors
  return(simils)
}



# Extract sentence level similiarity 

# Some sentences get dropped in the embedding process above
# e.g. if they only contain stopwords, punctuation or numbers
# Harmonize this here
sent$sentence_id <- paste0(sent$doc_id, "_Sentence", sent$sentence_id) # Unique
sent.vectors.av$sentence_id <- paste0(sent.vectors.av$doc_id, "_Sentence", sent.vectors.av$sentence_id) # Unique

sent <- sent %>% 
  filter(sentence_id %in% sent.vectors.av$sentence_id)


# Ensure that text and vector object are ordered identically
sent <- sent %>% ungroup() %>% arrange(sentence_id)
sent.vectors.av <- sent.vectors.av %>% ungroup() %>% arrange(sentence_id)
gc()


# Measure similarities
start <- Sys.time()
sent$dem.simil <- seed.simil(democracy.seeds,
                               sent.vectors.av %>% select(starts_with("V")),
                               glove.300)
gc()
sent$econ.simil <- seed.simil(economy.seeds,
                                sent.vectors.av %>% select(starts_with("V")),
                                glove.300)
gc()
sent$sec.simil <- seed.simil(security.seeds,
                               sent.vectors.av %>% select(starts_with("V")),
                               glove.300)
gc()
Sys.time() - start


# Export results 
write_rds(sent %>% select(-sentence),
          "./data/SemanticSimils.rds")



# Describe token-level similarities for appendix ####
# Table 3 in Appendix A5

econ <- read_rds("./data/EconomyNeighbours.rds") %>% 
  arrange(desc(seed), desc(sim.target)) %>% 
  select(-seed) %>% 
  mutate(sim.target = round(sim.target, 2)) %>% 
  head(25)
dem <- read_rds("./data/DemocracyNeighbours.rds") %>% 
  arrange(desc(seed), desc(sim.target)) %>% 
  select(-seed) %>% 
  mutate(sim.target = round(sim.target, 2)) %>% 
  head(25)
sec <- read_rds("./data/SecurityNeighbours.rds")%>% 
  arrange(desc(seed), desc(sim.target)) %>% 
  select(-seed) %>% 
  mutate(sim.target = round(sim.target, 2)) %>% 
  head(25)

t <- cbind(econ, dem, sec)
write.xlsx(t, "./output/Appendix_Table3_SemanticSimilToTargetVectors_RAW.xlsx")




# Measure run time
duration <- Sys.time() - start
duration # Time difference of 8.1583378 hours
