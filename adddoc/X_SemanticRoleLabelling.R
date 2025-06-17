##################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Semantic role labeling of UNGD sentences
# Author:   @ChRauh (June 11, 2025)
##################################################################


# Context:
# SRL approaches (probabilistic, semi-supervised machine learning) might offer an alternative approach to identify actorness in texts (along the semantic role of an agent)
# This measure is only used as a benchmark for my rule-based extraction of actorness motifs from dependency trees, 
# In prior tests I have extracted all SRL solutions for each sentence in UNGD speeches already and document this here
# N.B.: Requires a respectively setup Python environment on your machine and runs several hours ...
# for documentation I also include the resulting data in the replication archive ("./data/UNGD/ungd_sentences_srl.rds")

# Measure run time
start <- Sys.time()

# Packages #####
library(reticulate) # Interface to 'Python' CRAN v1.42.0
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0


# Choose python build to use here
# I am using a 3.7 version, in which realtio (Version Feb 7 2022) and all dependecies (!) are installed
# along the instructions at https://github.com/relatio-nlp/relatio
use_python("C:/Users/chris/AppData/Local/Programs/Python/Python37/python.exe", required = TRUE)

# Load python shell
# (also check which one its is)
# repl_python()

# The UNGD sentences
ungd <- read_rds("./Data/ungd_sentences.rds")
# ungd <- ungd[sample(1:nrow(ungd), 100, replace = F), ] # For testing


# Get text data into python environment
py$text <- r_to_py(ungd$sentence, convert = T)

# text <- py$text # Get python object back into R


# Semantic role labelling
# with relatio in python
py_run_string("from relatio.wrappers import run_srl
srl_res = run_srl(
    path=\"https://storage.googleapis.com/allennlp-public-models/openie-model.2020.03.26.tar.gz\", # pre-trained model
    sentences=text,
    progress_bar=True,
)")

# Get srl results back into r
srl <- py$srl_res

# Export for later usage
# write_rds(srl, "./data/ungd_sentences_srl_list.rds")




# Extract SRL triplets ####
# Long run time!

list.length <- length(srl)

triplets <- data.frame()

for (i in 1:list.length) {
  
  print(i)
  print(paste0(round(print((i/list.length)*100), 2), "%"))
  
  # There may be several verbs in each sentence verbs[[j]],
  # and each results in different semantic roles - so more triplets than sentences
  
  # Carry the sentence id through
  id <- i
  
  # Get number of verbs in sentence
  nverbs <- length(srl[[i]]$verbs)
  if(nverbs == 0) {next}
  
  # object to hold all triplets
  current <- character()
  
  # Loop over verbs
  for (j in 1:nverbs) {
    triplet <- srl[[i]]$verbs[[j]]$description
    current <- c(current, triplet)
  }
  
  # Assemble and append
  df <- data.frame(id = rep(id, length(current)),
                   raw = current,
                   text = paste(srl[[i]]$words, collapse = " "))
  triplets <- rbind(triplets, df)
}

# Separate the semantic roles in the resulting strings (brute force stringr)
triplets <- triplets %>% 
  mutate(agent = str_extract_all(raw, "\\[ARG0.*?\\]"),
         negation =str_extract_all(raw, "\\[ARGM-NEG.*?\\]"),
         verb = str_extract_all(raw, "\\[V.*?\\]"),
         patient = str_extract_all(raw, "\\[ARG1.*?\\]"))# Additional labels available!

triplets2 <- triplets %>% 
  mutate_at(vars(4, 5, 6, 7), ~str_remove_all(., "(\\[.*?:)|(\\])") %>% 
              str_trim()) %>% 
  select(-raw)

triplets2[triplets2 == "character(0)"] <- NA


# Export cleaned SRL solutions for later usage ####
write_rds(triplets2, "./data/UNGD/ungd_sentences_srl.rds")


# Measure run time
duration <- Sys.time() - start
duration # Time difference of 7.8245 hours
