##################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Extraction of actorness motifs from dependency trees 
#           of all UNGD sentences
# Author:   @ChRauh (June 13, 2025)
##################################################################

# Measure run time
start <- Sys.time()

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(semgram) # Extracting Semantic Motifs from Textual Data CRAN v0.1.0
source("1_ExtractActorness.R") # Customized functions expanding Stuhler's (2022) class of action motifs 
library(rsyntax) # Extract Semantic Relations from Text by Querying and Reshaping Syntax CRAN v0.1.4
library(data.table) # Extension of `data.frame` CRAN v1.17.0


# Input: Dependency trees of all sentences from the UNGD corpus ####
parsed <- read_rds("./data/ungd_DependencyTrees.rds")


# Rule-based extraction of all actorness motifs ####
# Using the functions specified in input script sourced above

actions <- extract_actorness(tokens = parsed, # CR expanded semantic motif extraction
                             entities = "*", # Any entity accepted, note that IO and country names have been harmonized in pre-processing (0_DependencyParsing.R)
                             markup = T) %>% 
  mutate(doc_id = str_remove(doc_id, "text") %>% as.numeric()) %>% # row id in the original UNGD sentence-level data
  arrange(doc_id)
write_rds(actions, "./data/ungd_ActornessMotifs.rds")


# Cross-checks ###
sum(actions$Entity == "EU") # 2680
sum(actions$Entity == "US") # 8568
sum(actions$Entity == "RU") # 4508


# Measure run time
duration <- Sys.time() - start
duration # Time difference of 11.10642 hours


