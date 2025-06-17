##################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Coding specific agents in semantic action motifs
#           extracted from UNGD sentences
# Author:   @ChRauh (June 11, 2025)
##################################################################

# Measure run time
start <- Sys.time()

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(quanteda) # Quantitative Analysis of Textual Data CRAN v4.3.0
library(newsmap) # Semi-Supervised Model for Geographical Document Classification CRAN v0.9.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1



# Data ####

# UN sentences with meta data
ungd <- read_rds("./data/UNGD/ungd_sentences.rds") %>% 
  mutate(id = row_number()) %>% 
  select(-sentence_id)

# Semantic actorness motifs in UNGD sentences
# id matches row number in UN sentences data (uniquely identified sentences)
actions <- read_rds("./data/ungd_ActornessMotifs.rds") %>% 
  rename(id = doc_id)

# Add metadata and full sentence to semantic motifs
actions <- actions %>% 
  left_join(ungd, by = "id")


# Code country agents ####

# How often does a country actor appear as an agent in UNGD sentences?
# Note that I have harmonized country refernces to the ISO2 country codes from the newsmap package before motif extraction, we exploit this here

cdict <- data_dictionary_newsmap_en # From newsmap package

# For manual inspection of the terms
cdict.terms <- cdict %>% 
  as.list() %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rename(term = 1)
codes <- rownames(cdict.terms) %>% 
  as.data.frame() %>% 
  rename(code = 1)
codes2 <- str_split(codes$code, "\\.", simplify = T) %>% 
  as.data.frame() %>% 
  rename(region1 = V1,
         region2 = V2,
         countrycode = V3) %>% 
  mutate(countrycode = str_remove_all(countrycode, "[0-9]"))
cdict.terms <- cbind(cdict.terms, codes2)
rm(codes, codes2)

# The list of unique country codes
country.codes <- cdict.terms %>% select(countrycode) %>% unique() %>% pull() 

# Filter actions of which one of the countries was the acting entity
country.actions <- actions %>% filter(Entity %in% country.codes)

# Absolute frequencies
country.actions.n <- country.actions %>% 
  select(Entity) %>% 
  group_by(Entity) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
head(country.actions.n)

# Get a wide data frame of country agents per id
country.actions.wide <- 
  country.actions %>%
  select(c(id, Entity)) %>% 
  unique() %>% # NB only stores whether there was at least one action by COUNTRY in id (sentence), not whether there were several
  mutate(value = 1) %>% # Create a binary indicator for presence of each Entity
  pivot_wider(names_from = Entity, values_from = value, values_fill = 0) # Spread the Entity to wide format with values filled with 1 and absent values with 0


# Export country counts 
write_rds(country.actions.wide, "./data/Country-Actorness.rds")


# Aggregate to speech level

# We want the number of country/actorness recognitions per speech
# To be precise: The number of sentences in speech that ascribe actorness to country X

agg.doc <- country.actions.wide %>% 
  left_join(ungd %>% select(id, doc_id), by = "id") %>% 
  select(-id) %>% 
  group_by(doc_id) %>% 
  summarise_all(sum)

# Export country actorness counts
write_rds(agg.doc, "./data/CountryActorness_BySpeech.rds")
rm(agg.doc)




# Code IO agents ####

# Again, I have harmonized the names of the IOs of interest prior to semantic motif extraction
# We exploit this here (Note the "AFU" case)

io.codes <- c("EU", "UN", "NATO", "OSCE", "IMF", "WorldBank", "WTO", "ANDEAN", "AFU", "CARICOM", "OAS", "ASEAN")


# Filter actions of which one of the countries was the acting entity
io.actions <- actions %>% filter(Entity %in% io.codes)

# Absolute frequencies
io.actions.n <- io.actions %>% 
  select(Entity) %>% 
  group_by(Entity) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
head(io.actions.n)

# Get a wide data frame of io agents per id
io.actions.wide <- 
  io.actions %>%
  select(c(id, Entity)) %>% 
  unique() %>% # NB only stores whether there was at least one action by io in id (sentence), not whether there were several
  mutate(value = 1) %>% # Create a binary indicator for presence of each Entity
  pivot_wider(names_from = Entity, values_from = value, values_fill = 0) # Spread the Entity to wide format with values filled with 1 and absent values with 0


# Export io counts 
write_rds(io.actions.wide, "./data/IO-Actorness.rds")


# Aggregate to speech level

# We want the number of IO-agents per speech
# To be precise: The number of sentences in speech that ascribe actorness to IO X

agg.doc <- io.actions.wide %>% 
  left_join(ungd %>% select(id, doc_id), by = "id") %>% 
  select(-id) %>% 
  group_by(doc_id) %>% 
  summarise_all(sum)

# Export IO actorness counts
write_rds(agg.doc, "./data/IO-Actorness_BySpeech.rds")
rm(agg.doc)




# Some checks ####

eu <- actions %>% filter(Entity == "EU")
eu.actions <- eu %>% select(action) %>% group_by(action) %>% summarise(count = n()) %>% arrange(desc(count))

# EU references, but no actorness
eu.dict <- paste("(European Union)", 
                 "(([^A-Za-z]|^)(E\\.U\\.)([^A-Za-z]|$))",
                 "(European Communit(y|ies))",
                 "(([^A-Za-z]|^)(EC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.C\\.)([^A-Za-z]|$))",
                 "(European Economic Communit(y|ies))",
                 "(([^A-Za-z]|^)(EEC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.E\\.C\\.)([^A-Za-z]|$))", 
                 sep = "|")

test <- actions %>% filter(str_detect(sentence, eu.dict))
test2 <- test %>% select(id, sentence, Entity) %>% group_by(id, sentence) %>% summarise(eu.actorness = sum(Entity == "EU"))


# Measure run time
duration <- Sys.time() - start
duration # Time difference of 1.696379 mins
