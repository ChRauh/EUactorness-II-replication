#####################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Coding specific agents in SRL solutions of UNGD sentences
# Author:   @ChRauh (June 11, 2025)
#####################################################################

# Context:
# SRL approaches (probabilistic, semi-supervised machine learning) might offer an alternative approach to identify actorness in texts (along the semantic role of an agent)
# This measure is only used as a benchmark for my rule-based extraction of actorness motifs from dependency trees in the paper, 
# But in prior tests I have extracted all SRL solutions for each sentence in UNGD speeches already and document how to extract actors from this here for completeness only

# Measure run time
start <- Sys.time()


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(quanteda) # Quantitative Analysis of Textual Data CRAN v4.3.0 
library(newsmap) # Semi-Supervised Model for Geographical Document Classification CRAN v0.9.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1 



# Data ####

# UN sentences with meta data
ungd <- read_rds("./data/ungd_sentences.rds") %>% 
  mutate(id = row_number()) %>% 
  select(-sentence_id)

# Semantic roles in UNGD sentences
# id matches row number in UN sentences data (uniquely identified sentences)
srl <- read_rds("./data/ungd_sentences_srl.rds") 

# Add metadata to SRL results
srl <- srl %>% 
  left_join(ungd, by = "id") %>% 
  mutate(srl_id = row_number())



# Code country agents ####
# How often does a country actor appear as an agent in UNGD sentences?

# The list of agents in sentences

agents <- srl %>% 
  filter(!is.na(agent)) %>% 
  select(id, srl_id, agent)


# Country text identifiers
# I use the the English country term list provided in the newsmap package (quanteda dictionary object)
# For each country this includes the country name, the country/nationality adjective, and the name of the capital (all lower case)

# from the newsmap paper (Digital Journalism).
# The seed dictionary that I created contains names of 239 countries and their
# major cities, as well as their demonyms. For example, the keywords registered to the seed
# dictionary for Ukraine and Iraq are only {Ukraine, Ukrainian*, Kiev} and {Iraq, Iraqi*,
# Baghdad}. Names of cities in the seed dictionary are restricted to the capital and the largest
# cities; therefore, the total number of keywords for all 239 countries is 800 words, on average,
# only 3.3 words per country.

# Dictionary keys are ISO2 country codes as far as I can see - CHECK!

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



# This includes compound terms, which needs to be prepared in the texts to be scored
# Here I first get a list of compund terms in the newsmap dictionary 

compounds <- cdict %>% 
  as.list() %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rename(term = 1) %>% 
  mutate(compound = str_detect(term, " ")) %>% 
  filter(compound)  %>% 
  pull(term)

# Get a dfm of country terms from the agents occurring in UNGD speeches

ccounts <- corpus(agents$agent, docvars = agents$srl_id) %>% 
  tokens(what = "word", verbose = T) %>% 
  tokens_compound(pattern = compounds,            # Here we compound tokens identified above - CHECK
                  valuetype = "fixed") %>% 
  dfm(tolower = T, 
      verbose = T) %>% 
  dfm_lookup(dictionary = cdict,
             valuetype = "glob",
             verbose = T) %>% 
  convert(to = "data.frame") %>% 
  select(-doc_id) # Genaret by corpus function above
  
# Quanteda preserves document order, so we can store identifiers here right away
ccounts <- ccounts %>% 
  mutate(id = agents$id,
         srl_id = agents$srl_id) %>% 
  relocate(id)

# The newsmap dictionary also encodes continents and regions hierarchically,
# For the present purposes, we only want to keep the iso2 country codes here
# and clean the variable names accordingly
names(ccounts) <- names(ccounts) %>% 
  str_remove("^.*\\.")

# Check whether country codes are unique without the region identifiers
length(names(ccounts)) == length(unique(names(ccounts)))

# Soviet Union
# The newsmap dictionary apparently does not include the Soviet Union
# Apparently a short coming for our present purposes (are there other defunct countries - CHECK?)

# I correct this one instance here
# Adding hits for 'soviet' to the Russia counts (BE EXPLCIT ABOUT THIS IN THE PAPER)

sum(str_detect(agents$agent, "(S|s)oviet"))
ccounts$RU <- ccounts$RU + str_detect(agents$agent, "(S|s)oviet")

# Export country counts per srl
# (to extract country-specific verbs later)
write_rds(ccounts, "./data/CountryAgentsBySRL.rds")


# Country-level aggregation
agg <- ccounts %>% 
  select(-c(id, srl_id)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(count = sum(value)) %>% 
  arrange(desc(count)) %>% 
  mutate(rank = row_number()) %>% 
  mutate(country = countrycode(name, "iso2c", "country.name"))

# Sanity check: Which countries are ascribed most actorness? (Top10)
# US by far on top, three other P5 in the TOP10, else very conflict-prone countries in the Middle East - appears reasonable
head(agg, 10)
agg$rank[agg$name == "GB"] # Only 41? Dictionary looks good though - CHECK compound terms again


# Aggregate to speech level

# We want the number of country/agents per speech
# To be precise: The number of sentences in speech that ascribe actorness to country X

# Note, however, that the counts (sums) on the sentence level may overlap:
# On the one hand there may be multiple dictionary hits within one agent representation.
# On the other hand, there are different and not mutually exclusive semantic role ''solutions' per sentence.

# Thus we first code TRUE/FALSE whether a given sentence (identified by id) has country X at least once as an agent
# Afterwards we sum (!) by speech (doc_id)

agg.sent <- ccounts %>%
  select(-srl_id) %>% 
  group_by(id)  %>% 
  summarise_all(sum) %>% # Grouping var is ignored in this setting, takes some time ...
  ungroup()  %>% 
  mutate(across(!id, function(x){as.logical(x)}))

agg.doc <- agg.sent %>% 
  left_join(ungd %>% select(id, doc_id), by = "id") %>% 
  select(-id) %>% 
  group_by(doc_id) %>% 
  summarise_all(sum)

# Export country actorness counts
write_rds(agg.doc, "./data/CountryAgentsBySpeech_SRL.rds")
rm(agg.doc)




# Code IO agents ####

# European Union

# I do this with a RegEx rather than with a dictionary approach
# Because of the myriad ways to refer to the EU 
# and the need to run this in a case-sensitive manner
eu.dict <- paste("(European Union)", 
                 "(([^A-Za-z]|^)(EU)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.U\\.)([^A-Za-z]|$))",
                 "(European Communit(y|ies))",
                 "(([^A-Za-z]|^)(EC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.C\\.)([^A-Za-z]|$))",
                 "(European Economic Communit(y|ies))",
                 "(([^A-Za-z]|^)(EEC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.E\\.C\\.)([^A-Za-z]|$))", 
                 sep = "|")

# Code whether the EU occurs as an agent (along the dictionary) 
# and directly correct some faulty cases that refer to MS or EU accession only
iocounts <- agents %>%
  mutate(eu = str_detect(agent, eu.dict) %>% 
           as.numeric()) %>% 
  mutate(eu = ifelse(str_detect(agent, "(members|countries|states|partners) (of|in) the") |
                       str_detect(agent, "accession to|membership") | 
                       str_detect(agent, "(M|m)ember (S|s)tates"), 
                     0, 
                     eu))
sum(iocounts$eu)


# Benchmark: Other security IOs
# Follow the EU coding approach above

un.dict <- paste("(United Nations)", 
                 "(([^A-Za-z]|^)(UN)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(U\\.N\\.)([^A-Za-z]|$))",
                 sep = "|")
iocounts$un <- str_detect(iocounts$agent, un.dict)


nato.dict <- paste("(North Atlantic Treaty Organi(z|s)ation)", 
                 "(([^A-Za-z]|^)(NATO)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(N\\.A\\.T\\.O\\.)([^A-Za-z]|$))",
                 sep = "|")
iocounts$nato <- str_detect(iocounts$agent, nato.dict)

osce.dict <- paste("(Organi(z|s)ation for Security and Co-operation in Europe)", 
                   "(([^A-Za-z]|^)(OSCE)([^A-Za-z]|$))",
                   "(([^A-Za-z]|^)(O\\.S\\.C\\.E\\.)([^A-Za-z]|$))",
                   sep = "|")
iocounts$osce <- str_detect(iocounts$agent, osce.dict)


# Benchmark: Other economic policy/development IOs

imf.dict <- paste("(International Monetary Fund)", 
                   "(([^A-Za-z]|^)(IMF)([^A-Za-z]|$))",
                   "(([^A-Za-z]|^)(I\\.M\\.F\\.)([^A-Za-z]|$))",
                   sep = "|")
iocounts$imf <- str_detect(iocounts$agent, imf.dict)

wb.dict <- paste("(World Bank)")
iocounts$wb <- str_detect(iocounts$agent, wb.dict)

wto.dict <- paste("(World Trade Organi(z|s)ation)", # include GATT?
                  "(([^A-Za-z]|^)(WTO)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(W\\.T\\.O\\.)([^A-Za-z]|$))",
                  sep = "|")
iocounts$wto <- str_detect(iocounts$agent, wto.dict)


# Benchmark: Other regional organizations

and.dict <- paste("(Andean Community)", 
                  "(([^A-Za-z]|^)(CAN)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(C\\.A\\.N\\.)([^A-Za-z]|$))",
                  "(Andean Pact)",
                  sep = "|")
iocounts$and <- str_detect(iocounts$agent, and.dict)

au.dict <- paste("(African Union)", 
                  "(([^A-Za-z]|^)(AU)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(A\\.U\\.)([^A-Za-z]|$))",
                  sep = "|")
iocounts$au <- str_detect(iocounts$agent, au.dict)

cc.dict <- paste("(Caribbean Community)",
                 "(Caricom)",
                 "(CARICOM)",
                 "(([^A-Za-z]|^)(CC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(C\\.C\\.)([^A-Za-z]|$))",
                 sep = "|")
iocounts$cc <- str_detect(iocounts$agent, cc.dict)

oas.dict <- paste("(Organi(z|s)ation of American States)", 
                 "(([^A-Za-z]|^)(OAS)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(O\\.A\\.S\\.)([^A-Za-z]|$))",
                 sep = "|")
iocounts$oas <- str_detect(iocounts$agent, oas.dict)

asean.dict <- paste("(Association of Southeast Asian Nations)", 
                  "(([^A-Za-z]|^)(ASEAN)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(A\\.S\\.E\\.A\\.N\\.)([^A-Za-z]|$))",
                  sep = "|")
iocounts$asean <- str_detect(iocounts$agent, asean.dict)


# Clean up
iocounts$agent <- NULL

# Export IO counts per srl
# (to extract IO-specific verbs later)
write_rds(iocounts, "./data/IOAgentsBySRL.rds")

# IO-level aggregation (for inspection)
agg <- iocounts %>% 
  select(-c(id, srl_id)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(count = sum(value)) %>% 
  arrange(desc(count)) %>% 
  mutate(rank = row_number())

# Aggregate to speech level
# See notes in country section above
agg.sent <- iocounts %>%
  select(-srl_id) %>% 
  group_by(id)  %>% 
  summarise_all(sum) %>% # Grouping var is ignored in this setting, takes some time ...
  ungroup()  %>% 
  mutate(across(!id, function(x){as.logical(x)}))

agg.doc <- agg.sent %>% 
  left_join(ungd %>% select(id, doc_id), by = "id") %>% 
  select(-id) %>% 
  group_by(doc_id) %>% 
  summarise_all(sum)

# Export IO actorness counts
write_rds(agg.doc, "./data/IOAgentsBySpeech_SRL.rds")
rm(agg.doc)


# Measure run time
duration <- Sys.time() - start
duration # Time difference of 9.931894 mins
