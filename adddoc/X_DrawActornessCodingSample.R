################################################################
# Project:  EU actorness in UNGD speeches
# Task:     Draw sample of EU statements for manual coding agency
# Authors:  Christian Rauh (12.02.2023)
################################################################

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(scales) # Scale Functions for Visualization CRAN v1.4.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1
library(patchwork) # The Composer of Plots CRAN v1.3.0



# Preparation of population ####


# All sentences that mention the EU

sentences <- read_rds("./Data/ungd_sentences.rds") %>% # All sentences
  mutate(id = row_number()) %>% # Unique ID as used for the SRL part 
  select(-sentence_id)

# Detect EU presence, as done in the SRL approach
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

sentences$eu_pres <- str_detect(sentences$sentence, regex(eu.dict)) # Takes a bit 
sum(sentences$eu_pres) # 5040

# EU agency by SRL agent coding 
# Note - several 'solutions' per sentence, but id is a consistent pointer

eu_agency_sentences <- 
  read_rds("./Data/IOAgentsBySRL.rds") %>% # Generated in 2_CodeAgents.R
  filter(eu == 1) %>% # All instances in which a Eu reference has been detected in agent field
  select(id) %>% # Keep the sentence ids and store them in anumerical vector
  unique() %>% 
  pull()

sentences$eu_agent <- ifelse(sentences$id %in% eu_agency_sentences, TRUE, FALSE)

sum(sentences$eu_agent) == length(eu_agency_sentences) # Should be TRUE



# Construct the coding samples ####

# Target sample size
0.05*nrow(sentences[sentences$eu_pres, ]) # 5% of all sentences that mention the EU equals 252 - reasonable for a human coding tasks (they will have the opportunity to take breaks)

# I wanted to stratify the sample by time (year) country, and absence or presence of EU agency - but too many empty strata
# strata_sample_sizes <- eu.sent %>%
#   count(year, eu_agent, name = "stratum_size") %>%                   # Count the number of rows in each stratum
#   mutate(sample_size = round(stratum_size / sum(stratum_size) * total_sample_size)) # Proportional allocation

# So stratification only along EU agency

# Population: Only sentences mentioning the EU at all
eu.sent <- sentences %>% 
  filter(eu_pres) %>% 
  mutate(sentence = str_replace_all(sentence, "\\s+", " ")) %>% # Text cosmetics: irregular and doubled white space
  mutate(sentence = str_replace_all(sentence, "( )([[:punct:]])", "\\2")) %>% # Text cosmetics: Punctuation was offset before auto coding
  mutate(sentence = str_replace_all(sentence, "â€ ™ ", "\\'"))


# I want to have some overlap to check intercoder reliability, say 100 sentences
shared_sample_size <- 100

# And then 150 additional sentences coded by one of three coders = 350 sentences
coder_sample_size <- 150


# Drawing the shared sample ####

# Fix seed for replication
set.seed(20250109)

# Calculate the sample size for each stratum (combination of year and eu_agent)
strata_sample_sizes <- eu.sent %>%
  count(eu_agent, name = "stratum_size") %>%                   # Count the number of rows in each stratum
  mutate(sample_size = round(stratum_size / sum(stratum_size) * shared_sample_size)) # Proportional allocation

# Draw the stratified random sample
shared_sample <- eu.sent %>%
  inner_join(strata_sample_sizes, by = "eu_agent") %>%         # Add sample sizes to the data
  group_by(eu_agent) %>%                                       # Group by strata
  mutate(row_id = row_number()) %>%                           # Add unique row IDs for reproducibility
  filter(row_id %in% sample(row_id, unique(sample_size))) %>% # Use filter instead of slice for reproducible sampling
  ungroup()   



# Drawing the coder-specific samples ####

# Calculate the sample size for each stratum (combination of year and eu_agent)
strata_sample_sizes <- eu.sent %>%
  count(eu_agent, name = "stratum_size") %>%                   # Count the number of rows in each stratum
  mutate(sample_size = round(stratum_size / sum(stratum_size) * coder_sample_size)) # Proportional allocation

# Coder 1
set.seed(198011226)
coder1 <- eu.sent %>%
  inner_join(strata_sample_sizes, by = "eu_agent") %>%         # Add sample sizes to the data
  group_by(eu_agent) %>%                                       # Group by strata
  mutate(row_id = row_number()) %>%                           # Add unique row IDs for reproducibility
  filter(row_id %in% sample(row_id, unique(sample_size))) %>% # Use filter instead of slice for reproducible sampling
  ungroup()   

# Coder 2
set.seed(19840410)
coder2 <- eu.sent %>%
  inner_join(strata_sample_sizes, by = "eu_agent") %>%         # Add sample sizes to the data
  group_by(eu_agent) %>%                                       # Group by strata
  mutate(row_id = row_number()) %>%                           # Add unique row IDs for reproducibility
  filter(row_id %in% sample(row_id, unique(sample_size))) %>% # Use filter instead of slice for reproducible sampling
  ungroup() 

# Coder 3
set.seed(19500214)
coder3 <- eu.sent %>%
  inner_join(strata_sample_sizes, by = "eu_agent") %>%         # Add sample sizes to the data
  group_by(eu_agent) %>%                                       # Group by strata
  mutate(row_id = row_number()) %>%                           # Add unique row IDs for reproducibility
  filter(row_id %in% sample(row_id, unique(sample_size))) %>% # Use filter instead of slice for reproducible sampling
  ungroup() 

# Coder 4
set.seed(498044226)
coder4 <- eu.sent %>%
  inner_join(strata_sample_sizes, by = "eu_agent") %>%         # Add sample sizes to the data
  group_by(eu_agent) %>%                                       # Group by strata
  mutate(row_id = row_number()) %>%                           # Add unique row IDs for reproducibility
  filter(row_id %in% sample(row_id, unique(sample_size))) %>% # Use filter instead of slice for reproducible sampling
  ungroup()  

# Coder 0 # for app testing
set.seed(098000226)
coder0 <- eu.sent %>%
  inner_join(strata_sample_sizes, by = "eu_agent") %>%         # Add sample sizes to the data
  group_by(eu_agent) %>%                                       # Group by strata
  mutate(row_id = row_number()) %>%                           # Add unique row IDs for reproducibility
  filter(row_id %in% sample(row_id, unique(sample_size))) %>% # Use filter instead of slice for reproducible sampling
  ungroup() 


# Coder specific data sets and export inn the format expected by the validation app ###

rm(.Random.seed)

# Coder 1
coder1 <- 
  coder1 %>% 
  rbind(shared_sample) %>% 
  select(id, sentence, eu_agent) %>% 
  rename(text = sentence) %>% 
  mutate(label = NA) %>% # Will hold the human assessment later
  slice_sample(prop = 1) # reshuffle order of coding units
sum(coder1$eu_agent)
# write.csv(coder1, "./ValidationApps/Agency-Validation-Model/coder1texts.csv")

# Coder 2
coder2 <- 
  coder2 %>% 
  rbind(shared_sample) %>% 
  select(id, sentence, eu_agent) %>% 
  rename(text = sentence) %>% 
  mutate(label = NA) %>% # Will hold the human assessment later
  slice_sample(prop = 1) # reshuffle order of coding units
sum(coder2$eu_agent)
# write.csv(coder2, "./ValidationApps/Agency-Validation-Model/coder2texts.csv")

# Coder 3
coder3 <- 
  coder3 %>% 
  rbind(shared_sample) %>% 
  select(id, sentence, eu_agent) %>% 
  rename(text = sentence) %>% 
  mutate(label = NA) %>% # Will hold the human assessment later
  slice_sample(prop = 1) # reshuffle order of coding units
sum(coder3$eu_agent)
# write.csv(coder3, "./ValidationApps/Agency-Validation-Model/coder3texts.csv")

# Coder 4
coder4 <- 
  coder4 %>% 
  rbind(shared_sample) %>% 
  select(id, sentence, eu_agent) %>% 
  rename(text = sentence) %>% 
  mutate(label = NA) %>% # Will hold the human assessment later
  slice_sample(prop = 1) # reshuffle order of coding units
sum(coder4$eu_agent)
# write.csv(coder4, "./ValidationApps/Agency-Validation-Model/coder4texts.csv")


# Coder 0 (for app testing)
coder0 <- 
  coder0 %>% 
  rbind(shared_sample) %>% 
  select(id, sentence, eu_agent) %>% 
  rename(text = sentence) %>% 
  mutate(label = NA) %>% # Will hold the human assessment later
  slice_sample(prop = 1) # reshuffle order of coding units
sum(coder0$eu_agent)
# write.csv(coder0, "./ValidationApps/Agency-Validation-Model/coder0texts.csv")

