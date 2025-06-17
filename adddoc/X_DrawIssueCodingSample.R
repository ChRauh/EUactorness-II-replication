####################################################################
# Project:  EU agency in UNGD speeches
# Task:     Extract embedding-scored sentences fro human validation
# Authors:  Christian Rauh (10.01.2025)
####################################################################


# Packages #####
library(tidyverse) 


# Get the full sample of UNGD sentences with semantic scores ####

# Semantic similarity scores by sentence
# Note, some sentences dropped during the embedding process
simil <- read_rds("./Data/SemanticSimils.rds")

# Raw UNGD sentences for aggregation
sent <- read_rds("./Data/ungd_sentences.rds") %>% 
  mutate(sentence_id = paste0(doc_id,"_Sentence",sentence_id)) %>% 
  left_join(simil %>% select(-doc_id), by = "sentence_id") %>% 
  rownames_to_column("id") %>% 
  select(-c("doc_id", "country")) %>%  
  filter(!is.na(dem.simil)) %>%  # Keep only sentences for which the semantic scores are available
  filter(!is.na(econ.simil)) %>% 
  filter(!is.na(sec.simil)) %>% 
  mutate(sentence = str_replace_all(sentence, "\\s+", " ")) %>% # Text cosmetics: irregular and doubled white space
  mutate(sentence = str_replace_all(sentence, "( )([[:punct:]])", "\\2")) %>% # Text cosmetics: Punctuation was offset before auto coding
  mutate(sentence = str_replace_all(sentence, "â€ ™ ", "\\'")) %>% 
  filter(str_detect(sentence, "^[A-Z]") & str_detect(sentence, "[[:punct:]]$")) %>% # Exclude list items etc
  filter(str_count(sentence, "\\S+") >= 5) %>% # At least 5-word sentences
  rename(text = sentence)

rm(simil)
gc()


# Inspect distributions ####
hist(sent$dem.simil)
hist(sent$econ.simil)
hist(sent$sec.simil)

# To maximize that the validation exercise captures the whole range of values (including outliers)
# rather than tapping only into the large noise in the center of the distributions
# and to enhance the corresponding discriminatory quality of the human validation
# I draw sample stratified by equal-width bins on each semantic similarity variable

# Furthermore, parts of the samples should overlapp so as to assess intercoder reliability later


# Stratified sampling ####

# Define samples and their sizes
core_sample_size <- 50
additional_sample_size <- 200
num_additional_samples <- 4 # (3 coders and one for app testing)

# Create equal-width bins of the semantic similarity scores for stratification
df <- sent %>%
  mutate(
    dem_bin = cut(dem.simil, breaks = 5),
    econ_bin = cut(econ.simil, breaks = 5),
    sec_bin = cut(sec.simil, breaks = 5)
  )


# Step 1: Create the core sample (common to all coders)

# Precompute sample sizes for the core sample (proportional sampling)
core_sample_sizes <- df %>%
  group_by(dem_bin, econ_bin, sec_bin) %>%
  summarise(group_size = n(), .groups = "drop") %>%
  mutate(sample_size = pmin(ceiling(core_sample_size / nrow(df) * group_size), group_size))
sum(core_sample_sizes$sample_size)

# Sample core rows using row indices (circling aroung the issue of varying constant in grouped operations)
set.seed(20250110)
core_sample <- core_sample_sizes %>%
  inner_join(df, by = c("dem_bin", "econ_bin", "sec_bin")) %>%
  group_by(dem_bin, econ_bin, sec_bin) %>%
  sample_frac(1) %>%  # Shuffle rows within each group to ensure random sampling
  filter(row_number() <= first(sample_size)) %>%
  ungroup()

# Step 2: Remove the core sample from the main dataset
remaining_df <- df %>%
  anti_join(core_sample, by = "id")  # Exclude core sample

# Step 3: Create additional non-overlapping samples
additional_samples <- list()

for (i in 1:num_additional_samples) {
  # Precompute sample sizes for the remaining data
  sample_sizes <- remaining_df %>%
    group_by(dem_bin, econ_bin, sec_bin) %>%
    summarise(group_size = n(), .groups = "drop") %>%
    mutate(sample_size = pmin(ceiling(additional_sample_size / nrow(df) * group_size), group_size))
  
  # Sample rows for the current sample
  set.seed(20250110)
  additional_samples[[i]] <- sample_sizes %>%
    inner_join(remaining_df, by = c("dem_bin", "econ_bin", "sec_bin")) %>%
    group_by(dem_bin, econ_bin, sec_bin) %>%
    sample_frac(1) %>%  # Shuffle rows within each group to ensure random sampling
    filter(row_number() <= first(sample_size)) %>%
    ungroup()
  
  # Remove the current sample from the remaining dataset to avoid overlap
  remaining_df <- remaining_df %>%
    anti_join(additional_samples[[i]], by = "id")
}

# Combine core sample with each additional sample
final_samples <- list()
for (i in 1:num_additional_samples) {
  final_samples[[i]] <- bind_rows(core_sample, additional_samples[[i]])
}


# Extract the four samples and export the####

coder1 <- final_samples[[1]] %>% 
  select(id, text, ends_with("simil")) %>% 
  mutate(classification1 = NA, # Variables to store the human label later
         classification2 = NA,
         classification3 = NA)
#write.csv(coder1, "./ValidationApps/Issue-Validation-Model/coder1texts.csv")

coder2 <- final_samples[[2]] %>% 
  select(id, text, ends_with("simil")) %>% 
  mutate(classification1 = NA, # Variables to store the human label later
         classification2 = NA,
         classification3 = NA)
#write.csv(coder2, "./ValidationApps/Issue-Validation-Model/coder2texts.csv")

coder3 <- final_samples[[3]] %>% 
  select(id, text, ends_with("simil")) %>% 
  mutate(classification1 = NA, # Variables to store the human label later
         classification2 = NA,
         classification3 = NA)
#write.csv(coder3, "./ValidationApps/Issue-Validation-Model/coder3texts.csv")

coder0 <- final_samples[[4]] %>% 
  select(id, text, ends_with("simil")) %>% 
  mutate(classification1 = NA, # Variables to store the human label later
         classification2 = NA,
         classification3 = NA)
#write.csv(coder0, "./ValidationApps/Issue-Validation-Model/coder0texts.csv")

overlap <- core_sample %>% 
  select(id, text, ends_with("simil")) %>% 
  mutate(classification1 = NA, # Variables to store the human label later
         classification2 = NA,
         classification3 = NA)
#write.csv(overlap, "./ValidationApps/Issue-Validation-Model/overlaptexts.csv")



ggplot() +
  geom_density(data = df, aes(x = dem.simil, color = "Population"), size = 1) +
  geom_density(data = coder1, aes(x = dem.simil, color = "Sample1"), size = 1) +
  geom_density(data = coder2, aes(x = dem.simil, color = "Sample2"), size = 1) +
  geom_density(data = coder3, aes(x = dem.simil, color = "Sample3"), size = 1) +
  labs(title = "Distribution of dem.simil in Population vs Sample")














