##################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Targeted dependency parsing of sentences in 
#           UNGD speeches
# Author:   @ChRauh (June 13, 2025)
##################################################################

# Measure run time
start <- Sys.time()

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(spacyr) # Wrapper to the 'spaCy' 'NLP' Library CRAN v1.3.0
spacy_initialize("en_core_web_sm") # NOTE: use the language model specified, spaCy version on my backend is 3.7.6
library(newsmap) # Semi-Supervised Model for Geographical Document Classification CRAN v0.9.0 
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1 



# The UNGD sentences ###
ungd <- read_rds("./data/UNGD/ungd_sentences.rds")


# Correct small errors in source texts (primarily encoding, copied column)
ungd$text <- ungd$sentence %>% 
  str_replace_all(fixed("â€ ™"), "'") %>% 
  str_replace_all(fixed("Ã£"), "ã") %>% 
  str_replace_all(fixed("Ã©"), "é") %>% 
  str_replace_all(fixed("Ã"), "à") %>% 
  str_replace_all(fixed("â€”"), "-") %>% 
  str_replace_all(fixed("â€œ”"), " ") %>% 
  str_replace_all(fixed("â€”"), " ")  %>% 
  str_replace_all(fixed("â€ ”"), " ")  %>% 
  str_replace_all(fixed("à³"), "ó")  %>% 
  str_replace_all(" co( ){0,1}-( ){0,1}operation", " cooperation") %>% 
  str_replace_all(fixed("integraÂ¬tion"), "integration")



# Harmonize IO references ####

# To have the EU and other IOs (which often come as multi-word expressions) correctly represented in the grammatical dependece trees
# I harmonize their references in the source texts consistently here. Facilitates extraction and clear entity labeling in actorness extraction downstream.
# Note that the RegEx matching used here is case sensitive by default while I take care that only full words match.

# Harmonize EU references 
eu.dict <- paste("(European Union)", 
                 # "(([^A-Za-z]|^)(EU)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.U\\.)([^A-Za-z]|$))",
                 "(European Communit(y|ies))",
                 "(([^A-Za-z]|^)(EC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.C\\.)([^A-Za-z]|$))",
                 "(European Economic Communit(y|ies))",
                 "(([^A-Za-z]|^)(EEC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.E\\.C\\.)([^A-Za-z]|$))", 
                 sep = "|")
ungd$text <- ungd$text %>% str_replace_all(eu.dict, " EU ")


# Other IOs
un.dict <- paste("(United Nations)", 
                 "(([^A-Za-z]|^)(UN)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(U\\.N\\.)([^A-Za-z]|$))",
                 sep = "|")
ungd$text <- ungd$text %>% str_replace_all(un.dict, " UN ")

nato.dict <- paste("(North Atlantic Treaty Organi(z|s)ation)", 
                   "(([^A-Za-z]|^)(NATO)([^A-Za-z]|$))",
                   "(([^A-Za-z]|^)(N\\.A\\.T\\.O\\.)([^A-Za-z]|$))",
                   sep = "|")
ungd$text <- ungd$text %>% str_replace_all(nato.dict, " NATO ")

osce.dict <- paste("(Organi(z|s)ation for Security and Co-operation in Europe)", 
                   "(([^A-Za-z]|^)(OSCE)([^A-Za-z]|$))",
                   "(([^A-Za-z]|^)(O\\.S\\.C\\.E\\.)([^A-Za-z]|$))",
                   sep = "|")
ungd$text <- ungd$text %>% str_replace_all(osce.dict, " OSCE ")

imf.dict <- paste("(International Monetary Fund)", 
                  "(([^A-Za-z]|^)(IMF)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(I\\.M\\.F\\.)([^A-Za-z]|$))",
                  sep = "|")
ungd$text <- ungd$text %>% str_replace_all(imf.dict, " IMF ")

wb.dict <- paste("(World Bank)")
ungd$text <- ungd$text %>% str_replace_all(wb.dict, " WorldBank ")

wto.dict <- paste("(World Trade Organi(z|s)ation)", 
                  "(([^A-Za-z]|^)(WTO)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(W\\.T\\.O\\.)([^A-Za-z]|$))",
                  sep = "|")
ungd$text <- ungd$text %>% str_replace_all(wto.dict, " WTO ")

and.dict <- paste("(Andean Community)", 
                  "(([^A-Za-z]|^)(CAN)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(C\\.A\\.N\\.)([^A-Za-z]|$))",
                  "(Andean Pact)",
                  sep = "|")
ungd$text <- ungd$text %>% str_replace_all(and.dict, " ANDEAN ")

au.dict <- paste("(African Union)", 
                 "(([^A-Za-z]|^)(AU)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(A\\.U\\.)([^A-Za-z]|$))",
                 "(Organi(s|z)ation of African Unity)", 
                 "(([^A-Za-z]|^)(OAU)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(O\\.A\\.U\\.)([^A-Za-z]|$))",
                 sep = "|")
ungd$text <- ungd$text %>% str_replace_all(au.dict, " AFU ") # Important NOTE: Using "AFU" here to prevent overlaps with the Iso2 country code of Australia ("AU"), this disinction is important downstream

cc.dict <- paste("(Caribbean Community)",
                 "(Caricom)",
                 "(CARICOM)",
                 "(([^A-Za-z]|^)(CC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(C\\.C\\.)([^A-Za-z]|$))",
                 sep = "|")
ungd$text <- ungd$text %>% str_replace_all(cc.dict, " CARICOM ")

oas.dict <- paste("(Organi(z|s)ation of American States)", 
                  "(([^A-Za-z]|^)(OAS)([^A-Za-z]|$))",
                  "(([^A-Za-z]|^)(O\\.A\\.S\\.)([^A-Za-z]|$))",
                  sep = "|")
ungd$text <- ungd$text %>% str_replace_all(oas.dict, " OAS ")

asean.dict <- paste("(Association of Southeast Asian Nations)", 
                    "(([^A-Za-z]|^)(ASEAN)([^A-Za-z]|$))",
                    "(([^A-Za-z]|^)(A\\.S\\.E\\.A\\.N\\.)([^A-Za-z]|$))",
                    sep = "|")
ungd$text <- ungd$text %>% str_replace_all(asean.dict, " ASEAN ")

rm(list = ls(pattern = "\\.dict$"))



# Harmonize country references ####

# Facilitates extraction and clear entity labeling in actorness extraction downstream, similar to the IOs above.

# Country text identifiers
# I use the the English country term list provided in the newsmap package (quanteda dictionary object)
# For each country this includes the country name, the country/nationality adjective, and the name of the capital (all lower case)

# From the original newsmap paper (https://www.tandfonline.com/eprint/dDeyUTBrhxBSSkHPn5uB/full):
# The seed dictionary that I created contains names of 239 countries and their
# major cities, as well as their demonyms. For example, the keywords registered to the seed
# dictionary for Ukraine and Iraq are only {Ukraine, Ukrainian*, Kiev} and {Iraq, Iraqi*,
# Baghdad}. Names of cities in the seed dictionary are restricted to the capital and the largest
# cities; therefore, the total number of keywords for all 239 countries is 800 words, on average,
# only 3.3 words per country.

# With a gfew exceptions manually corrected below, dictionary keys are ISO2 country codes (which I exploit in later steps).

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

# Country adjectives: I cannot replace them meaningfully with country names as this destroys the grammatical logic the dependency parser exploits.
# Thus I limit adjective matches only to cases in which they clearly refere to government actors from country X
cdict.terms$term <- cdict.terms$term %>% 
  str_replace_all("\\*$", " (government|state|federation|regime|president|prime minister|leader(s){0,1})") %>% 
  str_replace_all("ese$", "ese (government|state|federation|regime|president|prime minister|leader(s){0,1})") %>%
  str_replace_all("ois$", "ois (government|state|federation|regime|president|prime minister|leader(s){0,1})") %>%
  str_replace_all("oise$", "oise (government|state|federation|regime|president|prime minister|leader(s){0,1})")


# Ensure proper matching and replacement patterns 
# I use the capitalized country code for replacement
# Note that source text has punctuation offset by whitespaces already!
cdict.terms <- cdict.terms %>% 
  mutate(match = paste0("(?i)", " ", term, " ")) %>%  # First element ensures case insensitive matching
  mutate(replacement = paste0(" ", countrycode, " ")) %>% 
  select(-c(region1, region2)) %>% 
  filter(match != "(?i) us ") # Creates too many false positives in lower case and equals the replacement anyway

help <- data.frame(term = "united states of america", # ensuring that these instances are replace first
                   countrycode = "US",
                   match = "(?i) united states of america ",
                   replacement = " US ")
help2 <- data.frame(term = "soviet union", # not in newsmap dict
                   countrycode = "RU",
                   match = "(?i) soviet union ",
                   replacement = " RU ")

cdict.terms <- rbind(help, help2, cdict.terms)
rm(help, help2)

# Tests ###
# str_replace_all(" Burundi is a great country . ", cdict.terms$match[3], cdict.terms$replacement[3])
# str_replace_all(" The United States of America condem this action . ", "(?i) united states of america ", " US ")

# Start with whitespace if country names appear in the beginning of sentences
ungd$text2 <- paste0(" ", ungd$text)

# Replace country references with countrycodes (takes ~ 20 mins)
for (i in 1:nrow(cdict.terms)) {
  print(i)
  ungd$text2 <- ungd$text2 %>% str_replace_all(cdict.terms$match[i], cdict.terms$replacement[i])
}


# Inspections
test <- ungd %>% filter(str_detect(tolower(sentence), " russia"))
test <- ungd %>% filter(str_detect(tolower(sentence), " united states "))



# Text cleaning ####

# This is the input for the dependency parser
# Punctuation offsets mainly, so as to disturb dependency parsing as little as possible

ungd$text3 <- ungd$text2 %>% 
  str_replace_all("\\s+", " ") %>% # Multiple (irregular) white spaces to one regular whitespace
  str_replace_all("( )(\\'|’|\\.|,|\\!|\\?)", "\\2") %>%  # Faulty punctuation offsets
  str_replace_all("([A-Z])( )(-)( )([a-z])", "\\1\\3\\5") %>%
  str_replace_all("([A-Za-z])(\\(|\\[)", "\\1 \\2") %>% 
  str_replace_all(fixed("( "), "(") %>%
  str_replace_all(fixed("[ "), "(") %>% 
  str_trim(side = "both")

# Cross check by inspecting random examples
i <- sample(1:nrow(ungd), 1)
ungd$text3[i]



# Dependency parsing ####
# Note the doc_id that spacyr returns represents the row number of in the ungd sentences data frame!

parsed <- spacy_parse(ungd$text3, dependency = T) 
write_rds(parsed, "./data/ungd_DependencyTrees.rds")

# Measure run time
duration <- Sys.time() - start
duration # Time difference of 1.946485 hours
