###################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Visualise actorness extraction rules (dependency trees)
# Author:   @ChRauh (June 11, 2025)
###################################################################


# Measure run time
start <- Sys.time()

# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(data.table) # Extension of `data.frame` CRAN v1.17.0
library(semgram) # Extracting Semantic Motifs from Textual Data CRAN v0.1.0
source("1_ExtractActorness.R") # Customized function expandiv Stuhler'S (2022) class of action motifs
library(spacyr) # Wrapper to the 'spaCy' 'NLP' Library CRAN v1.3.0
spacy_initialize() # SpaCy version 3.7.6, model 'en_core_web_sm'
library(rsyntax) # Extract Semantic Relations from Text by Querying and Reshaping Syntax CRAN v0.1.4
library(magick) # Advanced Graphics and Image-Processing in R CRAN v2.8.7 # Advanced Graphics and Image-Processing in R CRAN v2.8.7


# Fix parameters of extraction functions here 
entities <- "EU"
verb_pos <- c("VERB", "AUX")
nominal_pattern <- "(tion|sion|ment|ance|ence|al|ing|ure|age|ery|ory|cy|ship|hood|ism|ability|activity|attack|aid|agenda|answer|appeal|approach|attempt|auspices|authority|ban|boycott|breach|call|campaign|capability|capacity|competence|compromise|control|claim|cut|demand|dialogue|directive|effort|force|help|influence|initiative|interest|mandate|message|move|offer|order|plan|pledge|policy|power|push|reply|request|response|responsibility|search|strategy|support|strength|tie|threat|visit|warning|willingness)$" # Common endings of nominalized verbs plus a few so-called zero-derivation nominalizations (where noun equals the verb)



# A1 

text <- "The EU attacked the freedom of American platform providers."
parsed <- spacy_parse(text, dependency = T)

query <- tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
              label = "Entity", fill = F,
              parents(pos = verb_pos,
                      label = "action", 
                      fill = F,
                      children(pos = verb_pos, relation = "conj", req = F,
                               not_children(relation = "nsubj", depth = 1),
                               label = "action", fill = F,
                               children(get_aux_verbs_par = "YES",
                                        pos = verb_pos, relation = "aux", req = F,
                                        label = "action", fill = F
                               )
                      ),
                      children(get_aux_verbs_par = "YES",
                               pos = verb_pos, relation = "aux", req = F,
                               label = "action", fill = F
                      )
              )
)


tokens_here <- annotate_tqueries(parsed %>% mutate(appos_child = "", get_aux_verbs_par = "YES"), 
                                 "query", query , overwrite = T, copy = F)
tokens_here$query_id[!is.na(tokens_here$query_id)] <- "Match"

png("./output/RuleA1.png", width = 1400, height = 400, res = 100)
plot_tree(tokens_here[doc_id == "text1"], token, lemma, pos,
          annotation = "query", use_color = F, viewer_mode = F)
mtext("Rule A1: ENTITY (here: \"EU\") is the nominal subject of a verb.", side = 3, line = 2.5, cex = 1.5, adj = 0)
dev.off()



# A4

text <- "Russia is pressured by the US and the EU."
parsed <- spacy_parse(text, dependency = T)

query <- tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "pobj",
                        parents(pos = "ADP", 
                                lemma = c("by", "of", "with", "between", "among", "from"),   # NOTE: Expansion by CR ("The agreement , which was signed between ENTITY and XYZ) - false positives?
                                relation = c("agent", "prep"), # Generalization: "We received funding from the ENTITY."
                                parents(pos = "VERB",
                                        label = "action", fill = F
                                )
                        )
                )
)


tokens_here <- annotate_tqueries(parsed %>% mutate(appos_child = "", get_aux_verbs_par = "YES"), 
                                 "query", query , overwrite = T, copy = F)
tokens_here$query_id[!is.na(tokens_here$query_id)] <- "Match"


png("./output/RuleA4.png", width = 1400, height = 400, res = 100)
plot_tree(tokens_here[doc_id == "text1"], token, lemma, pos,
          annotation = "query", use_color = F, viewer_mode = F)
mtext("Rule A4: ENTITY (here: \"EU\") is linked to verb in a passive \"by\" construction in conjunction with another entity.", side = 3, line = 2.5, cex = 1.5, adj = 0)
dev.off()



# A11

text <- "We welcome the strong support of the EU."
parsed <- spacy_parse(text, dependency = T)

query <- tquery(
  OR(token = entities, appos_child = "appos_child"),  # Match entities or their appositions
  relation = "pobj",  # Entity is the object of a prepositional phrase
  label = "Entity", fill = F,
  
  # Parents: the preposition governing the entity
  parents(
    token = c("of", "by", "with", "between", "among", "from"), 
    relation = "prep",
    
    # Parent of the preposition: the nominalized action
    parents(
      pos = c("NOUN", "PROPN"),
      lemma__R = nominal_pattern,  # Regex for nominalized actions
      label = "action",
      fill = F
    )
  )
)


tokens_here <- annotate_tqueries(parsed %>% mutate(appos_child = "", get_aux_verbs_par = "YES"), 
                                 "query", query , overwrite = T, copy = F)
tokens_here$query_id[!is.na(tokens_here$query_id)] <- "Match"


png("./output/RuleA11.png", width = 1400, height = 400, res = 100)
plot_tree(tokens_here[doc_id == "text1"], token, lemma, pos,
          annotation = "query", use_color = F, viewer_mode = F)
mtext("Rule A11: ENTITY (here: \"EU\") with prepostional link to a noun indicating a nominalized verb.", side = 3, line = 2.5, cex = 1.5, adj = 0)
dev.off()



# A17

text <- "The EU-led efforts in this regard were a disaster."
parsed <- spacy_parse(text, dependency = T)

query <- tquery(
  token = entities,  
  pos = c("NOUN", "PROPN"),  
  relation = "npadvmod",  # Updated relation type
  label = "Entity", fill = F,
  
  # The past participle verb that is modified by the ENTITY (e.g., "led", "mediated", "backed")
  parents(
    pos = c("VERB", "ADJ"),
    token__R = "ed$",        # Sometimes these instances are POS-tagged as adjectives (e.g. "EU-facilitated") but if they end on "ed" clearly derived from a verb
    label = "action",
    fill = F
  )
)


tokens_here <- annotate_tqueries(parsed %>% mutate(appos_child = "", get_aux_verbs_par = "YES"), 
                                 "query", query , overwrite = T, copy = F)
tokens_here$query_id[!is.na(tokens_here$query_id)] <- "Match"


png("./output/RuleA17.png", width = 1400, height = 400, res = 100)
plot_tree(tokens_here[doc_id == "text1"], token, lemma, pos,
          annotation = "query", use_color = F, viewer_mode = F)
mtext("Rule A17: ENTITY (here: \"EU\") modifies a verb in an adverbial noun phrase.", side = 3, line = 2.5, cex = 1.5, adj = 0)
dev.off()



# Combine graphs

library(magick) # Advanced Graphics and Image-Processing in R CRAN v2.8.7 # Advanced Graphics and Image-Processing in R CRAN v2.8.7

image1 <- image_read("./output/RuleA1.png")
image2 <- image_read("./output/RuleA4.png")
image3 <- image_read("./output/RuleA11.png")
image4 <- image_read("./output/RuleA17.png")

image1 <- image_border(image1, color = "white", geometry = "0x50")
image2 <- image_border(image2, color = "white", geometry = "0x50")
image3 <- image_border(image3, color = "white", geometry = "0x50")
image4 <- image_border(image4, color = "white", geometry = "0x50")


comb <- magick::image_append(c(image1, image2, image3, image4), stack = TRUE)

magick::image_write(comb, "./output/Appendix_Fig2_ExemplaryMatchingRulesActorness.png")

file.remove("./output/RuleA1.png")
file.remove("./output/RuleA4.png")
file.remove("./output/RuleA11.png")
file.remove("./output/RuleA17.png")


# Measure run time
duration <- Sys.time() - start
duration # Time difference of 6.785372 secs
