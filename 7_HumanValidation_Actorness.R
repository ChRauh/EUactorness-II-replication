#####################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     EU actorness measures against human validation data
# Author:   @ChRauh (June 11, 2025)
#####################################################################


# Measure run time
start <- Sys.time()

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(patchwork) # The Composer of Plots CRAN v1.3.0
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2' CRAN v1.1.3
library(ggtext) # Improved Text Rendering Support for 'ggplot2' CRAN v0.1.2
library(glue) # Interpreted String Literals CRAN v1.8.0
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics CRAN v2.3
library(semgram) # Extracting Semantic Motifs from Textual Data CRAN v0.1.0
source("1_ExtractActorness.R") # Customized functions expanding Stuhler'S (2022) class of action motifs with nominalized actions
library(rsyntax) # Extract Semantic Relations from Text by Querying and Reshaping Syntax CRAN v0.1.4
library(data.table) # Extension of `data.frame` CRAN v1.17.0
library(spacyr) # Wrapper to the 'spaCy' 'NLP' Library CRAN v1.3.0
spacy_initialize("en_core_web_sm") # spaCy Version: 3.7.6, language model: en_core_web_sm


# Set global plotting parameters for this session
theme_set(
  theme_bw() +
    theme(plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          axis.text.x = element_markdown(color = "black"),
          axis.text.y = element_markdown(color = "black"),
          legend.title = element_markdown(),
          legend.text = element_markdown(),
          strip.text = element_markdown(),
          plot.caption = element_markdown()
    )
)



# The human coded data ####

# Collected through the validation app from three human coders
# Column eu_agent contains the classification along the SRL/agent field approach

adf <- 
  # Combine label sets from the three human coders
  rbind(
    read_csv("./data/HumanValidation/Actorness/coder1texts.csv") %>% 
      select(id, text, eu_agent, label) %>%
      mutate(coder = 1),
    read_csv("./data/HumanValidation/Actorness/coder2texts.csv") %>% 
      select(id, text, eu_agent, label) %>%
      mutate(coder = 2),
    read_csv("./data/HumanValidation/Actorness/coder3texts.csv") %>% 
      select(id, text, eu_agent, label) %>%
      mutate(coder = 3)) %>% 
  # Reduce 4-point scale offered to the coders to logical variable indicating the tendency to or against EU agency
  mutate(tendency = ifelse(str_detect(label, "yes"), T, F)) %>% 
  # Label 4-point scale as ordered factor
  mutate(label = factor(label, levels = c("Clearly not","Probably not","Probably yes", "Clearly yes"))) %>% 
  # More telling name for auto-classification
  rename(eu_agent_srl = eu_agent)
  

# Intercoder reliability ####

# In construction of the validation sample, I ensured that there is some (balanced) overlap in the coding units
# I re-construct this here to assess agreement among the coders

# Ids coded by at least three coders = overlap sample
overlap.ids <- 
  adf %>% 
  select(id, coder) %>% 
  group_by(id) %>% 
  summarise(coders = length(unique(coder))) %>% 
  filter(coders == 3) %>% 
  select(id) %>% 
  pull()

# Extract cases coded by all three coders
overlap <- adf %>% 
  filter(id %in% overlap.ids) %>% 
  unique() %>% 
  arrange(id)

# There are 2 more cases than the expected 300
# Meaning that there were duplicates (created by random sampling) in which the coder disagreed with him/herself ...
# We need to filter these out before assessing reliability across coders

help <- overlap %>% select(id) %>% group_by(id) %>% summarise(count = n()) %>% filter(count > 3) %>% select(id) %>% pull()
help2 <- overlap %>% filter(id %in% help)

# Two times Coder 3, with consistent tendency, but varying intensity
# Dropping the hard decisions of coder three in these instances

overlap <- overlap %>% 
  filter(!(id == 651046 & coder == 3 & as.character(label) == "Clearly yes")) %>% 
  filter(!(id == 784606 & coder == 3 & as.character(label) == "Clearly not"))

rm(help, help2)


# Put to wider format and store intercoder agreement
# Focus on the tendency, not intensity
icoder <- overlap %>% 
  mutate(coder = paste0("coder", coder)) %>% 
  select(id, coder, tendency) %>% 
  pivot_wider(id_cols = id, names_from = coder, values_from = tendency) %>% 
  mutate(all_agree = (coder1 == coder2) & (coder2 == coder3),
         coder1_2 = (coder1 == coder2),
         coder1_3 = (coder1 == coder3),
         coder2_3 = (coder2 == coder3))

# Long format for plotting
icoder2 <- icoder %>% 
  select(all_agree, coder1_2, coder1_3, coder2_3) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(cases = sum(value))

icoder2$pair <- c("All coders agree", "Coders 1 & 2 agree", "Coders 1 & 3 agree", "Coders 2 & 3 agree") %>% 
  factor(levels = c("All coders agree", "Coders 1 & 2 agree", "Coders 1 & 3 agree", "Coders 2 & 3 agree"))


# Plot intercoder agreement
ggplot(icoder2, aes(x = cases, y = fct_rev(pair))) + 
  geom_col(fill = "darkblue", color = NA, width = .7) +
  geom_text(aes(label = paste0(cases, "%")), vjust = 0, hjust = 1.5, color = "white")+
  scale_x_continuous(expand = c(0,0.05))+
  coord_cartesian(xlim = c(0, 100))+ 
  labs(title = "**Do sentences imply *EU actorness*? - Intercoder agreement**",
       subtitle = "N = 100 sentences coded by all three human coders. 4-point Likert scale reduced to yes/no tendency.<br>
       Stratified random sample drawn from all sentences that mention the EU (or its predecessors) in the United Nations General Debate speeches 1970-2020.",
       x = "<br>**Percentage agreement across coders**",
       y= "")+
  theme(axis.text.y = element_markdown(size = 10, face = "bold.italic"))

ggsave("./output/Appendix_Fig4_HumanValidation-Actorness-IntercoderReliability.png", width = 31, height =10, units = "cm")


# Harmonize EU references in the sample texts ####
# Along the same dictionary used for the main data

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

# Text cleaning (again identical to main data)
adf <- adf %>% 
  mutate(text2 = text) %>% 
  mutate(text2 = str_replace_all(text2, eu.dict, " EU ") %>% 
           str_replace_all("\\s+", " ") %>% 
           str_replace_all("( ){0,1}(\\'|’)", "\\2") %>%  # Faulty punctuation offsets
           str_replace_all("([A-Z])(-)( )([a-z])", "\\1\\2\\4") %>%
           str_replace_all("([A-Za-z])(\\(|\\[)", "\\1 \\2") %>% 
           str_replace_all(fixed("( "), "(") %>%
           str_replace_all(fixed("[ "), "(") %>%
           str_replace_all(" co( ){0,1}-( ){0,1}operation", " cooperation") %>% 
           str_replace_all(fixed("integraÂ¬tion"), "integration") %>% # Encoding errors
           str_replace_all(fixed("â€”"), "-")
        ) %>% 
  mutate(text2 = ifelse(str_detect(text2, "EU"), 
                        str_replace(text2, "Union", "EU"), 
                        text2))


# Cross check by inspecting random examples
i <- sample(1:nrow(adf), 1)
adf$id[i]
adf$text2[i]




# Extract the different actorness measures ####
# SRL is already in the data

# Mere presence of EU
# Due to sampling holds for all observations
# Insightful nevertheless, as benchmarking against human assessments gives us estimate of false positive rate

adf$eu_agent_pres <- T


# SemGramn approach (Stuhler 2021)
# EU as the agent of a verb

# POS and dependency parsing
parsed <- spacy_parse(adf$text2, dependency = T) 

eu_action <- extract_motifs(tokens = parsed, entities = c("EU"), markup = T)$actions %>% # Semantic motif extraction, actions only
  mutate(doc_id = str_remove(doc_id, "text") %>% as.numeric()) %>% # row id in the original data
  arrange(doc_id)
agent_rows <- eu_action %>% 
  select(doc_id) %>% 
  unique() %>% # some cases where the EU was agent of more than one verb
  pull()

adf <- adf %>% 
  mutate(eu_agent_dep = row_number() %in% agent_rows)



# My lead measure
# Enhance semgram action motifs with nominalized actions
eu_action2 <- extract_actorness(tokens = parsed, entities = c("EU"), markup = T) %>% # CR expanded Semantic motif extraction
  mutate(doc_id = str_remove(doc_id, "text") %>% as.numeric()) %>% # row id in the original data
  arrange(doc_id)
agent_rows2 <- eu_action2 %>% 
  select(doc_id) %>% 
  unique() %>% # some cases where the EU was agent of more than one verb
  pull()

adf <- adf %>% 
  mutate(eu_agent_nom = row_number() %in% agent_rows2)


# Export data ####
write_rds(adf, "./data/HumanValidation/Actorness/ActornessValidationData.rds")




# Humans vs. machines ####
# Appendix Figure 5

# Focus on the human yes/no tendency
# and comparing it against all four versions of actorness auto-classification
# Visualize truth table for each metric and add standard performance metrics

adf <- read_rds("./data/HumanValidation/Actorness/ActornessValidationData.rds")


# Mere EU presence

df_pres <- # Calculate frequencies and percentages of matches and mismatches
  adf %>%
  count(eu_agent_pres, tendency) %>%
  rbind(data.frame(eu_agent_pres = c(F,F), tendency = c(F,T), n = c(0,0))) %>% # Needed only in this one for completeness - cases don't exist by definition
  mutate(share = n / sum(n) * 100) %>% 
  mutate(match = ifelse(eu_agent_pres == tendency, T, F)) %>% 
  mutate(tmatch = ifelse(eu_agent_pres & tendency, "True Positive",
                         ifelse(!eu_agent_pres & !tendency, "True Negative",
                                ifelse(eu_agent_pres & !tendency, "False Positive",
                                       "False Negative"))))

te_pres <- sum(df_pres$share[!df_pres$match]) # Total error rate
acc_pres <- sum(df_pres$share[df_pres$match]) # Accuracy
# Performance measures make no sense in this version, as there can be no negative as per sample contsruction (all contain EU mentions)
prec_pres <- NA
rec_pres <- NA
f1_pres <- NA

# Store them in html table
performance_pres <-
  data.frame(
    Accuracy = round(acc_pres, 2),
    Recall = round(rec_pres, 2),
    Precision = round(prec_pres, 2),
    `F1 Score` = round(f1_pres, 2)
  )
  
perf_pres_grob <- tableGrob(performance_pres, rows = NULL)


 
# Plot distribution of all instances
pl_truth_pres <- 
  ggplot(df_pres, aes(y = factor(eu_agent_pres), x = factor(tendency), fill = match)) +
  geom_tile(aes(alpha = share)) + 
  scale_alpha_continuous(limits = c(0, 100), range = c(0, 1))+
  geom_text(aes(label = tmatch), vjust = -1, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", share)), vjust = 1, color = "black") +
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  labs(title = "***Merely mentioned***",
       subtitle = "Note: True for all sentences in the validation sample.",
       y = "Classification<br>", x = "<br>Human assessement<br>", 
       fill = "Frequency (%)"
       ) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(color = "black", fill = "white", size = .8))

# Adjust the width of the table to match the full width of the plot
perf_pres_grob$widths <- grid::unit(rep(1 / ncol(performance_pres), ncol(performance_pres)), "npc")

# Combine the truth table plot and the table
pl_pres <- 
  plot_grid(
  pl_truth_pres, 
  perf_pres_grob, 
  ncol = 1, 
  align = "v", # Align vertically
  axis = "lr", # Ensure left and right edges align
  rel_heights = c(3, 0.5) # Adjust relative heights
)
pl_pres 



# Dependency parsing (semgram)

df_dep <- # Calculate frequencies and percentages of matches and mismatches
  adf %>%
  count(eu_agent_dep, tendency) %>%
  mutate(share = n / sum(n) * 100) %>% 
  mutate(match = ifelse(eu_agent_dep == tendency, T, F)) %>% 
  mutate(tmatch = ifelse(eu_agent_dep & tendency, "True Positive",
                         ifelse(!eu_agent_dep & !tendency, "True Negative",
                                ifelse(eu_agent_dep & !tendency, "False Positive",
                                       "False Negative"))))

te_dep <- sum(df_dep$share[!df_dep$match]) # Total error rate
acc_dep <- sum(df_dep$share[df_dep$match]) # Accuracy
prec_dep <- sum(df_dep$share[df_dep$tmatch == "True Positive"])/(sum(df_dep$share[df_dep$tmatch == "True Positive"])+sum(df_dep$share[df_dep$tmatch == "False Positive"])) # Precision
rec_dep <- sum(df_dep$share[df_dep$tmatch == "True Positive"])/(sum(df_dep$share[df_dep$tmatch == "True Positive"])+sum(df_dep$share[df_dep$tmatch == "False Negative"])) # Recall
f1_dep <- 2/((1/prec_dep)+(1/rec_dep))

# Store them in html table

performance_dep <-
  data.frame(
    Accuracy = round(acc_dep, 2),
    Recall = round(rec_dep, 2),
    Precision = round(prec_dep, 2),
    `F1 Score` = round(f1_dep, 2)
  )

perf_dep_grob <- tableGrob(performance_dep, rows = NULL)



# Plot distribution of all instances
pl_truth_dep <- 
  ggplot(df_dep, aes(y = factor(eu_agent_dep), x = factor(tendency), fill = match)) +
  geom_tile(aes(alpha = share)) + 
  scale_alpha_continuous(limits = c(0, 100), range = c(0, 1))+
  geom_text(aes(label = tmatch), vjust = -1, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", share)), vjust = 1, color = "black") +
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  labs(title = "***Active/passive subject of verb***",
       subtitle = "\'Action motifs\' in Stuhler's (2022) *semgram* algorithm.",
       y = "Classification<br>", x = "<br>Human assessement<br>", 
       fill = "Frequency (%)"
  ) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(color = "black", fill = "white", size = .8))

# Adjust the width of the table to match the full width of the plot
perf_dep_grob$widths <- grid::unit(rep(1 / ncol(performance_dep), ncol(performance_dep)), "npc")

# Combine the truth table plot and the table
pl_dep <- 
  plot_grid(
    pl_truth_dep, 
    perf_dep_grob, 
    ncol = 1, 
    align = "v", # Align vertically
    axis = "lr", # Ensure left and right edges align
    rel_heights = c(3, 0.5) # Adjust relative heights
  )
pl_dep 



# SRL approach

df_srl <- # Calculate frequencies and percentages of matches and mismatches
  adf %>%
  count(eu_agent_srl, tendency) %>%
  mutate(share = n / sum(n) * 100) %>% 
  mutate(match = ifelse(eu_agent_srl == tendency, T, F)) %>% 
  mutate(tmatch = ifelse(eu_agent_srl & tendency, "True Positive",
                         ifelse(!eu_agent_srl & !tendency, "True Negative",
                                ifelse(eu_agent_srl & !tendency, "False Positive",
                                       "False Negative"))))

te_srl <- sum(df_srl$share[!df_srl$match]) # Total error rate
acc_srl <- sum(df_srl$share[df_srl$match]) # Accuracy
prec_srl <- sum(df_srl$share[df_srl$tmatch == "True Positive"])/(sum(df_srl$share[df_srl$tmatch == "True Positive"])+sum(df_srl$share[df_srl$tmatch == "False Positive"])) # Precision
rec_srl <- sum(df_srl$share[df_srl$tmatch == "True Positive"])/(sum(df_srl$share[df_srl$tmatch == "True Positive"])+sum(df_srl$share[df_srl$tmatch == "False Negative"])) # Recall
f1_srl <- 2/((1/prec_srl)+(1/rec_srl))

# Store them in html table

performance_srl <-
  data.frame(
    Accuracy = round(acc_srl, 2),
    Recall = round(rec_srl, 2),
    Precision = round(prec_srl, 2),
    `F1 Score` = round(f1_srl, 2)
  )

perf_srl_grob <- tableGrob(performance_srl, rows = NULL)



# Plot distribution of all instances
pl_truth_srl <- 
  ggplot(df_srl, aes(y = factor(eu_agent_srl), x = factor(tendency), fill = match)) +
  geom_tile(aes(alpha = share)) + 
  scale_alpha_continuous(limits = c(0, 100), range = c(0, 1))+
  geom_text(aes(label = tmatch), vjust = -1, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", share)), vjust = 1, color = "black") +
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  labs(title = "***Occurs in agent field of SRL solution***",
       subtitle = "Semantic role labelling with AllenNLP model.",
       y = "Classification<br>", x = "<br>Human assessement<br>", 
       fill = "Frequency (%)"
  ) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(color = "black", fill = "white", size = .8))

# Adjust the width of the table to match the full width of the plot
perf_srl_grob$widths <- grid::unit(rep(1 / ncol(performance_srl), ncol(performance_srl)), "npc")

# Combine the truth table plot and the table
pl_srl <- 
  plot_grid(
    pl_truth_srl, 
    perf_srl_grob, 
    ncol = 1, 
    align = "v", # Align vertically
    axis = "lr", # Ensure left and right edges align
    rel_heights = c(3, 0.5) # Adjust relative heights
  )
pl_srl 





# Semgram + nominalized actions

df_nom <- # Calculate frequencies and percentages of matches and mismatches
  adf %>%
  count(eu_agent_nom, tendency) %>%
  mutate(share = n / sum(n) * 100) %>% 
  mutate(match = ifelse(eu_agent_nom == tendency, T, F)) %>% 
  mutate(tmatch = ifelse(eu_agent_nom & tendency, "True Positive",
                         ifelse(!eu_agent_nom & !tendency, "True Negative",
                                ifelse(eu_agent_nom & !tendency, "False Positive",
                                       "False Negative"))))

te_nom <- sum(df_nom$share[!df_nom$match]) # Total error rate
acc_nom <- sum(df_nom$share[df_nom$match]) # Accuracy
prec_nom <- sum(df_nom$share[df_nom$tmatch == "True Positive"])/(sum(df_nom$share[df_nom$tmatch == "True Positive"])+sum(df_nom$share[df_nom$tmatch == "False Positive"])) # Precision
rec_nom <- sum(df_nom$share[df_nom$tmatch == "True Positive"])/(sum(df_nom$share[df_nom$tmatch == "True Positive"])+sum(df_nom$share[df_nom$tmatch == "False Negative"])) # Recall
f1_nom <- 2/((1/prec_nom)+(1/rec_nom))

# Store them in html table

performance_nom <-
  data.frame(
    Accuracy = round(acc_nom, 2),
    Recall = round(rec_nom, 2),
    Precision = round(prec_nom, 2),
    `F1 Score` = round(f1_nom, 2)
  )

perf_nom_grob <- tableGrob(performance_nom, rows = NULL)



# Plot distribution of all instances
pl_truth_nom <- 
  ggplot(df_nom, aes(y = factor(eu_agent_nom), x = factor(tendency), fill = match)) +
  geom_tile(aes(alpha = share)) + 
  scale_alpha_continuous(limits = c(0, 100), range = c(0, 1))+
  geom_text(aes(label = tmatch), vjust = -1, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", share)), vjust = 1, color = "black") +
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  labs(title = "***Active/passive subject of (nominalized) verb***",
       subtitle = "Author's expansion of Stuhler's 'action motifs'.",
       y = "Classification<br>", x = "<br>Human assessement<br>", 
       fill = "Frequency (%)"
  ) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(color = "black", fill = "white", size = .8))

# Adjust the width of the table to match the full width of the plot
perf_nom_grob$widths <- grid::unit(rep(1 / ncol(performance_nom), ncol(performance_nom)), "npc")

# Combine the truth table plot and the table
pl_nom <- 
  plot_grid(
    pl_truth_nom, 
    perf_nom_grob, 
    ncol = 1, 
    align = "v", # Align vertically
    axis = "lr", # Ensure left and right edges align
    rel_heights = c(3, 0.5) # Adjust relative heights
  )
pl_nom 



# Combination of all plots

# Optics ...
invisible_spacer <- ggplot() + theme_void()

# Cobine truth tables
pl_comb <-
  plot_grid(pl_pres,
          invisible_spacer,
          pl_dep,
          invisible_spacer,
          pl_srl,
          invisible_spacer,
          pl_nom,
          nrow = 1,
          rel_widths = c(1, 0.05, 1, 0.05, 1, 0.05, 1)
          )

# Joint title
title <- ggdraw() + 
  draw_label(
    "Comparing four automated classifications of EU actorness against assessments of human coders (n=750)",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 16
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

# Final
pl_truth <- 
  plot_grid(
  title, pl_comb,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
ggsave("./output/Appendix_Fig5_HumanValidation_Actorness_AcrossApproaches.png", pl_truth, width = 50, height = 18, units = "cm")



# Measure run time
duration <- Sys.time() - start
duration # Time difference of 24.75899 secs


