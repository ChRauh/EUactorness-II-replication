############################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Inspect Human Validation data - Issue along semantic similarity
# Author:   @ChRauh (June 11, 2025)
############################################################################


# Measure run time
start <- Sys.time()

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(ggridges) # Ridgeline Plots in 'ggplot2' CRAN v0.5.6
library(ggtext) # Improved Text Rendering Support for 'ggplot2' CRAN v0.1.2

# Using markdown text formatting across all plots in this session
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
          strip.text = element_markdown()
          )
)



# Human coded data ####

# Collected through the validation app from three human coders

adf <- 
  # Combine label sets from the three human coders
  rbind(
    read_csv("./data/HumanValidation/IssueArea/coder1texts.csv") %>% 
      select(-X) %>%
      mutate(coder = 1),
    read_csv("./Data/HumanValidation/IssueArea/coder2texts.csv") %>% 
      select(-X) %>%
      mutate(coder = 2),
    read_csv("./Data/HumanValidation/IssueArea/coder3texts.csv") %>% 
      select(-X) %>%
      mutate(coder = 3)) %>% 
  # Rename human labels
  rename(econ.hum = classification1,
         dem.hum = classification2,
         sec.hum = classification3) %>% 
  # Label 4-point human scales as ordered factor
  mutate(across(
    ends_with(".hum"), 
    ~ factor(., levels = c("Clearly not","Probably not", "Probably yes", "Clearly yes"))
  ))


# Compare human assessment to semantic similarity from word embeddings ####


# Reshape data frame
# So as to have the human coder choice (= ends_with(".hum")) on specific issue area (dem/econ/sec) for each text (ind_code)
# side by side with the respective machine-generated similarity score (= ends_with(".simil"))

adfl <- adf %>% 
  mutate(ind_code = paste0(id, "-", coder)) %>% # The individual coding step
  mutate(across(ends_with(".hum"), ~ as.numeric(.))) %>% # Human codes to numeric
  select(-c(id, text, coder)) %>% 
  pivot_longer(
    cols = -ind_code,  # All columns except 'ind_code'
    names_to = c("type", "measure"),  # Split column names into 'type' and 'measure'
    names_sep = "\\."  # Use '.' as the separator
  ) %>%
  pivot_wider(
    names_from = measure,  # Spread 'measure' into separate columns
    values_from = value    # Use the values in the 'value' column
  ) %>% 
  mutate(hum = factor(hum,
                      levels = c(1,2,3,4),
                      labels = c("Clearly<br>*not*", "Probably<br>*not*", "Probably<br>*yes*", "Clearly<br>*yes*"))) %>% 
  mutate(type2 = recode(type,
                        "dem" = "***Liberal Democracy***", 
                        "econ" = "***Trade & Economy***", 
                        "sec" = "***Security***") %>% 
                  factor(levels = c("***Security***", "***Liberal Democracy***", "***Trade & Economy***")))


# Plot
# Mean levels of simil scores against human coder choice, by issue area

ggplot(adfl, aes(y = simil, x = hum, color = type2, group = type2)) + 
  geom_vline(xintercept = 2.5, linewidth = .8, color = "grey")+
  geom_vline(xintercept = c(1.5, 3.5), color = "grey")+
  stat_summary(
    fun.data = mean_cl_normal, 
    geom = "pointrange", 
    size = .7,
    linewidth = .5
  ) +
  stat_summary(
    fun = mean, 
    geom = "line",
    linetype = "dotted",
    linewidth = .6
  ) +
  facet_wrap(~type2, nrow = 1)+
  labs(title = "**Does a text invoke or imply *selected issue areas*?**",
       subtitle = "Comparing the embedding-based semantic similarity scores against the assessments of three human coders.<br>
       Stratified random sample of 974 sentences from speeches in the annual United Nations General Debate 1970-2020.<br>",,
       x = "<br>**Human assessment**<br>4-point Likert scale<br>",
       y = "**Semantic similarity scores**<br>Mean values and 95% confidence bands<br>")+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_markdown(size = 12))

ggsave("./output/Appendix_Fig7_HumanValidation_SemanticSimils.png", width = 26, height = 14, units = "cm")



# Measure run time
duration <- Sys.time() - start
duration # Time difference of 4.118073 secs
