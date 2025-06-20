#####################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Describe EU actorness recognition in UNGD speeches and 
#           compare to that of other actors
#           Replicates results reported in sections 4 and 5 
#           of the main text
# Author:   @ChRauh (June 11, 2025)
#####################################################################


# Measure run time
start <- Sys.time()


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(scales) # Scale Functions for Visualization CRAN v1.4.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1
library(patchwork) # The Composer of Plots CRAN v1.3.0
library(sf) # Simple Features for R CRAN v1.0-21
library(rnaturalearth) # World Map Data from Natural Earth CRAN v1.0.1
library(rnaturalearthdata) # World Vector Map Data from Natural Earth Used in 'rnaturalearth' CRAN v1.0.0
library(ggspatial) # Spatial Data Framework for ggplot2 CRAN v1.1.9
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2' CRAN v0.9.6


# Coded agents per speech ####
# Generated in 2.1_CodeAgentsFromSemanticMotifs.R
country.agents <- read_rds("./data/CountryActorness_BySpeech.rds")
io.agents <- read_rds("./data/IO-Actorness_BySpeech.rds")
names(io.agents) <- tolower(names(io.agents))


# How much does the EU recognize its actorness in its own speeches?
help <- io.agents %>% 
  filter(str_detect(doc_id, "^EU"))
sum(help$eu) # 46 actorness recognitions
sum(help$eu > 0) / nrow(help) # Share of speeches with EU actorness: .9
rm(help)

# Do the EU-MS just delegate EU actorness recognition to EU representatives after they have received speaking rights in the UN?
help <- io.agents %>% 
  filter(str_detect(doc_id, "^DEU|^FRA")) %>% # Germany and France
  mutate(country = str_extract(doc_id, "^[A-Z]{3}")) %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  mutate(post_UN = year >= 2011) %>% 
  filter(year >= 1992) %>% 
  select(country, post_UN, eu) %>% 
  mutate(eu2 = eu > 0) %>% 
  group_by(country, post_UN) %>% 
  summarise(eu = mean(eu), # Count of Actorness recognition
            eu2 = mean(eu2)) # Share of actorness recognition actoss speeches
help # To sum extent, yes ... note to reviewer 1




# EU actorness recognition over time ####

# Annual share of speeches that express EU actorness at least once
df <- io.agents %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  select(year, eu) %>% 
  mutate(eu = as.logical(eu)) %>% 
  group_by(year) %>% 
  summarise(eu.share = mean(eu)) %>% 
  ungroup()

sd(df$eu.share)
max(df$eu.share)
df[df$eu.share == max(df$eu.share),] # 1991 is the year with highest Eu actorness recognition

# Mark different phases of EU foreign policy competences
# 1993: Maastricht - CFSP pillar (intergovernmentalist, unanimity)
# 1997: Amsterdam - High representative for the Commom Foreign and Security Policy 
# 2009: Lisbon - Commission seat for the high rep ( f the Union for Foreign Affairs and Security Policy), EEAS and European peace facility set up in 2010, Council presidnent speaks in UNGD from 2011 onwards

df$phase <- "Common commercial policy only"
df$phase[df$year >= 1993] <- "Maastricht"
df$phase[df$year >= 2009] <- "Lisbon"


# Plot annual shares (Figure 1 in main text)
ggplot(df, aes(x=year, y = eu.share))+
  geom_hline(yintercept = mean(df$eu.share), linetype = "dashed")+
  geom_vline(xintercept = c(1992.5, 2008.5), color = "grey80", size = 2, alpha = .7)+
  geom_smooth(method = "lm", aes(group = phase), color = "#003399")+
  geom_line(aes(group = phase), size = 1)+
  scale_x_continuous(breaks = seq(1970, 2020, 5), expand = c(0.01,0))+
  scale_y_continuous(labels = scales::percent)+
  annotate("text", x = 1993, y = .01, label = "Maastricht treaty\n(CFSP introduced)", hjust = "left", vjust = "bottom", size = 10*(5/14))+
  annotate("text", x = 2009, y = .01, label = "Lisbon treaty\n(a.o. HigRep empowered, EEAS)", hjust = "left", vjust = "bottom", size = 10*(5/14))+
  labs(y = " \nAnnual share of UNGD speeches\nthat recognize EU actorness at least once\n ",
       caption = "Note: Dashed line indicates grand mean, solid black line provides annual values, and blue line shows estimated linear time trend within period.",
       x = "")+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.minor = element_blank())
  
ggsave("./output/Fig1_EUactorness_AnnualSpeechShares.png", width = 28, height = 16, units = "cm")



# Annual counts of EU agency per speech
df <- io.agents %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  select(year, eu) %>% 
  group_by(year) %>% 
  summarise(eu.count = sum(eu)) %>% 
  ungroup()

# Mark different phases of EU foreign policy competences
# 1993: Maastricht - CFSP pillar (intergovernmentalist, unanimity)
# 1997: Amsterdam - High representative for the Commom Foreign and Security Policy 
# 2009: Lisbon - Commission seat for the high rep ( f the Union for Foreign Affairs and Security Policy), EEAS and European peace facility set up in 2010, Council presidnent speaks in UNGD from 2011 onwards

df$phase <- "Common commercial policy only"
df$phase[df$year >= 1993] <- "Maastricht"
df$phase[df$year >= 2009] <- "Lisbon"

# Plot mean counts
ggplot(df, aes(x=year, y = eu.count))+
  geom_hline(yintercept = mean(df$eu.count), linetype = "dashed")+
  geom_vline(xintercept = c(1992.5, 2008.5), color = "grey80", size = 2, alpha = .7)+
  geom_smooth(method = "lm", aes(group = phase), color = "#003399")+
  geom_line(aes(group = phase), size = 1)+
  scale_x_continuous(breaks = seq(1970, 2020, 5), expand = c(0.01,0))+
  annotate("text", x = 1993, y = -5, label = "Maastricht treaty\n(CFSP introduced)", hjust = "left", vjust = "bottom", size = 10*(5/14))+
  annotate("text", x = 2009, y = -5, label = "Lisbon treaty\n(a.o. HighRep empowered, EEAS)", hjust = "left", vjust = "bottom", size = 10*(5/14))+
  labs(y = " \nAnnual count of EU actorness recognitions\nin UNGD speeches\n ",
       caption = "Note: Dashed line indicates grand mean, solid black line provides annual values, and blue line shows estimated linear time trend within period.",
       x = "")+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.minor = element_blank())

ggsave("./output/Appendix_Fig8_EUactorness_AnnualSpeechCounts.png", width = 28, height = 16, units = "cm")



# EU compared to other IOs ####

# IO speech shares across all data
io.means <- io.agents %>% 
  select(-doc_id) %>% 
  mutate_all(as.logical) %>%
  summarise_all(mean) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = toupper(name)) %>% 
  arrange(desc(value))

ggplot(io.means, aes(x=fct_reorder(name, value), y = value)) +
  geom_col()+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(vjust = .5, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.minor = element_blank())



# Annual means
df <- io.agents %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  select(-doc_id) %>% 
  mutate(across(.cols = 1:12, as.logical)) %>% # Binary: IO agency indicated at least once
  group_by(year) %>% 
  summarise_all(mean) %>% 
  pivot_longer(cols = 2:13) %>% 
  mutate(name = toupper(name)) %>% 
  mutate(name = name %>% str_replace("AFU", "OAU/AU")) %>% # Note: Named differently before, to prevent overlap with iso2 for Australia
  mutate(name = name %>% str_replace("WORLDBANK", "WB")) %>% 
  mutate(name = name %>% str_replace("ANDEAN", "AND")) %>% 
  filter(name != "UN") # because it trumps everything anyway

df_hlines <- df %>% # Grand means by IO
  group_by(name) %>% 
  summarise(iomean = mean(value)) %>% 
  arrange(desc(iomean)) %>% # ordered by grand mean
  mutate(name2 = factor(name))

df$name2 <- factor(df$name, levels = df_hlines$name)

ggplot(df, aes(x=year, y = value, color = name2)) + 
  # geom_line()+
  geom_smooth(method = "loess", span = .5, se = F)+
  geom_hline(data = df_hlines, aes(yintercept = iomean, color = name2), linetype = "dotted", size = 1)+
  scale_y_continuous(labels = scales::percent, expand = c(0,0), breaks = c(0, .05, .1, .15, .2)) +
  labs(y = "\nAnnual share of UNGD speeches\nthat recognize actorness of the respective IO at least once\n ",
       caption = "Loess-smoothed time trends, dashed line indicates grand mean 1970-2020.",
       x="")+
  scale_color_manual(values = c("#8da0cb", "#003399", "#cc66ff", "#cc66ff", "grey40", "#cc66ff", "#8da0cb", "#8da0cb", "#8da0cb", "grey40", "#8da0cb"))+
  facet_grid(.~name2)+
  coord_cartesian(ylim = c(0,.25))+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 14))
  
ggsave("./output/Fig3_IOactorness_compared.png", width = 30, height = 14, units = "cm")


# In-text stats
df %>% filter(name == "AU") %>% arrange(desc(value)) %>% head(3)
df %>% filter(name == "WTO") %>% arrange(desc(value)) %>% head(3)




# EU versus country actors ####
# Figure 4 in the main text

# Add EU to the country agents, and store in long form for aggregation
ctry <- country.agents %>% 
  left_join(io.agents %>% select(c(doc_id, eu)), by = "doc_id") %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "country.actor", values_to = "present") %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}"),
         country.speaker = str_extract(doc_id, "[A-Z]{3}"),
         hl = country.actor == "eu", # Mark EU actorness recognitions
         present2 = as.logical(present))
sum(ctry$hl) # 8307



# Compare EU against TOP20 states pre/post 1992

ctry.means.pre1992 <- ctry %>% 
  filter(year < 1992) %>% 
  group_by(country.actor) %>% 
  summarise(actorness = mean(present2, na.rm = T)) %>% 
  arrange(desc(actorness)) %>% 
  head(26) %>% 
  mutate(country.name = countrycode(country.actor, origin = "iso2c", destination = "country.name"),
         country.name = ifelse(country.actor == "eu", "European Union", country.name),
         country.name2 = paste0(country.name, " (", row_number(),")"),
         country.name3 = factor(country.name2, levels = country.name2)) %>% 
  mutate(col = ifelse(country.name %in% c("European Union", "France", "Germany", "Spain"), T, F),
         alp = ifelse(country.name == "European Union", T, F)) 

ctry.means.post1992 <- ctry %>% 
  filter(year >= 1992) %>% 
  group_by(country.actor) %>% 
  summarise(actorness = mean(present2, na.rm = T)) %>% 
  arrange(desc(actorness)) %>% 
  head(26) %>% 
  mutate(country.name = countrycode(country.actor, origin = "iso2c", destination = "country.name"),
         country.name = ifelse(country.actor == "eu", "European Union", country.name),
         country.name2 = paste0(country.name, " (", row_number(),")"),
         country.name3 = factor(country.name2, levels = country.name2)) %>% 
  mutate(col = ifelse(country.name %in% c("European Union", "France", "Germany", "Spain"), T, F),
         alp = ifelse(country.name == "European Union", T, F)) 

# Plots for each period

pl.pre <-
  ggplot(ctry.means.pre1992, aes(x=actorness, y = fct_rev(country.name3), fill = col, alpha = alp))+
  geom_col(width = .5)+
  scale_x_continuous(labels = scales::label_percent(), expand = c(0,0), breaks = c(0, .1,.2,.3,.4,.5,.6,.7))+
  scale_fill_manual(values = c("black","#003399"))+
  scale_alpha_manual(values = c(.5,1))+
  coord_cartesian(xlim = c(0,.5))+
  labs(title = "1970-1991",
       x = "", y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text =  element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16, hjust = .5))

pl.post <-
  ggplot(ctry.means.post1992, aes(x=actorness, y = fct_rev(country.name3), fill = col, alpha = alp))+
  geom_col(width = .5)+
  scale_x_continuous(labels = scales::label_percent(), expand = c(0,0), breaks = c(0, .1,.2,.3,.4,.5,.6,.7))+
  scale_fill_manual(values = c("black","#003399"))+
  scale_alpha_manual(values = c(.5,1))+
  coord_cartesian(xlim = c(0,.5))+
  labs(title = "1992-2020",
       x = "", y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text =  element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16, hjust = .5))

# Joint plot
pl.joint <- 
  pl.pre+pl.post +
  # plot_annotation(title = "               EU actorness vs. state actorness (top 25)\n",
  #                 caption = "                    Share of UNGD speeches with actorness recognition") &
  plot_annotation(caption = "                    Share of UNGD speeches with actorness recognition") &
  theme(plot.caption = element_text(hjust = .5, size = 12),
        plot.title = element_text(hjust = .5, size = 16, face = "bold"))

ggsave("./output/Fig4_EUactoness-vs-Top25countries.png", pl.joint,  width = 32, height = 20, units = "cm")




# Who speaks about EU agency - a map ####
# Figure 5 in the main text

# Get a 'world' data frame from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
world$iso_a3[world$iso_a3_eh == "FRA"] <- "FRA" # Missing ISO for some reason
world$iso_a3[world$iso_a3_eh == "NOR"] <- "NOR" # Missing ISO for some reason

# Calculate share of speeches with EU agency by country
# And store it in the map data
ceu <- 
  io.agents %>% 
  mutate(iso_a3 = str_extract(doc_id, "[A-Z]{3}")) %>% 
  select(iso_a3, eu) %>% 
  mutate(eu = as.logical(eu)) %>% 
  group_by(iso_a3) %>% 
  summarise(eu.share = mean(eu, na.rm = T)) %>% 
  filter(!is.na(iso_a3)) %>% 
  ungroup()
world <- world %>% 
  left_join(ceu, by = "iso_a3")


# Calculate share of speeches with EU agency by country - after 1992
# And store it in the map data
ceu <- 
  io.agents %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  filter(year >= 1992) %>% 
  mutate(iso_a3 = str_extract(doc_id, "[A-Z]{3}")) %>% 
  select(iso_a3, eu) %>% 
  mutate(eu = as.logical(eu)) %>% 
  group_by(iso_a3) %>% 
  summarise(eu.share.1992 = mean(eu, na.rm = T)) %>% 
  filter(!is.na(iso_a3)) %>% 
  ungroup()
world <- world %>% 
  left_join(ceu, by = "iso_a3")


# Plot map - post 1992
ggplot(data = world) +
  geom_sf(aes(fill = eu.share.1992), 
          color = "black", linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_gradientn(colors = c("lemonchiffon", "#FFCC00", "#003399"), 
                       na.value = "grey90",
                       values=c(0,.16,1),
                       labels = label_percent())+
  labs(fill = "Share of countries' UNGD speeches 1992-2020 recognizing EU as actor at least once: \n ",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"))

ggsave("./output/Fig5_EUactornessRecognition_map_post1992.png", width = 20, height = 12, units = "cm")


# In-text stats
help <- world %>% select(sovereignt, eu.share.1992) %>% arrange(desc(eu.share.1992))



# Who speaks about the EU - numerical comparisions / post 1992 ####

# Combine EU actorness measure
# With metadata on speaker and extract continent

meta <- read_rds("./Data/UNGD/ungd.rds") %>% 
  select(c(doc_id, country, state, eu)) %>% 
  rename(eu_member = eu) %>% 
  mutate(eu_member = as.logical(eu_member))
meta$eu_member[is.na(meta$eu_member)] <- F

meta$continent <- countrycode(meta$country, origin = "iso3c", destination = "continent")

df <- io.agents %>% 
  select(doc_id, eu) %>% 
  left_join(meta, by = "doc_id") %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  filter(year >= 1992)
  
# Share of EU actorness recognition from EU member states
df %>% filter(eu_member) %>% 
  select(eu) %>% 
  mutate(eu = as.logical(eu)) %>% 
  summarise(mean = mean(eu)) 

# Share of EU actorness recognition from European states that are not EU members
df %>% filter(!eu_member & continent == "Europe") %>% 
  select(eu) %>% 
  mutate(eu = as.logical(eu)) %>% 
  summarise(mean = mean(eu))

# Share of EU actorness recognition from outside Europe
df %>% filter(continent != "Europe") %>% 
  select(eu) %>% 
  mutate(eu = as.logical(eu)) %>% 
  summarise(mean = mean(eu))

# Russia apparently coded as "Europe"
df %>% filter(state == "russia") %>% 
  select(eu) %>% 
  mutate(eu = as.logical(eu)) %>% 
  summarise(mean = mean(eu))


# External recognition
ext <- df %>% 
  filter(continent != "Europe") %>% 
  select(c(state, eu)) %>% 
  group_by(state) %>% 
  summarise(eu = mean(as.logical(eu), na.rm = T) %>% as.numeric()) %>% 
  arrange(desc(eu)) %>% 
  ungroup()

# Countries never recognizing EU actorness in UNGD speeches
never <- ext %>% 
  filter(eu == 0)

# Countries having done this only once in the 28 years
once <- ext %>% 
  filter(eu > 0.03 & eu < 0.035)


# EU references in US and Chinese speeches ####

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

# All sentences in US and CHN speeches mentioning the EU
eusent <- read_rds("./data/UNGD/ungd_sentences.rds") %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  filter(year >= 1992) %>% 
  filter(country %in% c("USA", "CHN")) %>% 
  mutate(eu = str_detect(sentence, eu.dict)) %>% 
  filter(eu)

# Do US and CHN speak about Europe instead?
europesent <- read_rds("./data/UNGD/ungd_sentences.rds") %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  filter(year >= 1992) %>% 
  filter(country %in% c("USA", "CHN")) %>% 
  mutate(eu = str_detect(sentence, "Europe")) %>% 
  filter(eu)

table(europesent$country) # China 8, US 45 - most geo refs, though

# Do US and CHN speak about the largest mebers instead?
membersent <- read_rds("./data/UNGD/ungd_sentences.rds") %>% 
  mutate(year = str_extract(doc_id, "[0-9]{4}") %>% as.numeric()) %>% 
  filter(year >= 1992) %>% 
  filter(country %in% c("USA", "CHN")) %>% 
  mutate(eu = str_detect(sentence, "Germany|France")) %>% 
  filter(eu)

table(membersent$country) # China 1, US 10

  

# Semantic similarity to target concepts ####

# In which contexts is EU actorness recognized?
# Figure 2 in main text

# Semantic similarity scores by sentence
# Note, some sentences dropped during the embedding process
simil <- read_rds("./data/SemanticSimils.rds")

# Raw UNGD sentences for aggregation
sent <- read_rds("./data/UNGD/ungd_sentences.rds") %>% 
  mutate(sentence_id = paste0(doc_id,"_Sentence",sentence_id)) %>% 
  left_join(simil %>% select(-doc_id), by = "sentence_id") %>% 
  rownames_to_column("id")
rm(simil)
gc()


# IO agents by sentence
# Filter sentence ids with EU agency
io.sent <- read_rds("./data/IO-Actorness.rds") %>% 
  select(c(id, EU)) %>% 
  filter(EU > 0) %>% 
  unique()

# Mark sentences with EU actorness recognition
sent$eu.actor <- sent$id %in% io.sent$id
test <- sent %>% filter(eu.actor)

# Mark whether speaker is EU member
sent <- sent %>% 
  left_join(meta %>% select(c("doc_id", "eu_member")), by = "doc_id")

# Standardize simil measures
sent <- sent %>% 
  mutate(econ_std = scale(econ.simil, center = T, scale = T)[, 1],
         dem_std = scale(dem.simil, center = T, scale = T)[, 1],
         sec_std = scale(sec.simil, center = T, scale = T)[, 1])
  
# Keep only EU references
# Since the simil data are standardized, they express deviations from grand mean
df <- sent %>% 
  filter(eu.actor) %>% 
  select(c(doc_id, year, eu_member, econ_std, dem_std, sec_std)) %>% 
  pivot_longer(cols = 4:6)

# Ploting labels and orders
df$eu_member2 <- ifelse(df$eu_member, "Speaker represents a contemporary EU member state",
                        "Speaker represents a state outside the EU")
df$period <- ifelse(df$year > 1992, "1993-2020 (after Maastricht)", "1970-1992 (before Maastricht)")

df$concept <- NA
df$concept[df$name == "econ_std"] <- "Trade & economy  "
df$concept[df$name == "sec_std"] <- "Security  "
df$concept[df$name == "dem_std"] <- "Lib. democracy  "
df$concept <- factor(df$concept, 
                     levels = c("Trade & economy  ",
                                "Security  ",
                                "Lib. democracy  "))

# Plot aggregate
ggplot(df, aes(y=value, x = fct_rev(concept), group = eu_member2, color = concept))+
  stat_summary(geom = "linerange", fun.data = "mean_cl_normal", 
               fun.args = list(conf.int = .99), position = position_dodge(width = .4), size = .5)+
  stat_summary(geom = "linerange", fun.data = "mean_cl_normal", 
               fun.args = list(conf.int = .95), position = position_dodge(width = .4), size = 1.5)+
  stat_summary(geom = "point", fun = mean, size = 3, fill = "white", aes(shape = eu_member2), 
               stroke = 1.5, position = position_dodge(width = .4))+
  scale_shape_manual(values = c(21,23))+
  scale_color_manual(values = c("#003399", "grey20", "#FFCC00"), guide = "none")+
  # facet_wrap(.~period, ncol = 1)+
  labs(title = "",
       x = "",
       y = " \nCosine similarity of word vectors from sentences with EU actorness recognition to word vectors of target concept\n(Standardized deviation from the grand mean across all 945,686 sentences in UNGD speeches).\n ",
       shape = "")+
  guides(shape = guide_legend(reverse = TRUE))+
  coord_flip()+
  theme_bw()+
  theme(
        # legend.position = c(.77,.17),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        legend.background = element_rect(color = "black", fill = "white", linewidth = .4),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", hjust = 0),
        axis.text = element_text(color = "black", size = 12),
        text = element_text(size = 12))

ggsave("./output/Fig2_EUActRecognition_SemanticSimils.png", width = 28, height = 12, units = "cm")

# In-text stats
df %>% filter(name == "dem_std") %>% group_by(eu_member) %>% summarise(value = mean(value))



# Total EU mentions - speeches ####
# In-text stats

total <- sent %>% 
  mutate(eu_mention = str_detect(sentence, eu.dict)) %>% 
  group_by(doc_id) %>% 
  summarise(eu_mention = sum(eu_mention, na.rm = T),
            eu_actor = sum(eu.actor, na.rm = T))
sum(total$eu_mention > 0) # 1990
sum(total$eu_actor > 0) # 1012

# Inspection examples
test <- sent %>% 
  mutate(eu_mention = str_detect(sentence, eu.dict)) %>% 
  filter(eu_mention & !eu.actor)
test$sentence[3]

sum(sent$eu.actor) # 2018


# Measure run time
duration <- Sys.time() - start
duration # Time difference of 1.214132 mins

