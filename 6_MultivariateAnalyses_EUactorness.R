##################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Multivariate models of External EU actorness recogn.
#           Replicates results in section 5 of the main text and
#           Appendices 7 and 8
# Authors:  @ChRauh (June 17, 2025)
#################################################################

# Measure run time
start <- Sys.time()



# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(scales) # Scale Functions for Visualization CRAN v1.4.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1
library(patchwork) # The Composer of Plots CRAN v1.3.0
library(ggforce) # Accelerating 'ggplot2' CRAN v0.4.2
library(cepiigeodist) # CEPII's GeoDist Datasets CRAN v0.1
library(modelsummary) # Summary Tables and Plots for Statistical Models and Data: Beautiful, Customizable, and Publication-Ready CRAN v2.4.0 
library(overviewR) # Easily Extracting Information About Your Data CRAN v0.0.13
library(fixest) # Fast Fixed-Effects Estimations CRAN v0.12.1
library(logistf) # Firth's Bias-Reduced Logistic Regression CRAN v1.26.1
library(performance) # Assessment of Regression Models Performance CRAN v0.14.0



# Data ####

# The outcome data - EU actorness recognition by speech
reg <- read_rds("./data/IO-Actorness_BySpeech.rds") 
names(reg) <- tolower(names(reg))
reg <- reg %>% 
  select(c(doc_id, eu)) %>% 
  mutate(eu = eu %>% as.logical() %>% as.numeric()) %>% # Binary - EU actorness recognized at least once in speech
  mutate(iso3 = str_extract(doc_id, "[A-Z]{3}"), # ISO3 of speaker's country
         year = str_extract(doc_id, "[0-9]{4}")) %>% 
  filter(year > 1992) # Post-Maastricht only

# Filter out EU member states 
# We focus on external recognition
meta <- read_rds("./data/UNGD/ungd.rds") %>% 
  select(c(doc_id, country, state, eu)) %>% 
  rename(eu_member = eu) %>% 
  mutate(eu_member = as.logical(eu_member))
meta$eu_member[is.na(meta$eu_member)] <- F

reg <- reg %>% 
  left_join(meta %>% select(c(doc_id, eu_member)), by = "doc_id") %>% 
  filter(!eu_member) %>% 
  select(-eu_member)
rm(meta)

# Filter out EU institution speeches
reg <- reg %>% 
  filter(!str_detect(doc_id, "^EU_"))

# EU recognition - a rare event?
# In-text stats
sum(reg$eu==1)
sum(reg$eu==1)/nrow(reg)


# Post Lisbon period - H1 XXXX
reg$lisbon <- ifelse(reg$year > 2008, TRUE, FALSE)



# Independent variables ####
# In the following we add IVs on the country/year level to the data set

# Variable: Speech themes - H2
# Along semantic similarities on speech level

simil <- read_rds("./data/SemanticSimils.rds") %>% 
  select(c(doc_id, econ.simil, dem.simil, sec.simil)) %>% 
  group_by(doc_id) %>% 
  summarise(across(everything(), mean, na.rm = T)) # Aggregate to speech level

reg <- reg %>% 
  left_join(simil, by = "doc_id")

rm(simil)
gc()


# Geographical distance - H3a
# From cepiigeodist package

distances <- dist_cepii %>% 
  select(iso_o, iso_d, distcap) %>% 
  filter(iso_d == "BEL") %>% # Brussels/Belgium as capital of EU and thus 'country of destination'
  select(-iso_d) %>% 
  rename(iso3 = iso_o) # Country of origin should match UNGD speakers' country

reg <- reg %>% 
  left_join(distances, by = "iso3")

rm(distances)


# Trade dependence <- H3b
# Data collected in IMF direction of trade statiscs via imfr package
# Needs to be normalized to GDP below

eutrade <- read_rds("./data/CountryData/EU-Trade-By-Country.rds") %>% 
  mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  mutate(eu_trade_vol = eu_exports_to_ctry + eu_imports_from_ctry) %>% 
  select(iso3, year, eu_trade_vol)

reg <- reg %>% 
  left_join(eutrade, by = c("iso3", "year"))

rm(eutrade)


# P5 States - H4
# Simple dummy

reg$p5 <- reg$iso3 %in% c("USA", "CHN", "RUS", "GBR", "FRA")
reg %>% filter(p5) %>% 
  group_by(iso3) %>% 
  summarise(count = n())


# GDP worldshare - H5
# From World Development Indicators (via Niehaus database)

gdp <- read_delim("./data/CountryData/WDI_GDP_CurrentPrices.txt", delim = "\t")
names(gdp)[1:2] <- c("country", "iso3")  
names(gdp) <- names(gdp) %>% 
  str_remove_all(" \\[.*?\\]") %>% 
  str_remove_all(" ")
gdp$SeriesCode <- gdp$SeriesName <- NULL

gdp <- gdp %>% 
  mutate(across(.cols =  3:64, ~na_if(., "..")))

gdp <- gdp %>% 
  pivot_longer(cols = 3:64,
               names_to = "year",
               values_to = "gdp_current_usd") 

# Extract worldwide GDP
gdp_world <- gdp %>% 
  filter(iso3 == "WLD") %>% 
  rename(gdp_world = gdp_current_usd) %>% 
  select(-c(country,iso3))

gdp <- gdp %>% filter(iso3 != "WLD") %>% 
  left_join(gdp_world, by = "year") %>% 
  mutate(gdp_current_usd = as.numeric(gdp_current_usd),
         gdp_world = as.numeric(gdp_world),
         gdp_worldshare = (gdp_current_usd/gdp_world)*100,
         year = as.numeric(year)) %>% 
  filter(!(iso3 %in% c("HIC", "OED", "PST",    # Drop some group sources, EUU is EU in these data
                        "NAC", "ECS", "IBT", 
                        "LMY", "IBD",  "EAR",
                        "UMC", "MIC", "EAS",
                        "LTE", "EMU", "EAP", 
                        "TEA"))) %>% 
  select(iso3, year, gdp_current_usd, gdp_worldshare) %>% 
  mutate(year = year %>% as.character())


# Plot superpowers' market shares
# Appendix Figure 1
df <- gdp %>% 
  filter(iso3 %in% c("EUU", "USA", "CHN")) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 1970)
df$market <- NA
df$market[df$iso3 == "EUU"] <- "European Union"
df$market[df$iso3 == "USA"] <- "USA"
df$market[df$iso3 == "CHN"] <- "China"
df$market <- factor(df$market, levels = c("USA", "European Union", "China"))

ggplot(df, 
       aes(x = year, 
           y = gdp_worldshare/100, 
           color = market,
           group = market))+
  geom_line(size = 2)+
  labs(title = "'Market shares' in the world economy",
       x = "",
       y = "Share of worldwide GDP\n ",
       color = "Market: ",
       caption = "Calculated from World Development Indicators, GDP annual in current USD")+
  scale_color_manual(values = c("black", "#003399", "#EE1C25"))+
  scale_y_continuous(labels = scales::label_percent())+
  scale_x_continuous(breaks = seq(1970, 2020, 10))+
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(face = "bold"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12, face = "bold"),
        plot.background = element_rect(fill='white', color= "white"),
        plot.caption = element_text(hjust = .5))

ggsave("./output/Appendix_Fig1_GDP-Shares.png", width = 16, height = 10, units = "cm")



# Add GDP to regression data
reg <- reg %>% 
  left_join(gdp, by = c("iso3", "year"))

# Normalize EU trade dependence to national GDP
reg$eu_trade_share <- (reg$eu_trade_vol*1000000)/reg$gdp_current_usd # Note: EU trade volume was in million current USD
reg$eu_trade_vol <- NULL

# Note: Can be theoretically higher than 1, but capped here
# https://en.wikipedia.org/wiki/List_of_countries_by_trade-to-GDP_ratio
reg$eu_trade_share[reg$eu_trade_share > 1] <- 1

hist(reg$eu_trade_share)
reg %>% arrange(desc(eu_trade_share)) %>% 
  filter(year == 2020) %>% 
  select(iso3, eu_trade_share)

rm(gdp, gdp_world)



# VDEM - liberal democracy index - H6
# From Niehaus database

vdem <- read_delim("./data/CountryData/02_13_23_0330pm_wep.csv", delim = ",") %>% 
  select(c(country, gwabbrev, year, v2x_libdem_VDEM))

# Get country names right
# Both Gleditsch & Ward character and numeric create the same missings
ctry <- vdem %>% 
  select(country, gwabbrev) %>% 
  mutate(cguess = countrycode(gwabbrev, origin = "gwc",  destination = "country.name")) %>% 
  group_by_all() %>% 
  summarise(count = n())
# can I get them from abbrev?
ctry$isotest <- countrycode(ctry$gwabbrev, origin = "iso3c",  destination = "country.name")
# Partially ...
for (i in 1:nrow(ctry)) {
  if(!is.na(ctry$cguess[i])){next}
  else{ctry$cguess[i] <- ctry$isotest[i]}
}
# Clean by hand
ctry$cguess[ctry$country == "Antigua & "] <- "Antigua & Barbuda"
ctry$cguess[ctry$country == "Grenada"] <- "Grenada"
ctry$cguess[ctry$country == "Hong Kong"] <- "Hong Kong SAR China"
ctry$cguess[ctry$country == "Kiribati"] <- "Kiribati"
ctry$cguess[ctry$country == "Marshall I"] <- "Marshall Islands"
ctry$cguess[ctry$country == "Monaco"] <- "Monaco"
ctry$cguess[ctry$country == "Nauru"] <- "Nauru"
ctry$cguess[ctry$country == "Nauru"] <- "Nauru"
ctry$cguess[ctry$country == "Palau"] <- "Palau"
ctry$cguess[ctry$country == "Saint Kitt"] <- "St. Kitts & Nevis"
ctry$cguess[ctry$country == "Saint Luci"] <- "St. Lucia"
ctry$cguess[ctry$country == "Saint Vinc"] <- "St. Vincent & Grenadines"
ctry$cguess[ctry$country == "San Marino"] <- "San Marino"
ctry$cguess[ctry$country == "Seychelles"] <- "Seychelles"
ctry$cguess[ctry$country == "Vanuatu"] <- "Vanuatu"
ctry$cguess[ctry$country == "Vietnam, D"] <- "Vietnam"

# Get correct ISO3c
ctry$iso3c <- countrycode(ctry$cguess, origin = "country.name", destination = "iso3c") # KV, Yemen A and Yugoslavia can be ignored given period and UN speaking rights

# Merge this into the vdem data
vdem <- vdem %>%
  left_join(ctry %>% 
              ungroup() %>% 
              select(country, iso3c), 
            by = "country") %>% 
  rename(iso3 = iso3c,
         libdem = v2x_libdem_VDEM) %>% 
  select(-c(gwabbrev, country)) %>% 
  mutate(year =  year  %>% as.character()) %>% 
  unique()

# Write to target data
reg <- reg %>% 
  left_join(vdem, by = c("iso3", "year"))





# Finalize and describe data available for multivariate analysis ####

# Keep only columns for hypotheses and order
reg <- reg %>% 
  mutate(p5 = p5 %>% as.numeric()) %>% 
  select(c(iso3, year, eu, econ.simil, dem.simil, sec.simil, distcap, eu_trade_share, p5, gdp_worldshare, libdem))



# Get overview of available data 

# Complete cases
sum(complete.cases(reg))
sum(complete.cases(reg))/nrow(reg)

# Variable info 
tmp <- reg %>% 
  select(-c(iso3, year)) %>% 
  rename(`EU actornesss recognized` = eu,
         `Trade & economy simil.` = econ.simil,
         `Liberal democr. simil.` = dem.simil,
         `Security simil.` = sec.simil,
         `Distance to Brussels (kms)` = distcap,
         `EU trade dependence` = eu_trade_share,
         `P5 state` = p5,
         `Share of world GDP (%)` = gdp_worldshare,
         `Liberal Democracy Index (VDem)` = libdem)

# datasummary_skim(tmp, output = "./Plots/ModelVarOverview.docx") # No histogram :(
datasummary_skim(tmp, output = "./output/Appendix_Table4_ModelVarOverview.html")
datasummary_skim(tmp, output = "html") # Copied manually, html written to disc excludes histogram


# Inspect unusual cases
reg %>% 
  filter(gdp_worldshare > 30) # All US

miss.dem <-
  reg %>% 
  filter(is.na(libdem)) %>% 
  group_by(iso3) %>% 
  summarise(count = n()) %>% 
  head(28) # Those are actually not available in VDEM






######################################
# Model EU actorness recognition ####


# Complete cases only
# Listwise deletion
comp <- reg %>% filter(complete.cases(.))

# Log GDP world share
comp <- comp %>% 
  mutate(gdp_worldshare = log(gdp_worldshare))

# z-standardized version (to compare effect sizes)
compz <- comp %>% 
  mutate(across(.cols = 4:11, function(x){scale(x, center = T, scale = T)[, 1]}))

# Regression formula suggested by the hypotheses
reg.form <- as.formula("eu ~ econ.simil + dem.simil + sec.simil + distcap + eu_trade_share + p5 + gdp_worldshare + libdem")


# Linear probability models 

# Basic linear probability model
lpm <- feols(fml = reg.form, 
             data = compz,
             vcov = "iid",
             ssc = ssc(adj = FALSE, cluster.adj = FALSE))
summary(lpm)

# LPM - heteroskedasticity robust SEs (White correction)
lpm.robust <- 
  feols(fml = reg.form, 
        data = compz, 
        vcov = "hetero")

# LPM - Standard errors clustered in year
lpm.c.year <-
  feols(fml = reg.form,
        data = compz , 
        vcov = cluster ~ year)

# LPM - Standard errors clustered in country
lpm.c.country <-
  feols(fml = reg.form,
        data = compz , 
        vcov = cluster ~ iso3)


# LPM - Standard errors clustered in country and year
lpm.twoway <-
      feols(fml = reg.form,
      data = compz , 
      vcov = cluster ~iso3 + year)



# LOGIT models

# Basic logit model
logit <- feglm(fml = reg.form, 
             data = compz,
             family = "binomial",
             vcov = "iid",
             ssc = ssc(adj = FALSE, cluster.adj = FALSE))
summary(logit)

# LOGIT - heteroskedasticity robust SEs (White correction)
logit.robust <- 
  feglm(fml = reg.form, 
        data = compz, 
        family = "binomial",
        vcov = "hetero")

# LOGIT - Standard errors clustered in year
logit.c.year <-
  feglm(fml = reg.form,
        data = compz , 
        family = "binomial",
        vcov = cluster ~ year)

# LOGIT - Standard errors clustered in country
logit.c.country <-
  feglm(fml = reg.form,
        data = compz , 
        family = "binomial",
        vcov = cluster ~ iso3)

# LOGIT - Standard errors clustered in country and year
logit.twoway <-
  feglm(fml = reg.form,
        data = compz, 
        family = "binomial",
        vcov = cluster ~iso3 + year)

# Rare event logit 
firth <- logistf(formula = reg.form,
                 data = compz)




# Overview table 
# For discussion in the appendix

modelsummary(models = list("1" = lpm, 
                           "2" = lpm.robust, 
                           "3" = lpm.c.year,
                           "4" = lpm.c.country,
                           "5" = lpm.twoway,
                           "6" = logit, 
                           "7" = logit.robust, 
                           "8" = logit.c.year,
                           "9" = logit.c.country,
                           "10" = logit.twoway,
                           "11" = firth),
             coef_omit = 1, # Drop intercept
             stars = T,
             fmt = 2,
             gof_map = "all",
             output = "./output/Appendix_Table5_RegressionModelSummaries.html")




# Plot basic LPM ####
# Figure 6 in main text

coeffs <- as.data.frame(cbind(coef = coef(lpm),
                              confint(lpm, level = 0.95),
                              confint(lpm, level = 0.99)))
names(coeffs) <- c("coeff", "lb95", "ub95", "lb99", "ub99")
coeffs <- coeffs %>% 
  rownames_to_column("iv") %>% 
  filter(iv != "(Intercept)")
coeffs$labels <- c("'Economy & Trade'\nterms",
                "'Liberal democracy'\nterms'",
                "'Security'\nterms",
                "Distance\nto Brussels",
                "EU trade\ndependency",
                "P5 State",
                "Share of world\nGDP (logged)",
                "Lib. Democracy\nIndex (VDem)")
coeffs$labels <- factor(coeffs$labels,
                        levels = c("'Economy & Trade'\nterms",
                                   "'Liberal democracy'\nterms'",
                                   "'Security'\nterms",
                                   "Distance\nto Brussels",
                                   "EU trade\ndependency",
                                   "P5 State",
                                   "Share of world\nGDP (logged)",
                                   "Lib. Democracy\nIndex (VDem)"))
coeffs$group <- c(rep("H2: Semantic similarity of speech to ...", 3),
                  rep("H3a-b: Interaction of speaker's country with the EU", 2),
                  rep("H4a-c: Status of speaker's country in the international system",3))

coeffs$group <- factor(coeffs$group,
                       levels = c("H2: Semantic similarity of speech to ...",
                                  "H3a-b: Interaction of speaker's country with the EU",
                                  "H4a-c: Status of speaker's country in the international system"))

ggplot(coeffs, aes(y = fct_rev(labels), x = coeff)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted", size = 1)+
  geom_linerange(aes(xmin = lb99, xmax = ub99), size = 0.4, color = "grey30")+
  geom_linerange(aes(xmin = lb95, xmax = ub95), size = 1.2, color = "grey30")+
  geom_point(shape = 21, fill = "white", size = 2, stroke = 2, color = "grey30")+
  facet_col(vars(group), scales = "free_y", space = "free")+
  labs(title = " \nVariational patterns in external recognition of EU actorness in UNGD speeches",
       subtitle = "Estimates from multivariate linear probability model. DV: EU actorness is recognized at least once in speech by foreign country.\n ",
       x = " \nStandardized regression coefficient\nwith 95% and 99% confidence intervals", 
       y = "",
       caption = " \nN: 3,3638 UNGD speeches between 1992 and 2020. Adj. R2:.05.")+
  theme_bw()+
  theme(strip.text = element_text(hjust = 0, face = "italic"),
        strip.background = element_rect(fill = "white"),
        axis.text.y = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        text = element_text(size = 12, color = "black"),
        plot.caption = element_text(hjust = .5))

ggsave("./output/Fig6_LPM_results.png", width = 28, height = 15, units = "cm")



# Some benchmarks for writing ####
# In-text stats

sd(comp$eu_trade_share)

sd(comp$gdp_worldshare)
mean(comp$gdp_worldshare[comp$iso3 == "IND"]) - sd(comp$gdp_worldshare)

cases <- comp %>% 
  filter(gdp_worldshare < -1.4 & gdp_worldshare > -1.45)


sd(comp$distcap)



# Measure run time
duration <- Sys.time() - start
duration # Time difference of 26.85586 secs




