# EUactorness-II-replication

*Replication archive* accompanying **[Rauh, Christian](www.christian-rauh.eu) (2025) "International recognition of European Union ‘actorness’: Language-based evidence from United Nations General Debate speeches 1970-2020", conditionally accepted for publication in *International Interactions***.\
 

## Contents

[Environment setup]

[Folders and files in this archive]

[Scripts in this archive]

[Contact]
&nbsp;        
&nbsp;        
     


## Environment setup

This replication archive is organized as an [**RStudio project**](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects). Please initiate replication by starting 'EUactorness-II-replication.Rproj' to have all relative paths working properly. All main analyses have been implemented in [**R version 4.5.0 (2025-04-11 ucrt)**](https://cran.r-project.org/bin/windows/base/old/4.5.0/) -- "How About a Twenty-Six".\
 

**Additional R packages** are indicated via respective library calls on top of each individual script (with the respective repository and package version annotated in the comments). For convenience, here is an alphabetical list of all library calls (including repository and package versions used here) across the different scripts:

```         
library(cepiigeodist) # CEPII's GeoDist Datasets CRAN v0.1
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2' CRAN v1.1.3
library(data.table) # Extension of `data.frame` CRAN v1.17.0
library(fixest) # Fast Fixed-Effects Estimations CRAN v0.12.1
library(ggforce) # Accelerating 'ggplot2' CRAN v0.4.2
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2' CRAN v0.9.6
library(ggridges) # Ridgeline Plots in 'ggplot2' CRAN v0.5.6
library(ggspatial) # Spatial Data Framework for ggplot2 CRAN v1.1.9
library(ggtext) # Improved Text Rendering Support for 'ggplot2' CRAN v0.1.2
library(glue) # Interpreted String Literals CRAN v1.8.0
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics CRAN v2.3
library(logistf) # Firth's Bias-Reduced Logistic Regression CRAN v1.26.1
library(magick) # Advanced Graphics and Image-Processing in R CRAN v2.8.7 # Advanced Graphics and Image-Processing in R CRAN v2.8.7
library(modelsummary) # Summary Tables and Plots for Statistical Models and Data: Beautiful, Customizable, and Publication-Ready CRAN v2.4.0 
library(newsmap) # Semi-Supervised Model for Geographical Document Classification CRAN v0.9.0
library(overviewR) # Easily Extracting Information About Your Data CRAN v0.0.13
library(patchwork) # The Composer of Plots CRAN v1.3.0
library(performance) # Assessment of Regression Models Performance CRAN v0.14.0
library(quanteda) # Quantitative Analysis of Textual Data CRAN v4.3.0
library(reticulate) # Interface to 'Python' CRAN v1.42.0
library(rnaturalearth) # World Map Data from Natural Earth CRAN v1.0.1
library(rnaturalearthdata) # World Vector Map Data from Natural Earth Used in 'rnaturalearth' CRAN v1.0.0
library(rsyntax) # Extract Semantic Relations from Text by Querying and Reshaping Syntax CRAN v0.1.4
library(scales) # Scale Functions for Visualization CRAN v1.4.0
library(semgram) # Extracting Semantic Motifs from Textual Data CRAN v0.1.0
library(sf) # Simple Features for R CRAN v1.0-21
library(shiny) # Web Application Framework for R CRAN v1.10.0
library(spacyr) # Wrapper to the 'spaCy' 'NLP' Library CRAN v1.3.0 (Note: used with spaCy Version 3.7.6 and language model "en_core_web_sm" in dedicated Python environment)
library(tidytext) # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools CRAN v0.4.2
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(xlsx) # Read, Write, Format Excel 2007 and Excel 97/2000/XP/2003 Files CRAN v0.6.5
```

 \
You can easily **install all packages** (and their dependencies) required for replication to your R environment by the following call (but note the package versions in the annotations above if you run into conflicts):

```         
install.packages(c('cepiigeodist', 'countrycode', 'cowplot', 'data.table', 'fixest', 'ggforce', 'ggrepel', 'ggridges', 'ggspatial', 'ggtext', 'glue', 'gridExtra', 'logistf', 'magick', 'modelsummary', 'newsmap', 'overviewR', 'patchwork', 'performance', 'quanteda', 'reticulate', 'rnaturalearth', 'rnaturalearthdata', 'rsyntax', 'scales', 'semgram', 'sf', 'shiny', 'spacyr', 'tidytext', 'tidyverse', 'xlsx'))
```

 \
Note that some of the initial data extraction scripts, in particular the dependency parsers, require a separate **Python 3.7** environment with [**spaCy 3.7.6**](https://spacy.io/usage) and language model **'[en_core_web_sm](https://spacy.io/models/en)'** installed on your machine.

Communication with this backend in R works through the `reticulate` package and usage of spaCy is wrapped by the `spacyr` package. This package typically allows installing the required Python environment directly from R itself via `spacy_install(version = "3.7.6", lang_models = "en_core_web_sm")`, but if you run into troubles please see the trouble-shooting [**here**](https://cran.r-project.org/web/packages/spacyr/readme/README.html).

However, **the replication archive also features all intermediary outputs** (including those generated by the demanding spaCy Python model) so that you can start the replication at different steps of the pipeline defined below.\
 \
All run time estimations of individual scripts reported below refer to a **Windows 10 OS running on an AMD Ryzen 7 PRO 5850U CPU with 48 GB RAM**.\
 

## Folders and files in this archive     
- **Folder './data/'**: Contains all input and intermediary data produced by the scripts as specified below (see section [Scripts in this archive]).
  - ***Folder './data/UNGD/'***: R binary file versions of the raw UN General Debate text corpus by [Mikhaylov et al (2017)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0TJX8Y&version=6.0): one on the level of full speeches (ungd.rds) and one on the level of individual sentences (ungd_sentences.rds).
  - ***Folder './data/GloVe/'***: R binary file version of the [pre-trained GloVe 6b/300d word vector model (Pennington et al. 2014)](https://nlp.stanford.edu/projects/glove/) (large file of ~ 1GB).
  - ***Folder './data/CountryData/'***: Raw versions of the Country/Year data used in the multivariate analyses from different sources (see section 3 of the main text for detail).
  - ***Folder './data/HumanValidation/'***: Human-coded sample sentences used in the validation of actorness (*appendix A4*) and issue emphasis measures (*appendix A5*).
  - ***Folder './data/'***: The root folder contains all intermediary data produced by the consecutive scripts as specified below, so that you can replicate all individual analyses also without necessarily repeating the computationally expensive and time-consuming data extraction scripts beforehand.    
&nbsp;      
    
- **Folder './output/'**: Holds all figures and numerical tables reported in the main text and the different appendices as they are produced by the individual scripts below (see section [Scripts in this archive]). 
&nbsp;    
     
- **Folder './adddoc/'**: Contains additional scripts not needed for replication but probably helpful for researchers wanting to extend some of the approaches discussed in the paper (esp. with regard to the web apps collecting human codes, visualizing dependency trees and annotations, or implementing SRL from within R).
&nbsp;    
    
- **Folder './manuscript/'**: Contains .docx and .pdf files of the main manuscript and the online appendices as submitted to the journal.
&nbsp;    
- **Folder ROOT**: Contains all .R scripts needed to replicate the results in the main text or the appendices (see section [Scripts in this archive] for detail).    
&nbsp;    
&nbsp;             
    
## Scripts in this archive    

This section provides brief summaries of each script - **task**, **special requirements**, **run time**, and **key outputs**.    
&nbsp;    
    
Note that the scripts are consecutively named and ordered, indicating their dependencies. However, all intermediary outputs are specified below and are directly contained in the archive so that *you can also replicate individual scripts separately*.    
&nbsp;    

### Preparatory data extraction scripts

-   **0_DependencyParsing.R**: Harmonizes all EU, IO and country references across the 945,686 sentences from the UN General Debate corpus 1970-2020 [(Mikhaylov et al., 2017, V6)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0TJX8Y&version=6.0) to then parse their grammatical dependency trees through the spaCy Python backend.
    -   Run time: **~ 2 hours**
    -   Python 3.7 backend for `spacyr` required (spaCy Version 3.7.6, language model 'en_core_web_sm')
    -   For convenient downstream replication, output separately stored in './data/ungd_DependencyTrees.rds'.    
    &nbsp;    
-   **1_ExtractActorness.R**: Functions implementing the rule-based annotation of actorness motifs in grammatical dependency trees as discussed in ***Appendix A4*** (expanding on [Stuhler 2022](https://github.com/omstuhler/semgram?tab=readme-ov-file)). Not to be executed separately, but called in script 2 below.\
     \
-   **2_SemanticMotifExtraction.R**: Parses all UNGD sentences to represent them in grammatical dependency trees, to then annotate and extract actorness motifs in/from these trees (using the functions above).
    -   Run time: **~ 11 hours**
    -   For convenient downstream replication, output separately stored in './data/ungd_ActornessMotifs.rds'.\
         \
-   **3_CodeAgentsFromSemanticMotifs.R**: Coding specific agents (EU, IOs, countries) in semantic action motifs extracted from UNGD sentences above.
    -   Run time: \~ 2 minutes\
    -   For convenient downstream replication, outputs separately stored in './data/Country-Actorness.rds', './data/CountryActorness_BySpeech.rds', './data/IO-Actorness.rds', and './data/IO-Actorness_BySpeech.rds'.\
         \
-   **4_EmbeddingSimilarities.R**: Embeds each sentence of the UNGD speeches into the vector space of the [pre-trained GloVe 6b/300d word vector model (Pennington et al. 2014)](https://nlp.stanford.edu/projects/glove/), to then calculate their cosine similarity to seed vectors circumscribing *economy*, *security*, and *liberal democracy* issues as described in section 3 of the main text and Appendix A5.
    -   Run time: **~ 10 hours (RAM intensive task!)**
    -   Pre-trained word embeddings included in the replication archive (please cite the original source when using this in your work; large file of \~ 1GB)\
    -   For convenient downstream replication, data outputs separately stored in './data/Sentence-GLOVE-vectors.rds', './data/SemanticSimils.rds', './data/EconomyNeighbours.rds', './data/SecurityNeighbours.rds',and './data/DemocracyNeighbours.rds'.\
    -   Appendix output: './output/Appendix_Table3_SemanticSimilToTargetVectors_RAW.xlsx' (unformatted version of ***Table 3 in Appendix A5***).  
&nbsp;    
&nbsp;    
    
### Main analysis scripts    
-   **5_DescriptiveAnalyses_EUactorness**: Produces the descriptive analyses reported in *Sections 4 and 5* of the main text.
    - Run time: ~ 2 mins
    - Text outputs:
      - **Main text Figure 1 - EU actorness over time (shares)**: './output/Fig1_EUactorness_AnnualSpeechShares.png'
      - **Appendix Figure 8 - EU actorness over time (counts)**: './output/Appendix_Fig8_EUactorness_AnnualSpeechCounts.png'
      - **Main text Figure 3 - EU / IO actorness comparison**: './output/Fig3_IOactorness_compared.png'
      - **Main text Figure 4 - EU actorness compared to countries**: './output/Fig4_EUactoness-vs-Top25countries.png'
      - **Main text Figure 5 - EU actorness recognition map (post-1992)**: './output/Fig5_EUactornessRecognition_map_post1992.png'
      - **Main text Figure 2 - Semantic contexts of EU actorness recognition**: './output/Fig2_EUActRecognition_SemanticSimils.png'
      - Several individual numbers reported in writing directly printed to the console upon execution (marked by 'In-text stats' comments)    
      &nbsp;    
-   **6_MultivariateAnalyses_EUactorness.R**: Produces the multivariate analyses reported in *Section 5* and Appendices *A1*, *A7*, and *A8*.
    - Run time: ~ 1 min
    - Text outputs:
      - **Appendix Figure 1 - Global GDP shares US, EU, China**: './output/Appendix_Fig1_GDP-Shares.png'
      - **Appendix Table 4 - Descriptives of Model Variables**: './output/Appendix_Table4_ModelVarOverview.html'
      - **Appendix Table 5 - Regression tables**: './output/Appendix_Table5_RegressionModelSummaries.html' (manually formatted further for presentation in text)
      - **Main text Figure 6 - Main results of LPM regression**: './output/Fig6_LPM_results.png'
      - Several individual numbers reported in writing directly printed to the console upon execution (marked by 'In-text stats' comments) 
&nbsp;    
&nbsp;     
     
### Measurement validation scripts    
-   **7_HumanValidation_Actorness.R**: Compares different text-based measures of actorness recognition against human-coded sample sentences.
    - Run time: ~ 1 min
    - Text outputs:
      - **Appendix Figure 4 - Intercoder reliability**: './output/Appendix_Fig4_HumanValidation-Actorness-IntercoderReliability.png'
      - **Appendix Figure 5 - Performance metrics of actorness measures**: './output/Appendix_Fig5_HumanValidation_Actorness_AcrossApproaches.png'
      &nbsp;    
-   **8_HumanValidation_SemanticSimilarities.R**: Compares word-vector-based semantic similarity measure against human-coded sample sentences.
    - Run time: ~ 1 min
    - Text outputs:
      - **Appendix Figure 7 - Human ratings of issue context prevalence against embedding-based scores**: './output/Appendix_Fig7_HumanValidation_SemanticSimils.png'
      &nbsp;   
&nbsp;    
&nbsp;    
    
### Additional documentation
-   **./adddoc/X_DrawActornessCodingSample.R**: Documents how the sample for human validation of the actorness measure was initially drawn from the UNGD sentences.
-   **./adddoc/X_DrawIssueCodingSample.R**: Documents how the sample for human validation of the issue context measure was initially drawn from the UNGD sentences.
-   **./adddoc/X_PlotExtractionRules.R**: Documents how to plot dependency trees with annotated actorness (reproducing the fictitious examples in *Appendix Figure 2*).
-   **./adddoc/X_SemanticRoleLabelling.R**: Illustrates how to call Python-based SRL algorithm from within R (not used in the final paper).
-   **./adddoc/Actorness-Validation-Model/app.R**: Code of web application used to gather human codes for validation of actorness measure (screenshot of this app provided as *Appendix Figure 3*)
-   **./adddoc/Issue-Validation-Model/app.R**: Code of web application used to gather human codes for validation of issue emphasis measure (screenshot of this app provided as *Appendix Figure 6*)

&nbsp;    
&nbsp;     
     
## Contact

Please direct all questions about the code or the data in this archive to [**Christian Rauh**](www.christian-rauh.eu) via [**Mail**](mailto:rauh@wzb.eu) or [**GitHub**](https://github.com/ChRauh/EUactorness-II-replication).     
&nbsp;    
If you use any of the external data or dedicated functions shipped with this replication data, please do cite the original sources (as done in the main manuscript or the appendices of the present paper).    
&nbsp;    
     
