##################################################################
# Project:  EU actorness recognition (International Interactions)
# Task:     Rule-based identification of actorness motifs in
#           grammatical dependency trees of individual sentences
#           (see Appendix A.4 for rationale)
# Author:   @ChRauh (June 10, 2025)
##################################################################


#' @importFrom stringr str_c str_detect str_split str_extract
#' @importFrom data.table data.table
#' @importFrom rsyntax annotate_tqueries NOT parents tquery
NULL

#' Extract 'actorness' motifs from parsed text object
#'
#' This is a function for extracting explicit or implied actions of an ENITITY from text. Extraction is done by applying a set of extraction rules to the parsed text object
#' that includes part-of-speech tags and dependency relations. 
#' 
#' This function is an expansion of the theoretical reasoning and the dependency rules of 'action motifs' provided by Stuhler (2022).
#' 
#' In this view action motifs imply that an entity is doing something. The most straightforward example of this is when the entity serves as a nominal subject
#' of a verb ("ENTITY calls." - a_call). There are various syntactic constructions, however, in which a verb is considered an action despite the entity not being its nominal
#' subject. This includes instances in which the entity is the conjunct of a 
#' nominal subject ("John and ENTITY called." - a_call), there are multiple verbs ("ENTITY calls and asks." - a_call, a_ask), 
#' the entity  serves as an appositional modifier of a nominal subject (My friend ENTITY called. - a_call), 
#' and passive constructions ("John was called by ENTITY." - a_call). 
#' 
#' In Stuhlers approach, all actions are either lexical verbs or, if explicitly specified, auxiliary verbs.
#' 
#' Here I extend this motif class by dedicated rules to cover nominalizations which imply that ENTITY does, did, could or should do something,
#' ("ENTITY's support was crucial for this outcome.", "The cooperation of ENTITY was crucial", "The agreement betwenn John and ENTITY was important.")
#' 
#' @param tokens A tokens data.frame with predicted dependencies as generated, for instance, by spacyr::spacy_parse(). Dependencies need to be in ClearNLP style. This tag set is used by all English language models implemented in spaCy. Other languages or dependency grammars are currently not supported.
#' @param entities Specifies the core entities around which to extract motifs. This can be a single character string or a vector of character strings.
#' By default, multi-token strings such as "Harry Potter" will be parsed and considered. Note that this parameter is case-sensitive.
#' It defaults to "*" in which case any token is treated as a potential entity.
#' @param custom_cols Generally, the columns in the tokens object should be labeled as follows: "doc_id", "sentence_id", "token_id", "token", "lemma", "pos", "head_token_id" , "dep_rel". If the columns in your tokens object are not labeled according to this scheme, provide the matching column names to custom_cols in the corresponding order.
#' @param fast If set to TRUE, some of the more specific extraction rules are not applied. This results in fewer extractions but faster run time. Defaults to FALSE.
#' @param parse_multi_token_entities Should multi-token entities (e.g., "Harry Potter") be considered? Defaults to TRUE. When using multi-token entities, it is crucial that tokens are separated by a space character. Input should match the tokenized version in the tokens object.
#' For instance, hyphens are usually considered a token in tokenization, so that "Claude Levi-Strauss" should be passed to the function as "Claude Levi - Strauss".
#' @param extract Defines whether extracted motifs are represented in "lemma" or "token" form. Defaults to "lemma" which reduces sparsity and is preferable for most purposes.
#' @param markup If TRUE, motifs will also be provided as collapsed markup tokens (e.g., "aP_ask_Harry"). Defaults to FALSE.
#' @param add_sentence If TRUE, the sentence for each motif is added to the extracted motif. Note that this is done by pasting together the tokens of the sentence, so that the representation might differ minimally from the original text. Nonetheless, this can be helpful for validation and for a mode of analyses that switches between distant and close readings of the text. Defaults to FALSE. Note that setting this to TRUE will noticeably increase runtime.
#' @param add_paragraph If TRUE, a pseudo-paragraph (the sentence the motif is contained in, as well as ones immediately before and after it) for each motif is added to the extracted motif. Defaults to FALSE. Setting this to TRUE will noticeably increase runtime.
#' @param verb_prep If TRUE, prepositions that follow an action or treatment are added to the respective verb. For instance in "ENTITY believes in Sue." the action a_believe-in and the action-patient motif aP_believe-in_Sue will be extracted;
#' whereas otherwise, only a_believe would be extracted.
#' This is currently only implemented for the most common syntactic patterns for action, action-patient, treatment, and agent-treatment motifs (those also considered if fast is set to TRUE).
#' Note that the number of action motifs is unaffected by this parameter as action motifs are extracted regardless of whether
#' or not they have a preposition as dependent. However, the number of extracted action-patient, treatment, and agent-treatment motifs will increase if the parameter is set to
#' TRUE because the relation between action and patient (as well as between treatment and ENTITY) is frequently mediated by a preposition. Note that setting this to TRUE will
#' likely increase the level of sparsity in subsequent analyses.
#' Defaults to FALSE.
#' @param verb_prep_greedy By default, assuming verb_prep is set to TRUE, only prepositions immediately following a verb are considered (e.g., "ENTITY believes in Sue." leads to a_believe-in.) 
#' but more distant ones are disregarded (e.g., "ENTITY slammed it on the table." leads to a_slam, not a_slam-on). This behavior can be changed if verb_prep_greedy is set to TRUE. 
#' Note that this might result in some not immediately intuitive action motifs 
#' (e.g., the action a_want-on as extracted from "ENTITY want you on television!").
#' @param be_entity Should things that are linked to an entity via "being" (or one of its lemmas) be considered as characterization motifs?
#' For example, if we are extracting characterizations in the sentence "my parents are ENTITY", should we extract the characterization motif "be_parent"? Defaults to TRUE.
#' @param get_aux_verbs Should auxiliary verbs (e.g., can, could, may, must) be considered actions? Defaults to FALSE.
#' @param aux_verb_markup Should auxiliary verbs with "to" be marked up so that "going" in "going to eat" becomes "going-to".
#' Note that this will not affect cases of the sort "going to the bar." This can be useful for analyses concerning modality. Defaults to TRUE.
#' @param pron_as_ap Should pronouns be considered agents and patients? Defaults to FALSE.
#' @param use_appos Should things linked to an entity via an appositional modifier be considered as equivalent to the entity?
#' For example, if we specify our entity to be "Peter" in the sentence "My brother Peter left.", should "brother" be considered equivalent to "Peter"?
#' Only if use_appos = TRUE, we can extract "leaving" as action motif associated with Peter, as the subject associated with "leaving" is "brother". Defaults to TRUE.
#' @param lowercase Should all tokens and lemmas be lowercased? Defaults to FALSE.
#' @param verbose Should progress be reported during execution? Defaults to FALSE.
#' @param nominal_pattern Regular expression capturing patterns that point to nominalized verbs. Note: Even through the matching is restricted to NOUN and PROPN lemmas, this is not exact science and false positives might ensue. Validate the patterns in your specific applications!
#' @param nominal_exclude Regular expression containing nouns that match the respective nominal pattern but should nevertheless be excluded (application specific!)
#' @return A list with six dataframes, one for each motif class. List elements of motif classes not specified in the motif_classes parameter will be empty.
#' @export
#' @references
#' Stuhler, O. (2022) "Who Does What To Whom? Making Text Parsers Work for Sociological Inquiry." Sociological Methods and Research. <doi: 10.1177/00491241221099551>.
#' @examples
#' # Given data.frame with parsed sentence – as can be generated with spacyr::spacy_parse().
#' tokens_df = data.frame(doc_id = rep("text1", 4),
#'                        sentence_id = rep(1, 4),
#'                        token_id = 1:4,
#'                        token = c("Emil", "chased", "the", "thief"),
#'                        lemma = c("Emil", "chase", "the", "thief"),
#'                        pos = c("PROPN", "VERB", "DET", "NOUN"),
#'                        head_token_id = c(2,2,4,2),
#'                        dep_rel = c("nsubj", "ROOT", "det", "dobj")
#'                        )
#'
#' # Extract motifs around specific entities, here "Emil"
#' extract_motifs(tokens = tokens_df, entities = c("Emil"))
#'
#' # Extract all possible motifs
#' extract_motifs(tokens = tokens_df, entities = "*")

extract_actorness = function(tokens,
                          entities = "*",
                          motif_classes = c("a"),
                          custom_cols,
                          nominal_pattern = "(tion|sion|ment|ance|ence|al|ing|ure|age|ery|ory|cy|ship|hood|ism|ability|activity|attack|aid|agenda|answer|appeal|approach|attempt|auspices|authority|ban|boycott|breach|call|campaign|capability|capacity|competence|compromise|control|claim|cut|demand|dialogue|directive|effort|force|help|influence|initiative|interest|mandate|message|move|offer|order|plan|pledge|policy|power|push|reply|request|response|responsibility|search|strategy|support|strength|tie|threat|visit|warning|willingness)$", # Common endings of nominalized verbs plus a few so-called zero-derivation nominalizations (where noun equals the verb)
                          nominal_exclude = "(average|integration|government|development|enlargement|extension|membership|presidency|representative|emission|border)$",
                          fast = F,
                          parse_multi_token_entities = T,
                          extract = "lemma",
                          markup = F,
                          add_sentence = F,
                          add_paragraph = F,
                          verb_prep = F,
                          verb_prep_greedy = F,
                          be_entity = T,
                          get_aux_verbs = T,
                          aux_verb_markup = T,
                          pron_as_ap = F,
                          use_appos = T,
                          lowercase = F,
                          verbose = F,
                          perl = T){
  
  ###############################################################################################
  #####################################Pre-processing############################################
  ###############################################################################################
  
  ###############################################################################################
  ##### Text input
  if(missing("tokens")){
    stop("It seems you didn't provide a tokens object.", call. = FALSE)
  }
  if(missing("entities")){
    message("It seems you didn't specify any core entities to extract motifs around. Defaulting to entities = *.")
  }
  
  ###############################################################################################
  ##### If custom_cols is provided, adjust the columns
  if(!missing("custom_cols")){
    if(length(custom_cols) != 8){
      stop("You provided a custom columns vector of length other than 8.", call. = FALSE)
    } else {
      names(tokens)[which(names(tokens) == custom_cols[1])] = "doc_id"
      names(tokens)[which(names(tokens) == custom_cols[2])] = "sentence_id"
      names(tokens)[which(names(tokens) == custom_cols[3])] = "token_id"
      names(tokens)[which(names(tokens) == custom_cols[4])] = "token"
      names(tokens)[which(names(tokens) == custom_cols[5])] = "lemma"
      names(tokens)[which(names(tokens) == custom_cols[6])] = "pos"
      names(tokens)[which(names(tokens) == custom_cols[7])] = "head_token_id"
      names(tokens)[which(names(tokens) == custom_cols[8])] = "dep_rel"
    }
  }
  
  ###############################################################################################
  ##### Unify column labels in tokens dataframe
  ##### Development Note: this should be taken out and subsequent code adjusted.
  if("sentence_id" %in% names(tokens)){
    names(tokens)[which(names(tokens) == "sentence_id")] = "sentence"
  }
  if("dep_rel" %in% names(tokens)){
    names(tokens)[which(names(tokens) == "dep_rel")] = "relation"
  }
  
  ###############################################################################################
  ##### Set max preposition distance to the right for greedy preposition mode
  if(verb_prep_greedy){
    verb_prep_dist = 4
  } else {
    verb_prep_dist = 1
  }
  
  ###############################################################################################
  ##### Replace
  
  # Get all instances of entities that are multigrams
  if(T %in% str_detect(entities, " ") & parse_multi_token_entities == T){
    for(entity in entities){
      if(entity == entities[1]){
        phrase_df = data.frame()
        master_length = dim(tokens)[1]
      }
      
      entity_split = unlist(str_split(entity, pattern = " "))
      entity_length = length(entity_split)
      
      if(entity_length>1){
        if(verbose){cat("Replacing phrase: ", entity, "\n")}
        length_seq = 1:entity_length
        which_first_id = which(tokens[,"token"] == entity_split[1])
        
        for(rownumber in which_first_id-1){
          if(isTRUE(all.equal(unname(unlist(tokens[c(rownumber+length_seq), "lemma"])), entity_split))){
            for(token in length_seq){
              row_index = c(rownumber+token)
              if(nrow(phrase_df) == 0){
                phrase_df = rbind(phrase_df, c(tokens[row_index,],
                                               phrase_replacement = paste(entity_split, collapse = " ")),
                                  stringsAsFactors = F)
              } else {
                if(!paste(tokens[row_index,][,c("doc_id", "sentence","token_id")], collapse = "_") %in%
                   str_c(phrase_df$doc_id, phrase_df$sentence, phrase_df$token_id, sep = "_")){
                  phrase_df = rbind(phrase_df, c(tokens[row_index,],
                                                 phrase_replacement = paste(entity_split, collapse = " ")),
                                    stringsAsFactors = F)
                }
              }
            }
          }
        }
      }
    }
    
    # Merge in the phrase replacements
    if(exists("phrase_df") & nrow(phrase_df) > 0){
      tokens = merge(tokens, phrase_df[,c("doc_id", "sentence", "token_id", "phrase_replacement")],
                     by = c("doc_id", "sentence", "token_id"), all.x = T)
      tokens$token = ifelse(!is.na(tokens$phrase_replacement), tokens$phrase_replacement, tokens$token)
      tokens$lemma = ifelse(!is.na(tokens$phrase_replacement), tokens$phrase_replacement, tokens$lemma)
    } else {
      tokens$phrase_replacement = NA
    }
  } else {
    tokens$phrase_replacement = NA
  }
  if(T %in% str_detect(entities, " ") & parse_multi_token_entities == F){
    message("Warning: multi-token entities were detected but parsing them was set to FALSE.\n")
  }
  
  
  ###############################################################################################
  ##### Mark up tokens with eligible appos children
  if(use_appos){
    appos_child = tquery(token = entities, relation = "appos",
                         parents(pos = c("NOUN", "PROPN", "PRON"), NOT(token = entities),
                                 label = "appos_child", fill = F
                         )
    )
    
    tokens = annotate_tqueries(tokens, "appos_child", appos_child, overwrite = T, copy = F)
    tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  } else {
    tokens$appos_child = ""
  }
  
  
  ###############################################################################################
  ##### Set pronoun parameter
  if(pron_as_ap){
    agent_patient_pos = c("NOUN", "PROPN", "PRON")
    tokens$lemma = ifelse(tokens$lemma == "-PRON-", tokens$token, tokens$lemma)
  } else {
    agent_patient_pos = c("NOUN", "PROPN")
  }
  
  ###############################################################################################
  ##### Replace auxiliary phrases
  
  # Get going to, need to, etc. instances
  if(aux_verb_markup){
    if(verbose){cat("Replacing aux phrases.. ", "\n")}
    aux_phrases = list(c("going", "to"),
                       c("need", "to"), c("needed", "to"),
                       c("have", "to"), c("had", "to"),
                       c("ought", "to"))
    
    for(aux_phrase in 1:length(aux_phrases)){
      if(aux_phrase == 1){
        aux_phrase_df = data.frame()
        master_length = dim(tokens)[1]
      }
      
      aux_phrase_split = aux_phrases[[aux_phrase]]
      
      length_seq = 1:2
      which_first_id = which(tokens[,"token"] == aux_phrase_split[1])
      
      for(rownumber in which_first_id-1){
        if(isTRUE(all.equal(unname(unlist(tokens[c(rownumber+length_seq), "token"])), aux_phrase_split)) &
           tokens[rownumber+length_seq[2], "relation"] == "aux"){
          for(token in length_seq){
            row_index = c(rownumber+token)
            if(nrow(aux_phrase_df) == 0){
              aux_phrase_df = rbind(aux_phrase_df, c(tokens[row_index,],
                                                     aux_phrase_replacement = paste(aux_phrase_split, collapse = "-")),
                                    stringsAsFactors = F)
            } else {
              if(!paste(tokens[row_index,][,c("doc_id", "sentence","token_id")], collapse = "_") %in%
                 str_c(aux_phrase_df$doc_id, aux_phrase_df$sentence, aux_phrase_df$token_id, sep = "_")){
                aux_phrase_df = rbind(aux_phrase_df, c(tokens[row_index,],
                                                       aux_phrase_replacement = paste(aux_phrase_split, collapse = "-")),
                                      stringsAsFactors = F)
              }
            }
          }
        }
      }
    }
    
    # Merge in the phrase replacements
    if(exists("aux_phrase_df") & nrow(aux_phrase_df) > 0){
      tokens = merge(tokens, aux_phrase_df[,c("doc_id", "sentence", "token_id", "aux_phrase_replacement")],
                     by = c("doc_id", "sentence", "token_id"), all.x = T)
      
      
      tokens$token = ifelse(!is.na(tokens$aux_phrase_replacement), tokens$aux_phrase_replacement, tokens$token)
      tokens$lemma = ifelse(!is.na(tokens$aux_phrase_replacement), tokens$aux_phrase_replacement, tokens$lemma)
    } else {
      tokens$aux_phrase_replacement = ""
    }
  } else {
    tokens$aux_phrase_replacement = ""
  }
  
  ###############################################################################################
  ##### Set auxiliary markup parameter
  if(get_aux_verbs){
    if(!aux_verb_markup){
      message("You are extracting auxiliary verbs but aux_verb_markup is set to FALSE. This may not give ideal results.\n")
    }
    tokens$get_aux_verbs_par = "YES"
    verb_pos = c("VERB", "AUX")
  } else {
    tokens$get_aux_verbs_par = "NO"
    verb_pos = c("VERB")
  }
  
  ###############################################################################################
  ######################################Rule implementation######################################
  ###############################################################################################
  
  ###############################################################################################
  ############################################Action#############################################
  ###############################################################################################
  
  if("a" %in% motif_classes){
    if(verbose){cat("Extracting actions\n")}
    
    ###############################################################################################
    ##### Run fast rules
    if(!verb_prep){
      tryCatch({
        nsubj_act_conj = a_1(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(nsubj_act_conj) != 4){stop(nsubj_act_conj)}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_1). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_conj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      # Set other to empty
      nsubj_act_conj_prep_1 = 
        nsubj_act_conj_prep_2 = 
        nsubj_act_conj_prep_3 = 
        nsubj_act_conj_prep_4 = 
        nsubj_act_conj_prep_5 = 
        nsubj_act_conj_prep_6 = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      
      ##### Run fast prep rules (no nominalizations here!!!!)
    } else {
      tryCatch({
        nsubj_act_conj_prep_1 = a_1_prep_1(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist)
        if(ncol(nsubj_act_conj_prep_1) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_1_prep_1). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_conj_prep_1 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        nsubj_act_conj_prep_2 = a_1_prep_2(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist)
        if(ncol(nsubj_act_conj_prep_2) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_1_prep_2). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_conj_prep_2 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        nsubj_act_conj_prep_3 = a_1_prep_3(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist)
        if(ncol(nsubj_act_conj_prep_3) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_1_prep_3). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_conj_prep_3 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        nsubj_act_conj_prep_4 = a_1_prep_4(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist)
        if(ncol(nsubj_act_conj_prep_4) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_1_prep_4). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_conj_prep_4 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        nsubj_act_conj_prep_5 = a_1_prep_5(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist)
        if(ncol(nsubj_act_conj_prep_5) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_1_prep_5). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_conj_prep_5 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        nsubj_act_conj_prep_6 = a_1_prep_6(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist)
        if(ncol(nsubj_act_conj_prep_6) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_1_prep_6). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_conj_prep_6 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      # Set other to empty
      nsubj_act_conj <- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
    }
    
    
    
    ###############################################################################################
    ##### Run slow rules
    if(fast){
      nsubj_act_noun_conj_verb_conj =
        by_act =
        by_act_noun_conjunct =
        by_act_2 =
        by_act_2_1 =
        by_act_2_noun_conj =
        by_act_2_noun_conj_1 =
        xcomp_act_conj_verb =
        xcomp_act_conj_noun = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      
    } else {
      tryCatch({
        nsubj_act_noun_conj_verb_conj = a_2(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(nsubj_act_noun_conj_verb_conj) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_2). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        nsubj_act_noun_conj_verb_conj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act = a_3(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(by_act) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_3). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_noun_conjunct = a_4(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(by_act_noun_conjunct) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_4). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_noun_conjunct <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2 = a_5(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(by_act_2) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_5). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_2 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2_1 = a_6(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(by_act_2_1) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_6). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_2_1 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2_noun_conj = a_7(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(by_act_2_noun_conj) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_7). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_2_noun_conj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2_noun_conj_1 = a_8(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(by_act_2_noun_conj_1) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_8). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_2_noun_conj_1 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        xcomp_act_conj_verb = a_9(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(xcomp_act_conj_verb) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_9). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_verb <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        xcomp_act_conj_noun = a_10(tokens, entities, verb_pos, agent_patient_pos, extract)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_10). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      # From here: Nominalized action patterns added by CR
      
      tryCatch({
        pobj_nominal = a_11(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_11). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      tryCatch({
        conj_pobj_nominal = a_12(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_12). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      tryCatch({
        conj_conj_pobj_nominal = a_12.2(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_12). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      tryCatch({
        possessive_nominal = a_13(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_13). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      tryCatch({
        compound_nominal = a_14(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_14). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      tryCatch({
        nmod_nominal = a_15(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_15). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      tryCatch({
        nmod_conj_nominal = a_16(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_15). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      
      tryCatch({
        npadvmod_participle = a_17(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern)
        if(ncol(xcomp_act_conj_noun) != 4){stop()}
      }, error = function(e){
        message("There was an error in extracting action motifs (a_15). Some action motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
    }
  }
  
  
  ###############################################################################################
  ######################################Lowercase extractions####################################
  ###############################################################################################
  
  ###############################################################################################
  ##### Lowercase
  if(lowercase){
    tokens$token = tolower(tokens$token)
    tokens$lemma = tolower(tokens$lemma)
  }
  
  
  ###############################################################################################
  ###################################Combine the motifs to a list################################
  ###############################################################################################
  
  ###############################################################################################
  actions = if("a" %in% motif_classes){
    actions = rbind(nsubj_act_conj,
                    nsubj_act_conj_prep_1, 
                    nsubj_act_conj_prep_2, 
                    nsubj_act_conj_prep_3, 
                    nsubj_act_conj_prep_4, 
                    nsubj_act_conj_prep_5, 
                    nsubj_act_conj_prep_6, 
                    nsubj_act_noun_conj_verb_conj,
                    by_act,
                    by_act_noun_conjunct,
                    by_act_2,
                    by_act_2_1,
                    by_act_2_noun_conj,
                    by_act_2_noun_conj_1,
                    xcomp_act_conj_verb,
                    xcomp_act_conj_noun,
                    pobj_nominal,         # CR-added rules from here
                    conj_pobj_nominal,
                    conj_conj_pobj_nominal,
                    possessive_nominal,
                    compound_nominal,
                    nmod_nominal,
                    nmod_conj_nominal,
                    npadvmod_participle)
    if(lowercase){
      actions$Entity = tolower(actions$Entity)
      actions$action = tolower(actions$action)
    }
    
    if(nrow(actions)>0){
      actions$action = str_split(actions$action, " ")
      actions = with(actions,
                     {data.frame(lapply(actions[,c("doc_id", "ann_id", "Entity")], rep, times=lengths(action)),action=unlist(action))}
      )
    } else {character(0)}
  } else {character(0)}
  if(markup & length(actions) > 0){actions$markup = paste0("a_", actions$action)}
  if(add_sentence & length(actions) > 0){
    actions$sentence = mapply(retrieve_sentence, actions$doc_id, str_extract(actions$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }
  if(add_paragraph & length(actions) > 0){
    actions$paragraph = mapply(retrieve_paragraph, actions$doc_id, str_extract(actions$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }
  

  # Filter out nominalized actions in the nominal_exclude pattern
  if(!is.character(actions)){actions = actions[!grepl(nominal_exclude, actions$action, ignore.case = T), ]}
  
  
  ###############################################################################################
  ###########################################Return##############################################
  ###############################################################################################
  return(actions)
}




#################################################################################################
# Action rules / Functions ######################################################################
# Query and annotation in output of dependency parser ###########################################
#################################################################################################

# First comprises all ten rules developed by Stuhler (a_1:a_10) - slight adaptations, marked by comments
# Then my rules for relationships between Entity and nominalized action

# A1

###############################################################################################
##### Rule: nsubj act or conjuncted second verb
##### Example: "ENTITY asked Joe." (asked)
##### Example: "ENTITY called and asked Joe." (called, asked)
##### Note: Note that the current basic annotation scheme cannot distinguish between a dependent of the first
##### conjunct and a shared dependent of the whole coordination (see https://universaldependencies.org/u/dep/conj.html).
##### E.g. in "ENTITY called and Joe answered." "answered" would be picked up as action of ENTITY if we just took conj-dependents.
##### To prevent this, we add a not_children condition to avoid cases where an independent subject is named.

a_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
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
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A2 

###############################################################################################
##### Rule: nsubj with conjuncted second actor and possibly second verb
##### Example: "Joe and ENTITY called Steve" (called)
##### Example: "Joe and ENTITY called and asked Steve." (called, asked)

a_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "conj", label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"),  relation = "nsubj",
                        parents(pos = verb_pos,
                                label = "action", fill = F,
                                children(pos = verb_pos, relation = "conj", req = F,
                                         not_children(relation = "nsubj", depth = 1),
                                         label = "action", fill = F,
                                         children(get_aux_verbs_par = "YES",
                                                  pos = verb_pos, relation = "aux", req = F,
                                                  label = "action", fill = F
                                         ),
                                         children(get_aux_verbs_par = "YES",
                                                  pos = verb_pos, relation = "aux", req = F,
                                                  label = "action", fill = F
                                         )
                                )
                        )
                )
  )
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A3


###############################################################################################
##### Rule: Passive subject with by
##### Example: "Sue is asked by Entity." (asked)

a_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, 
                   appos_child = "appos_child"), 
                relation = "pobj",
                label = "Entity", fill = F,
                parents(pos = "ADP", 
                        lemma = c("by", "of", "with", "between", "among", "from"),   # NOTE: Expansion by CR ("The agreement , which was signed between ENTITY and XYZ) - false positives?
                        relation = c("agent", "prep"), # Generalization: "We received funding from the ENTITY."
                        parents(pos = "VERB",
                                label = "action", fill = F))
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A4

###############################################################################################
##### Rule: Passive subject with by and noun conjunct
##### Example: "Sue is asked by Steve and ENTITY." (asked)

a_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
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
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A5

###############################################################################################
##### Rule: Passive subject with by conjunction
##### Example: "Sue is called and asked by ENTITY." (called)

a_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                label = "Entity", fill = F,
                parents(pos = "ADP", 
                        lemma = c("by", "of", "with", "between", "among", "from"),   # NOTE: Expansion by CR ("The agreement , which was signed between ENTITY and XYZ) - false positives?
                        relation = c("agent", "prep"), # Generalization: "We received funding from the ENTITY."
                        parents(pos = c("VERB", "AUX"), relation = "conj",
                                parents(pos = "VERB",
                                        label = "action", fill = F)
                        )
                )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A6

###############################################################################################
##### Rule: Passive subject with by conjunction second verb
##### Example: "Sue is called and asked by ENTITY." (asked)

a_6 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                label = "Entity", fill = F,
                parents(pos = "ADP", 
                        lemma = c("by", "of", "with", "between", "among", "from"),   # NOTE: Expansion by CR ("The agreement , which was signed between ENTITY and XYZ) - false positives? 
                        relation = c("agent", "prep"), # Generalization: "We received funding from the ENTITY."
                        parents(pos = "VERB", relation = "conj",
                                label = "action", fill = F
                        )
                )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A7

###############################################################################################
##### Rule: Passive subject with by verb conjunction and noun conjunction
##### Example: "Sue is called and asked by Greg and ENTITY" (called)

a_7 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "pobj",
                        parents(pos = "ADP", 
                                lemma = c("by", "of", "with", "between", "among", "from"),   # NOTE: Expansion by CR ("The agreement , which was signed between ENTITY and XYZ) - false positives? 
                                relation = c("agent", "prep"), # Generalization: "We received funding from the ENTITY."
                                parents(pos = c("VERB", "AUX"), relation = "conj",
                                        parents(pos = "VERB",
                                                label = "action", fill = F
                                        )
                                )
                        )
                )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

# A8

###############################################################################################
##### Rule: Passive subject with by verb conjunction and noun conjunction (second verb)
##### Example: "Sue is called and asked by Greg and ENTITY." (asked)

a_8 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "pobj",
                        parents(pos = "ADP", 
                                lemma = c("by", "of", "with", "between", "among", "from"),   # NOTE: Expansion by CR ("The agreement , which was signed between ENTITY and XYZ) - false positives? 
                                relation = c("agent", "prep"), # Generalization: "We received funding from the ENTITY."
                                parents(pos = c("VERB"), relation = "conj",
                                        label = "action", fill = F
                                )
                        )
                )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

# A9

###############################################################################################
##### Rule: Verb with xcomp clause and its conjuncts
##### Example: "ENTITY wants to eat and drink." (eat, drink)
##### Note: not_children inserted is in order to avoid passiveness.

a_9 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"),
                        children(pos = "VERB", relation = "xcomp",
                                 not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                 not_children(relation = "nsubj"),
                                 label = "action", fill = F,
                                 children(pos = "VERB", relation = "conj", req = F,
                                          not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                          not_children(relation = "nsubj"),
                                          label = "action", fill = F,
                                          children(pos = "VERB", relation = "conj", req = F,
                                                   not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                                   not_children(relation = "nsubj"),
                                                   label = "action", fill = F
                                          )
                                 )
                        )
                        
                )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A10


###############################################################################################
##### Rule: Verb with xcomp clause and noun conjunct
##### Example: "Jack and ENTITY want to eat." (eat)
##### Note: not_children inserted in order to avoid passiveness.

a_10 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("PROPN", "NOUN", "PRON"), relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = "VERB", relation = "xcomp",
                                         not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                         not_children(relation = "nsubj"),
                                         label = "action", fill = F,
                                         children(pos = "VERB", relation = "conj", req = F,
                                                  not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                                  not_children(relation = "nsubj"),
                                                  label = "action", fill = F)
                                )
                        )
                )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}



# A11 - nom


###############################################################################################
##### Rule: Prepositional links between the ENTITY and nominalized verbs
##### Example: "We welcome the support of the ENTITY." (support)
##### Derived from one of Stuhler's rules for possession motifs

a_11 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
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
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}



# A12 - nom


###############################################################################################
##### Rule: Prepositional links between the ENTITY and nominalized verbs
##### ENTITY is conjoined (conj) to another noun which is the prepositional object (pobj) of a nominalized action
##### Example: "We welcome the support of the US and the ENTITY." (support)
##### Derived from one of Stuhler's rules for possession motifs
##### Note: deeper conj links where the entity is part of a longer list !?

a_12 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
    token = entities,  # Match the target entities
    pos = c("NOUN", "PROPN"), # Only the nouns (not full nounphrases)
    relation = "conj",  # It is conjoined to another noun
    fill = F,
    label = "Entity",
    
    # The "head" of the conjunction must be a pobj 
    parents(
      relation = "pobj",
      fill = F,
      
      # Ensure the pobj is part of a nominalization prepositional phrase
      parents(
        token = c("of", "by", "with", "between", "among", "from", "without"),
        relation = "prep",
        
        parents(
          pos = c("NOUN", "PROPN"),
          lemma__R = nominal_pattern,
          label = "action",
          fill = F
        )
      )
    )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A12.2

###############################################################################################
##### Rule: Prepositional links between the ENTITY and nominalized verbs
##### ENTITY is conjoined (conj) to two other nouns which are the prepositional object (pobj) of a nominalized action
##### Example: "We welcome the support of the US, the UK  and the ENTITY." (support)
##### Derived from one of Stuhler's rules for possession motifs
##### Note: deeper conj links where the entity is part of a longer list !?

a_12.2 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
    token = entities,  
    pos = c("NOUN", "PROPN"),  
    relation = "conj",  
    label = "Entity", fill = F,
    
    # One extra conj step before reaching pobj
    parents(
      relation = "conj",
      pos = c("NOUN", "PROPN"),
      fill = F,

      parents(
        relation = "pobj",
        fill = F,
        
        parents(
          token = c("of", "by", "with", "between", "among", "from", "without"),
          relation = "prep",
          
          parents(
            pos = c("NOUN", "PROPN"),
            lemma__R = nominal_pattern,
            label = "action",
            fill = F
          )
        )
      )
    )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}





# A13 - nom


###############################################################################################
##### Rule: ENTITY is in a direct  possessive relationship with a nominalized action
##### Example: "The ENTITY's support was crucial." (support)

a_13 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
    token = entities,  # Match the target entity (EU)
    pos = c("NOUN", "PROPN"),  # Ensure it's a noun or proper noun
    relation = "poss",  # Must be a possessive relationship
    label = "Entity", fill = F,
    
    # Parent must be a nominalized action (like "support", "agreement", etc.)
    parents(
      pos = c("NOUN", "PROPN"),
      lemma__R = nominal_pattern,  # Regex for nominalized actions
      label = "action",
      fill = F
    )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A14 - nom


###############################################################################################
##### Rule: ENTITY is a direct compound of a nominalized action.
##### Example: "ENTITY support is crucial." (support)

a_14 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
    token = entities,  
    pos = c("NOUN", "PROPN"),  
    relation = "compound",  
    label = "Entity", fill = F,
    
    # The noun it modifies must be a nominalized action
    parents(
      pos = c("NOUN", "PROPN"),
      lemma__R = nominal_pattern,
      label = "action",
      fill = F
    )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}



# A15 - nom


###############################################################################################
##### Rule: ENTITY is a direct nominal modifier of a nominalized action
##### Example: "The ENTITY and XYZ agreement is great." (agreement)

a_15 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
    token = entities,  
    pos = c("NOUN", "PROPN"),  
    relation = "nmod",  
    label = "Entity", fill = F,
    
    # The noun it modifies must be a nominalized action
    parents(
      pos = c("NOUN", "PROPN"),
      lemma__R = nominal_pattern,
      label = "action",
      fill = F
    )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}


# A16 - nom


###############################################################################################
##### Rule: ENTITY is a conjoint of direct nominal modifier of a nominalized action
##### Example: "The XYZ and ENTITY agreement is great." (agreement)

a_16 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
    token = entities,  
    pos = c("NOUN", "PROPN"),  
    relation = "conj",  # ENTITY is conjoined to another noun
    label = "Entity", fill = F,
    
    # The noun that ENTITY is conjoined with (XYZ)
    parents(
      pos = c("NOUN", "PROPN"),
      relation = "nmod",  # XYZ modifies a nominalized action
      fill = F,
      
      # The nominalized action it modifies
      parents(
        pos = c("NOUN", "PROPN"),
        lemma__R = nominal_pattern,  # Must be a nominalized action
        label = "action",
        fill = F
      )
    )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}



# A17 - 


###############################################################################################
##### Rule: Adverbial noun phrases where the ENTITY modifies a verb
##### Example: "The ENTITY-led efforts were a mess." (lead)

a_17 = function(tokens, entities, verb_pos, agent_patient_pos, extract, nominal_pattern){
  rule = tquery(
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
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

