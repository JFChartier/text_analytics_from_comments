library(magrittr)
#library(data.table)
library(stringr)
library(quanteda)
library(dplyr)
library(qdapRegex)
library(text2vec)


text_preprocessing <- function(text_in_claims, myFrenchStopwords = myFrenchStopwords, my_doc_count_min=2, my_doc_proportion_max=1, to_stemm = T, language=c("fr", "en"), spelling_checker=T, charNgram = 4){
  
  preprocesCorpus=myTextCleaner(text = text_in_claims)
  tokenizedCorpus=myTextTokenizer(text = preprocesCorpus, charNgram = charNgram, myFrenchStopwords = myFrenchStopwords, to_stemm = to_stemm, language = language, spelling_checker = spelling_checker)
  vectorizedCorpus=myTextVectorizer(tokenizedCorpus = tokenizedCorpus, my_doc_count_min = my_doc_count_min, my_doc_proportion_max = my_doc_proportion_max)
  return (list(tokenizedCorpus=tokenizedCorpus, vectorizedCorpus=vectorizedCorpus))
}


myTextVectorizer<-function(tokenizedCorpus, my_doc_count_min=0, my_doc_proportion_max=1){
  
  it_tokens = text2vec::itoken(tokenizedCorpus,
                               preprocessor = identity,
                               progressbar = FALSE)
  
  tokenizedCorpus=NULL
  #gc()
  
  vocab_tokens = text2vec::create_vocabulary(it_tokens) %>% text2vec::prune_vocabulary(doc_count_min = my_doc_count_min, doc_proportion_max = my_doc_proportion_max)
  
  dtm_tokens= text2vec::create_dtm(it_tokens, text2vec::vocab_vectorizer(vocab_tokens)) #%>% text2vec::normalize(., norm = "l2")
  
  return (dtm_tokens)
  
}


myTextTokenizer<-function(text, charNgram=4, myFrenchStopwords = NULL, to_stemm = T, language="fr", spelling_checker = T){
  
  #text=unique_word_in_comments
  #text=comments_from_agents$Commentaires_de_lagent
  
  # ce tokeniseur est fait a l'origine pour l'anglais
  tokenizedCorpus= quanteda::tokens(text, what="word", remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, split_hyphens = TRUE, remove_symbols=TRUE, remove_url = TRUE)
  
  # split token with "'"
  ## pour les mots anglais cette fonction n'a pas le meme sens
  tokenizedCorpus=lapply(tokenizedCorpus, function(x){stringr::str_split(string = x, pattern = "'") %>% unlist()}) %>%  quanteda::as.tokens(.)
  
  
  if (is.null(myFrenchStopwords) == F){
    myStopWords=unique(quanteda::stopwords(language))
    myStopWords<-c(myStopWords, myFrenchStopwords) %>% unique(.) %>% tolower()
  
    # filtrer selon un antidictionnaire et singleton
    tokenizedCorpus=quanteda::tokens_remove(tokenizedCorpus, case_insensitive = F, valuetype = "glob", pattern=myStopWords, min_nchar=3)
  }
  
  if (spelling_checker == T){
    dic_speller = ifelse (test = language == "fr", yes = "fr_FR", "en_US")
    
    tokenizedCorpus = spelling_correcter(tokenizedCorpus = tokenizedCorpus, dic_speller = dic_speller)
  }
  
  if (is.null(myFrenchStopwords) == F){
    myStopWords=unique(quanteda::stopwords(language))
    myStopWords<-c(myStopWords, myFrenchStopwords) %>% unique(.) %>% tolower()
    
    # filtrer selon un antidictionnaire et singleton
    tokenizedCorpus=quanteda::tokens_remove(tokenizedCorpus, case_insensitive = F, valuetype = "glob", pattern=myStopWords, min_nchar=3)
  }
  
  
  #racinisation
  if (to_stemm == T){
    tokenizedCorpus=quanteda::tokens_wordstem(tokenizedCorpus, language = language)
  }
  
  
  # remove accents
  # tokenizedCorpus=sapply(tokenizedCorpus, function(x){
  #  stringi::stri_trans_general(str = x, id = "Latin-ASCII")
  # }) %>% as.tokens(.)
  
  
  tokenizedCorpus=quanteda::as.list(tokenizedCorpus)
  
  if (charNgram>0){
    tokenizedCorpus=lapply(tokenizedCorpus, function(x) {
      text2vec::char_tokenizer(strings = paste0(x, collapse = "~"), xptr=T) %>% unlist(., recursive = F, use.names = F) %>% quanteda::char_ngrams(., n = 4) %>% c(., (x))
    })
    
  }
  
  return(tokenizedCorpus)
  
  
}

spelling_correcter <- function(tokenizedCorpus, dic_speller = "fr_FR") {
  
  #tokenizedCorpus <- quanteda::tokens(tokenizedCorpus)
  
  type_freq = quanteda::featfreq(quanteda::dfm(tokenizedCorpus, tolower =F, valuetype = "fixed"))
  
  # extract types to only work on them
  types <- names(type_freq)
  
  # spelling
  correct <- hunspell::hunspell_check(words = (types), dict = dic_speller)
  
  #find find_approved_words, i.e. more frequent than 2
  approuved_words = (type_freq > 2) & !correct
  
  pattern <- types[!correct & !approuved_words]
  replacement <- sapply(hunspell::hunspell_suggest(pattern, dict = dic_speller), function(x){x[1]})
  # if no replacement available, keep original
  replacement = ifelse(test = is.na(replacement), yes = pattern, no = replacement)
  
  
  types <- stringi::stri_replace_all_fixed(str = types, pattern = pattern, replacement = replacement, vectorize_all = F)
  
  
  # replace original tokens
  tokenizedCorpus_new <- quanteda::tokens_replace(x = tokenizedCorpus, pattern = quanteda::types(tokenizedCorpus), replacement = as.character(types), valuetype = "fixed")
  #tokenizedCorpus_new <- quanteda::tokens_remove(sent_t_new, pattern = "NULL", valuetype = "fixed")
  
  # #ajouter les tokens suspicieux
  # lapply(1:2, function(i){
  #   c(tokenizedCorpus_new[[i]], tokenizedCorpus[[i]])
  # })
  
  
  #sent_t_new <- quanteda::tokens_remove(sent_t_new, pattern = "NULL", valuetype = "fixed")
  
  return(tokenizedCorpus_new)
}




myTextCleaner<-function(text){
  
  preprocesCorpus = tolower(x = text) 
  preprocesCorpus = gsub("[^[:alnum:] ]", " ", preprocesCorpus, ignore.case = F)
  
  preprocesCorpus = gsub('[[:digit:]]+', '', preprocesCorpus)
  
  preprocesCorpus=stringr::str_squish(preprocesCorpus)
  return(preprocesCorpus)
  
}

getLatentVectorsOfComments <- function(vectorizedCorpus, svd_v, svd_d, originalFeatures){
  
  vectorizedCorpus = quanteda::as.dfm(vectorizedCorpus)
  vectorizedCorpus = quanteda::dfm_match(vectorizedCorpus, features = originalFeatures)
  
  myReducedQueryVector <-  vectorizedCorpus %*% svd_v %*% solve(diag((svd_d)))
  myReducedQueryVector=normRowVectors(myReducedQueryVector) %>% as.data.frame()
  colnames(myReducedQueryVector) = paste(colnames(myReducedQueryVector), "transLSAofClaim", sep = "")
  return (myReducedQueryVector)
}

#function to create a unit normed vector
normVector <- function(x){
  if(sum(x)==0)
    return (x)
  else 
    return (x / sqrt(sum(x^2)))
  
}
#function to norm many vectors
normRowVectors<-function(m){
  t(apply(m, MARGIN = 1, FUN = function(x) normVector(x)))
}



wordSelectionRoutine <- function(dfm, target, minDocFrequency = 0, coefficient = c("lr", "chi2")){
  
  
  specificityScore = quanteda::textstat_keyness(x = dfm, target=target, measure = coefficient)
  return (specificityScore)
  
}

get_most_relevant_words <- function(tokenizedCorpus=tokenizedCorpus, target=target, minDocFrequency=1, coefficient = c("lr", "chi2"), min_specificity_score = 0, p_value = 0.01){
  #tokenizedCorpus = tokens_test
  #target = (claims_2000_2017_select$EVENEMEN !="?") %>% .[1:5000]
  #remove accents
  # tokenizedCorpus=sapply(tokenizedCorpus, function(x){
  #   stringi::stri_trans_general(str = x, id = "Latin-ASCII")
  # })
  
  vectorizedCorpus=quanteda::dfm(x = quanteda::tokens(tokenizedCorpus), tolower = F, stem = F)
  vectorizedCorpus = quanteda::dfm_trim(vectorizedCorpus, min_docfreq = minDocFrequency)
  
  #vectorizedCorpus = quanteda::as.dfm(vectorizedCorpus)
  specificities = quanteda::textstat_keyness(x = vectorizedCorpus, target=target, measure = coefficient, sort=T)
  specificities = (specificities[, 2] > min_specificity_score & specificities[, 3] <= p_value) %>% specificities[., ]
  
  #topWords = specificities %>% subset(., p < 0.001) %>%  plyr::arrange(., desc(G2)) %>% .[1:n_top_word, c("feature")]
  #topWords= topWords[!is.na(topWords)]
  #return(topWords$feature) 
  return(unlist(specificities[,1]))
}




getLatentVectorsOfComments <- function(vectorizedCorpus, svd_v, svd_d, originalFeatures){
  
  vectorizedCorpus = quanteda::as.dfm(vectorizedCorpus)
  vectorizedCorpus = quanteda::dfm_match(vectorizedCorpus, features = originalFeatures)
  
  #myReducedQueryVector = projetNewDocumentsIntoLsaTrainedModel(matrixV = svd_v, singularValues = svd_d,  newData = dfm.by.SINI_DT_SINI)
  myReducedQueryVector <-  vectorizedCorpus %*% svd_v %*% solve(diag((svd_d)))
  myReducedQueryVector=normRowVectors(myReducedQueryVector) %>% as.data.frame()
  colnames(myReducedQueryVector) = paste(colnames(myReducedQueryVector), "transLSAofClaim", sep = "")
  return (myReducedQueryVector)
}