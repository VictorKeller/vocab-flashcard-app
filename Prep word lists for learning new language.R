#### Prep word lists for learning new language ####
library(stringr)
library(tidyverse)



main_df <- read.csv(file.choose(), stringsAsFactors = F, skip = 1)


new_df <- read.csv(file.choose(), stringsAsFactors = F, header = F)


main_df <- unique(main_df)



#### Functions ####


## remove numbering ####
remove_numbering <- function(variable, separator = ". ") {
  
  sapply(strsplit(variable, separator), function(x) x[2])
  
}





## split original text and translation ####
split_translation_list <- function(data, variable, separator = " - ") {
  
  first_part <- sapply(strsplit(data[[variable]], separator), function(x) x[1])
  
  second_part <- sapply(strsplit(data[[variable]], separator), function(x) x[2])
  
  split_df <- data.frame(first_part, second_part)
  
  return(split_df)
  
}




## fix special characters in whole dataframe ####
fix_special_chars <- function(data) {
  
  for (variable in names(data)) {
    
    data[[variable]] <- gsub("\xfc", "ü", data[[variable]])
    data[[variable]] <- gsub("<fc>", "ü", data[[variable]])
    data[[variable]] <- gsub("<e4>", "ä", data[[variable]])
    data[[variable]] <- gsub("\xe4", "ä", data[[variable]])
    data[[variable]] <- gsub("\xc4", "Ä", data[[variable]])
    data[[variable]] <- gsub("\xf6", "ö", data[[variable]])
    data[[variable]] <- gsub("<f6>", "ö", data[[variable]])
    data[[variable]] <- gsub("<d6>", "Ö", data[[variable]])
    data[[variable]] <- gsub("<df>", "ß", data[[variable]])
    
  }
  
  return(data)
  
}









###########



#### Actions #####

## fix entry ####

main_df$english <- ifelse(main_df$english == " apple fritter", "apple fritter", main_df$english)

main_df$english <- ifelse(main_df$english == " money", "money", main_df$english)

main_df$english <- ifelse(main_df$english == " shut up, enough already", "shut up, enough already", main_df$english)








## categorize as phrase or word ####

main_df$phrase_or_word <- ifelse(str_count(main_df$english, " ") > 1, "phrase", "word")






## add word ####
# main_df <- rbind(main_df, data.frame(english = "vegetarian", swiss_german = ""))





# ## find special characters ####
# new_df$V2 <- sapply(strsplit(new_df$V1, "<"), function(x) x[2])
# new_df$V2 <- sapply(strsplit(new_df$V2, ">"), function(x) x[1])







#########



##### Add new list to main list #####


new_df$V1 <- remove_numbering(new_df$V1, separator = "\\. ")

new_df <- split_translation_list(new_df, "V1", " - ")


names(new_df) <- c("swiss_german", "english")


new_df$swiss_german <- fix_special_chars(new_df$swiss_german)


main_df <- rbind(main_df, new_df)


main_df$swiss_german <- fix_special_chars(main_df$swiss_german)


main_df$english <- tolower(main_df$english)


main_df <- unique(main_df)


main_df <- filter(main_df, !is.na(english))






##### Compile all unique words #####

words <- NULL
for (i in 1:nrow(main_df)) {
  
  word <- gsub('[[:punct:] ]+', ' ', main_df$english[i])
  
  word <- strsplit(word, " ")[[1]]
  
  words <- c(words, word)
  
}

words <- unique(words)



main_df <- main_df %>% filter(str_count(english, " ") > 0)


main_df <- rbind(main_df, data.frame(english = words, swiss_german = rep("", length(words)), phrase_or_word = rep("word", length(words))))





##### Add word frequency ######

freqs <- read.csv(file.choose(), stringsAsFactors = F)




freqs <- split_translation_list(freqs, "word", separator = ": ")


names(freqs) <- c("english", "frequency")


freqs$frequency <- as.numeric(freqs$frequency)




main_df <- merge(main_df, freqs, all = T)





##### Add word translations #####

translations <- read.csv(file.choose(), stringsAsFactors = F)


translations$word <- fix_special_chars(translations$word)


translations <- split_translation_list(translations, "word", separator = " = ")


translations <- unique(translations)


names(translations) <- c("english", "german")



main_df <- merge(main_df, translations, all = T)








##### Add phrase translations #####

translations <- read.csv(file.choose(), stringsAsFactors = F)


translations$phrase <- fix_special_chars(translations$phrase)


translations <- split_translation_list(translations, "phrase", separator = " = ")


translations <- unique(translations)


names(translations) <- c("english", "german")


translations$english <- tolower(translations$english)



for (i in unique(main_df$english[main_df$phrase_or_word == "phrase" & is.na(main_df$german)])) {
  
  if (i %in% translations$english) {
    
    main_df$german[main_df$english == i] <- translations$german[translations$english == i]
    
  }
  
}





##### Repeated phrases with different translations #####


translations <- translations %>% 
  group_by(english) %>% 
  summarise(german = paste0(german, collapse = " / "))



main_df <- main_df %>% 
  group_by(english) %>% 
  summarise(
    phrase_or_word = phrase_or_word,
    frequency = frequency,
    swiss_german = paste0(swiss_german, collapse = " / "),
    german = paste0(german, collapse = " / "))


main_df <- unique(main_df)

main_df$german <- ifelse(main_df$german == "NA" | main_df$german == "NA / NA", NA, main_df$german)





##### Repeated words with different frequencies #####

main_df <- main_df %>% 
  group_by(english, german) %>% 
  summarise(
    phrase_or_word = phrase_or_word,
    swiss_german = swiss_german,
    frequency = mean(frequency, na.rm = T))


main_df <- unique(main_df)





## Save updated list ####

write.csv(main_df, "main list.csv", row.names = F)








