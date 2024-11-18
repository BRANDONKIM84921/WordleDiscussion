library(tidyverse)

word_list <- readLines("wordlewords.txt")

get_feedback <- function(guess, target, wordlen = 5) {
  guess <- toupper(guess)
  target <- toupper(target)
  
  feedback <- character(wordlen)
  
  target_matched <- logical(wordlen)
  
  for (i in 1:wordlen) {
    if (substring(guess, i, i)  == substring(target, i, i)) {
      feedback[i] <- "G"
      target_matched[i] <- TRUE  
    } else {
      feedback[i] <- "B"
    }
  }
  
  for (i in 1:wordlen) {
    if (feedback[i] == "B") {  
      for (j in 1:wordlen) {
        if (!target_matched[j] & substring(guess, i, i) == substring(target, j, j)) {
          feedback[i] <- "Y"
          target_matched[j] <- TRUE  
          break
        }
      }
    }
  }
  
  paste(feedback, collapse = "")
}

calculate_entropy <- function(word, word_list, get_avg = FALSE) {
  ents <- word_list %>% 
    sapply(function(w) get_feedback(word, w)) %>%
    table %>%
    data.frame() %>%
    mutate(word = word, Freq = Freq/sum(Freq)) |>
    rename(feedback = `.`)
  
  if (get_avg) {
    ents <- ents %>%
      group_by(word) %>%
      summarize(avg = sum(Freq * log(1/Freq, 2)))
  }
  
  ents
    
}

get_entropies <- data.frame()

for (word in word_list) {
  
  print(word)
  
  ents <- calculate_entropy(word, word_list)
  
  get_entropies <- rbind(get_entropies, ents)
  
}

avg_entropies <- get_entropies %>%
  group_by(word) %>%
  summarize(avg = sum(Freq * log(1/Freq, 2))) %>%
  arrange(desc(avg))

write_csv(avg_entropies, here::here("avg_entropies.csv"))

