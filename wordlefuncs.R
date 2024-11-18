library(tidyverse)

# All 5 letter words that can be answers for NYT wordle
word_list <- readLines("wordlewords.txt")

# Determine result of guess given a target
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

# Entropy calculation
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

# Determining best start
get_entropies <- data.frame()

for (word in word_list) {
  
  print(word)
  
  ents <- calculate_entropy(word, word_list)
  
  get_entropies <- rbind(get_entropies, ents)
  
}

# Best start:
avg_entropies <- get_entropies %>%
  group_by(word) %>%
  summarize(avg = sum(Freq * log(1/Freq, 2))) %>%
  arrange(desc(avg))

write_csv(avg_entropies, here::here("avg_entropies.csv"))

