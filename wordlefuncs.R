library(tidyverse)

# All 5 letter words that can be answers for NYT wordle
word_list <- readLines("wordlewords.txt")

# Determine result of guess given a target
get_feedback <- function(guess, target, word_len = 5) {
  guess <- toupper(guess)
  target <- toupper(target)
  
  feedback <- character(word_len)
  
  target_matched <- logical(word_len)
  
  for (i in 1:word_len) {
    if (substring(guess, i, i)  == substring(target, i, i)) {
      feedback[i] <- "G"
      target_matched[i] <- TRUE  
    } else {
      feedback[i] <- "B"
    }
  }
  
  for (i in 1:word_len) {
    if (feedback[i] == "B") {  
      for (j in 1:word_len) {
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
calculate_entropy <- function(word, word_list, word_len = 5, get_avg = FALSE) {
  ents <- word_list %>% 
    sapply(function(w) get_feedback(word, w, word_len)) %>%
    table %>%
    data.frame() %>%
    mutate(word = word, Freq = Freq/sum(Freq)) |>
    rename(feedback = `.`)
  
  if (get_avg) {
    ents <- ents %>%
      group_by(word) %>%
      summarize(average_bits = sum(Freq * log(1/Freq, 2)))
  }
  
  ents
    
}

# Determining best start (takes a long time)
get_entropies <- data.frame()

for (word in word_list) {
  
  ents <- calculate_entropy(word, word_list)
  
  get_entropies <- rbind(get_entropies, ents)
  
}

# Best start:
avg_entropies <- get_entropies %>%
  group_by(word) %>%
  summarize(avg = sum(Freq * log(1/Freq, 2))) %>%
  arrange(desc(avg))

# Trying Other common words
avg_entropies <- avg_entropies %>%
  rbind(calculate_entropy("soare", word_list, get_avg = TRUE),
        calculate_entropy("salet", word_list, get_avg = TRUE)) %>%
  arrange(desc(avg))

# Writing
write_csv(avg_entropies, here::here("avg_entropies.csv"))

