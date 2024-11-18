library(tidyverse)

word_list <- readLines("wordlewords.txt")

get_feedback <- function(guess, target) {
  feedback <- character(5)
  
  target_matched <- logical(5)
  
  for (i in 1:5) {
    if (substring(guess, i, i)  == substring(target, i, i)) {
      feedback[i] <- "G"
      target_matched[i] <- TRUE  
    } else {
      feedback[i] <- "B"
    }
  }
  
  for (i in 1:5) {
    if (feedback[i] == "B") {  
      for (j in 1:5) {
        if (!target_matched[j] & substring(guess, i, i) == substring(target, j, j)) {
          feedback[i] <- "Y"
          target_matched[j] <- TRUE  
          break
        }
      }
    }
  }
  
  return(paste(feedback, collapse = ""))
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

calculate_entropy("salet", word_list, get_avg = TRUE)

avg_entropies <- get_entropies %>%
  group_by(word) %>%
  summarize(avg = sum(Freq * log(1/Freq, 2)))

