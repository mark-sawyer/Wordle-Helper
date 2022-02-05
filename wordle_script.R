
### Setup ----
library(tidyverse)
lengthen_words <- function(word_df) {
  word_df %>%
    mutate(
      `1`=str_sub(word, 1, 1),
      `2`=str_sub(word, 2, 2),
      `3`=str_sub(word, 3, 3),
      `4`=str_sub(word, 4, 4),
      `5`=str_sub(word, 5, 5)
    ) %>%
    pivot_longer(`1`:`5`, names_to="position", values_to="letter") %>%
    mutate(position=as.integer(position))
}
update_remaining_words <- function(guess_word, pattern_found) {
  if (str_length(guess_word) != 5 | str_length(pattern_found) != 5) {
    print("incorrect input")
    return(remaining_words)
  }
  
  guess_word <- str_to_lower(guess_word)
  guesses_so_far <<- guesses_so_far + 1L
  last_guess <<- guess_word
  last_pattern <<- pattern_found
  
  lengthen_words(remaining_words) %>%
    inner_join(
      tibble(
        position=1:5,
        guess_letter=str_split(guess_word, "")[[1]]
      ),
      by="position"
    ) %>%
    group_by(word) %>%
    mutate(
      outcome=case_when(
        guess_letter == letter ~ "1",
        guess_letter %in% letter[letter != guess_letter] ~ "2",
        TRUE ~ "3"
      )
    ) %>%
    summarise(pattern=str_c(outcome, collapse="")) %>%
    filter(pattern == pattern_found) %>%
    select(-pattern)
}
get_best <- function() {
  if (pull(count(words)) == pull(count(remaining_words)))
  {
    return(
      read.csv("get_best_first_turn.csv") %>%
        as_tibble() %>%
        select(-X) %>%
        mutate(remaining=TRUE) %>%
        filter(row_number() <= 10)
    )
  } else if (guesses_so_far == 1L & last_guess == "raise")
  {
    return(
      read.csv(str_c("Best After RAISE//", last_pattern, ".csv")) %>%
        as_tibble() %>%
        select(-X)
    )
  }
  
  lengthen_words(remaining_words) %>%
    inner_join(
      lengthen_words(words) %>%
        rename(guess="word", guess_letter="letter"),
      by="position"
    ) %>%
    group_by(guess, word) %>%
    mutate(
      outcome=case_when(
        guess_letter == letter ~ "1",
        guess_letter %in% letter[letter != guess_letter] ~ "2",
        TRUE ~ "3"
      )
    ) %>%
    summarise(pattern=str_c(outcome, collapse="")) %>%
    group_by(guess, pattern) %>%
    summarise(n=n()) %>%
    mutate(prob=n / sum(n)) %>%
    summarise(
      expectation=sum(prob * n),
      max=max(n)
    ) %>%
    mutate(remaining=guess %in% remaining_words[["word"]]) %>%
    arrange(expectation, desc(remaining)) %>%
    filter(row_number() <= 10)
}

words <- read.delim("wordle-answers-alphabetical.txt", header=FALSE) %>%
  as_tibble() %>%
  rename(word="V1")

remaining_words <- words

guesses_so_far <- 0L
last_guess <<- ""
last_pattern <- ""

### Play ----
# 1 = green
# 2 = yellow
# 3 = grey
# For example, a pattern of green-yellow-grey-grey-grey would be entered as 12333

# 1
get_best()
remaining_words <- update_remaining_words(guess_word="", pattern_found="")

# 2
get_best()
remaining_words <- update_remaining_words(guess_word="", pattern_found="")

# 3
get_best()
remaining_words <- update_remaining_words(guess_word="", pattern_found="")

# 4
get_best()
remaining_words <- update_remaining_words(guess_word="", pattern_found="")

# 5
get_best()
remaining_words <- update_remaining_words(guess_word="", pattern_found="")

# 6
get_best()
remaining_words <- update_remaining_words(guess_word="", pattern_found="")
