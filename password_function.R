
pword_generator <- function (seed, n_total, n_upper, n_number, n_character){
  
set.seed(seed)
  
lower_case <- sample(letters, n_total-(n_upper+ n_number + n_character))

upper_case <- sample(LETTERS, n_upper)

a_number <- sample(0:9,n_number)

a_character <- sample (c("!", "\"", "#", "$", "%", "&", 
  "'", "(", ")", "*", "+", ",", "-", 
  ".", "/", ":", ";", "", "?", "@", 
  "[", "]", "^", "_", "{", "|", "}", "~"),n_character)


pword <- sample(c(lower_case, upper_case, a_number, a_character), size = n_total, replace = FALSE)

pword <- cat(pword, sep = "")

}

# eg
pword_generator(270874, 12, 1, 1, 2)


