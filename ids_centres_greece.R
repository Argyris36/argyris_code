
# a function for creating ids ---------------------------------------------

#let there be n = 20 centres (foreis) with the following names
n_centres <- 20
names_centres <- LETTERS[1:n_centres]

# let there be the folloowing ids available to each centre
list_ids <- list()
for (i in 1:n_centres) {
  ids <- sample(10000:20000, 200, replace = FALSE)
  ids <- paste0(names_centres[i], "_", ids)
  list_ids[[i]] <- ids
  
}
names(list_ids) <- names_centres




create_groups_from_ids_func <- function(ids, n) {
  # Sample ids without replacement
  sampled_ids <- sample(ids, n, replace = FALSE)
  
  # Split sampled ids into two groups
  group_A <- sample(sampled_ids, round(length(sampled_ids) / 2), replace = FALSE)
  group_B <- sampled_ids[!sampled_ids %in% group_A]
  
  return(list(group_A, group_B))
}

# this is an example of how to use the function
# for group C, let there be n = 17 people
n <- 17
create_groups_from_ids_func(list_ids[["C"]], 17)
