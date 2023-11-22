
# search keywords and patterns in pdf -------------------------------------

library(pdfsearch)


#### create a function

pdf_search_func <- function(keyword, file ){
  
  result <- list()
  
  for(i in 1: length(file)) { 
    
    file <- file
    result[[i]] <- keyword_search(file[[i]], 
                                  keyword = c(keyword),
                                  path = TRUE)
  }
  return(result)
}







#now using the function to extract all pdf files in my current directory
library("rebus")

#for example to get the following from my current folder
files <- list.files(path="/Users/stringarisa/argyris_code/", pattern= ".pdf" %R% END , all.files=TRUE, 
                    full.names=TRUE)


files <- list.files(path="/Users/stringarisa/Desktop", pattern= ".pdf" %R% END , all.files=TRUE, 
                    full.names=TRUE)

#for these keywords
keywords <- c("social", "three", "treatment")
keywords <- ("Keren\\w\\s.\\nStringaris")

pdf_search_func(keywords,files )
