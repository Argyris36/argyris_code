# code to get citations per day


#remotes::install_github("jkeirstead/scholar")
library(scholar)
library(tidyverse)

# Function to fetch citations for the current date
fetch_citations <- function(scholar_id) {
  citations <- last(get_citation_history(scholar_id))[[2]]
  return(citations)
}

# Function to update the dataframe with the latest citation count for today
update_citations_df <- function(df, scholar_id) {
  # Fetch the latest citation count
  citations <- fetch_citations(scholar_id)
  
  # Get today's date
  today <- Sys.Date()
  
  # Check if today's date already exists in the dataframe
  if (today %in% df$date) {
    # Update the citation count for today
    df$citations[df$date == today] <- citations
  } else {
    # Add a new row for today with the citation count
    new_row <- data.frame(date = today, citations = citations)
    df <- bind_rows(df, new_row)
  }
  
  return(df)
}

# Example usage
scholar_id <- "9B82424AAAAJ"  # Replace with your Google Scholar ID

# Load existing dataframe or create an empty one if it doesn't exist
if (file.exists("citations_df.csv")) {
  citations_df <- read.csv("citations_df.csv", stringsAsFactors = FALSE)
  citations_df$date <- as.Date(citations_df$date)
} else {
  citations_df <- data.frame(date = as.Date(character()), citations = integer())
}

# Update the dataframe with the latest citation count for today
citations_df <- update_citations_df(citations_df, scholar_id)

# Save the updated dataframe
write.csv(citations_df, "citations_df.csv", row.names = FALSE)

read.csv("citations_df.csv")

citations_df$end_date <- rep("2024-12-31",nrow(citations_df))

citations_df$dates_for_citations <- 365 - as.numeric(as.Date(citations_df$end_date) - as.Date(citations_df$date))  

citations_df$citations_by_days <- citations_df$citations/citations_df$dates_for_citations*365
citations_df$citations_by_days <- citations_df$citations_by_days

paste("Your number of citations so far for this year, today on", Sys.Date() , "is", last(citations_df$citations))
paste("Your predicted number of total citations for this year, today on", Sys.Date() , "is", round(
  last(citations_df$citations_by_days)))



plot_increase <- citations_df %>%
  ggplot(aes(date, citations)) +
  geom_point()+
  ggtitle("number of citations this year")+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  theme_minimal()

running_avg_citations_by_days <- 0
for(i in 1: length(citations_df$citations_by_days)){
  
  running_avg_citations_by_days[i] <-  mean(citations_df$citations_by_days[1:i])
  
}

citations_df$running_avg_citations_by_days <- running_avg_citations_by_days 

plot_per_day <- citations_df %>%
  ggplot(aes(date, citations_by_days)) +
  geom_point()+
  ggtitle("predicted number of citations by end of year")+
  ylim(2500, 3500) +
  geom_hline(yintercept = round(mean(running_avg_citations_by_days)),colour = "red",  linetype = "dashed")+
  annotate("text", x = as.Date("2024-05-20"), y = round(mean(running_avg_citations_by_days)) +300, label = 
             paste0("Daily Projected\n Total Citations this Year: ",  
                    round(mean(running_avg_citations_by_days)))) +
  ylab("predicted total citations this year") +
  theme_minimal()



plot_histogram <- citations_df %>%
  ggplot(aes(running_avg_citations_by_days)) +
  geom_histogram()+
  ggtitle("running average of citations", subtitle = "median in red")+
  geom_vline(xintercept = round(median(running_avg_citations_by_days)),colour = "red",  linetype = "dashed")+
  theme_minimal()


library(patchwork)

(plot_increase + plot_per_day)/plot_histogram




# function to get prediction of h-index -----------------------------------


# predict_h_index <- function(id, journals = NULL) {
#   id <- tidy_id(id)
#   
#   # Getting the h-index and checking for NA
#   h <- get_profile(id)$h_index
#   if (is.na(h)) return(NA)
#   
#   n <- get_num_articles(id) # number of articles written
#   y <- as.numeric(format(Sys.Date(), "%Y")) - get_oldest_article(id)
#   j <- get_num_distinct_journals(id)
#   
#   # Handle optional journals parameter
#   if (is.null(journals)) {
#     q <- get_num_top_journals(id)
#   } else {
#     q <- get_num_top_journals(id, journals)
#   }
#   
#   # Regression coefficients
#   coefs <- c(
#     1, 0.760, 0.373, 0.967, -0.069, 0.018, 0.033,
#     2, 1.413, 0.781, 0.936, -0.132, 0.018, 0.064,
#     3, 2.227, 1.105, 0.903, -0.193, 0.027, 0.096,
#     4, 3.196, 1.386, 0.871, -0.274, 0.039, 0.145,
#     5, 3.997, 1.578, 0.858, -0.345, 0.063, 0.198,
#     6, 4.752, 1.671, 0.817, -0.377, 0.117, 0.282,
#     7, 5.741, 1.761, 0.761, -0.420, 0.170, 0.394,
#     8, 6.531, 1.796, 0.669, -0.420, 0.252, 0.508,
#     9, 7.482, 1.653, 0.561, -0.415, 0.383, 0.629,
#     10, 8.734, 1.326, 0.478, -0.411, 0.522, 0.823
#   )
#   coefs.m <- matrix(coefs, nrow=10, byrow=TRUE)
#   coefs <- coefs.m[,-1]
#   vals <- c(1, sqrt(n), h, y, j, q)
#   
#   # Calculate the h-index predictions
#   h.pred <- coefs %*% vals
#   h.vals <- c(h, h.pred)
#   
#   # Check for sensible values
#   standard.warning <- "You're probably not a neuroscientist. Please read the documentation for information on the limitations of this function."
#   
#   if (any(diff(h.vals) < 0)) {
#     warning(paste0("Decreasing h-values predicted. ", standard.warning))
#   }
#   
#   if (any(h.vals < 0)) {
#     warning(paste0("Negative h-values predicted. ", standard.warning))
#   }
#   
#   return(data.frame(years_ahead = 0:10, h_index = h.vals))
# }