# code to get citations per day

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



citations_df$end_date <- rep("2024-12-31",nrow(citations_df))

citations_df$dates_for_citations <- 365 - as.numeric(as.Date(citations_df$end_date) - as.Date(citations_df$date))  

citations_df$citations_by_days <- citations_df$citations/citations_df$dates_for_citations*365

paste("Your number of citations so far for this year, today on", Sys.Date() , "is", last(citations_df$citations))
paste("Your predicted number of total citations for this year, today on", Sys.Date() , "is", round(
  last(citations_df$citations_by_days)))



citations_df %>%
  ggplot(aes(date, citations)) +
  geom_point()+
  ggtitle("number of citations per day so far this year")

avg_citations_per_day <- mean(citations_df$citations_by_days)
avg_citations_per_day
citations_df %>%
  ggplot(aes(date, citations_by_days)) +
  geom_point()+
  ggtitle("predicted total number of citations by end of year")+
  ylim(2500, 3500) +
  geom_hline(yintercept = avg_citations_per_day,colour = "red",  linetype = "dashed")+
  annotate("text", x = as.Date("2024-05-20"), y = avg_citations_per_day+500, label = 
             paste0("Average Projected\n Total Citations this Year\n obtained on ", format(Sys.Date(), format="%B %d %Y"), ":\n ",  round(avg_citations_per_day)))
  




# put this into the terminal
#/Users/stringarisa/argyris_code/daily_citations.R
# crontab -e
# 44 22 * * * /usr/local/bin/Rscript /Users/stringarisa/argyris_code/daily_citations.R > test_scheduling
#Esc, type :w and hit Enter
# Quit vim: :q
# List your cron jobs: crontab -l. Your command should now be listed.