library(XML)
library(rvest)

url_part_1 <- "http://results.eci.gov.in/pc/en/constituencywise/Constituencywise"
url_part_2 <- ".htm?ac="

constituencies_data <- read.table('states_and_constituencies_list.csv',sep = ";", header = TRUE)
candidatewise_results_df = data.frame(Candidate.Name=character(), Party.Name=character(), EVM.Votes=numeric(), Postal.Votes=numeric(), Total.Votes=numeric(), Percent.of.Votes=numeric(), Constituency.ID=numeric(), Constituency.Name=character(), State.ID=character(), State.Name=character())

for (i in 1:nrow(constituencies_data)) {
  state_id <- constituencies_data$State.ID[i]
  state_name <- constituencies_data$State.Name[i]
  
  constituency_id <- constituencies_data$Constituency.ID[i]
  constituency_name <- constituencies_data$Constituency.Name[i]
  
  url <- paste0(url_part_1, state_id, constituency_id, url_part_2, constituency_id)
  print(url)
  webpage <- read_html(url)
  df <- html_nodes(webpage, "table")[11] %>% html_table(fill = TRUE)

  temp <- as.data.frame(df)
  res <- temp[3: (nrow(temp)-1), 2:(ncol(temp)-2) ]
  
  res$Constituency.ID <- constituency_id
  res$Constituency.Name <- constituency_name
  
  res$State.ID <- state_id
  res$State.Name <- state_name
    
  names(res) <- names(candidatewise_results_df)
  candidatewise_results_df <- rbind(candidatewise_results_df, res)
}

write.csv(candidatewise_results_df, file="elections_candidatewise_results.csv")
