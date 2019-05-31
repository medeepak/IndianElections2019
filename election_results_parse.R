library(XML)
library(rvest)

url <- '/Users/deepaksr/elections.html'
webpage <- read_html(url)

states_list_xml_nodes <- html_nodes(webpage, "table")[7] %>% html_table(fill = TRUE)
state_ids_from_xml_nodes <- html_nodes(webpage, "table")[7] %>% xml_find_all("//tbody//td//select//option") %>% xml_attr("value")
state_names_from_xml_nodes <- html_nodes(webpage, "table")[7] %>% xml_find_all("//tbody//td//select//option") %>% xml_text()
states_df <- as.data.frame(cbind(state_ids_from_xml_nodes, state_names_from_xml_nodes))

#Remove NA and Select State option from result
states_df <- subset(states_df, !is.na(state_ids_from_xml_nodes))
states_df <- subset(states_df, state_ids_from_xml_nodes != "Select State")


state_and_constituency_node_list <- as.list(html_nodes(webpage, "input"))

constituencies <- state_and_constituency_node_list %>% xml_find_all("//input//@value")
state_ids <- state_and_constituency_node_list %>% xml_find_all("//input//@id")
#Remove ampersands from text
constituencies <- gsub("amp;", "", constituencies)

state_and_constituency_df = data.frame(Constituency.ID=character(), Constituency.Name=character(), State.ID=factor())

for(iter in 1:length(constituencies)) {
  temp_df <- as.data.frame(table(noquote(strsplit(gsub( "value=\"","", as.character(constituencies)), ";")[iter])))
  temp_df <- subset(temp_df, Var1 != "\"")

  temp_df <- mutate(temp_df, Constituency.ID=lapply(strsplit(as.character(temp_df$Var1), split=","),head,n=1))
  temp_df <- mutate(temp_df, Constituency.Name=trimws(lapply(strsplit(as.character(temp_df$Var1), split=","),tail,n=1)))

  state_id_list <- rep(names(table(noquote(strsplit(gsub( '[id="]',"", as.character(state_ids)), ";")[iter]))), nrow(temp_df))

  temp_df <- cbind(temp_df[3:4], trimws(state_id_list))
  state_and_constituency_df <- rbind(state_and_constituency_df, temp_df)
}

for(states_id_iter in 1:nrow(state_and_constituency_df)) {
  state_and_constituency_df[states_id_iter,4] <- states_df[states_df$state_ids_from_xml_nodes==trimws(state_and_constituency_df[states_id_iter,3]),2]
}

names(state_and_constituency_df)[3] <- "State.ID"
names(state_and_constituency_df)[4] <- "State.Name"

state_and_constituency_df <- apply(state_and_constituency_df,2,as.character)
write.csv2(state_and_constituency_df, file="states_and_constituencies_list.csv")







