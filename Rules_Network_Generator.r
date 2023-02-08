# Rule graphs
library(igraph)
library(RColorBrewer)
library(visNetwork)
library(tidyverse)
library(jsonlite)

rules <- read.csv('rules_repository.csv')
json_list <- as.list(rules$parsed_rule)

STATES <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", 
            "PR")
SASFUNC <- c('=', '==', '+', '-', '*', '/', '>', '<', '>=', '<=', '~=', '&', 
             '|', 'and', 'or', 'sum', 'avg', 'min', 'max', 'count', 'mean', 
             'median', 'mode', 'round', 'abs', 'int', 'if', 'then', 'else', 
             'substr', 'in', 'Y', 'N', 'agg', 'concat', 'getTime', 'not')
SYMBOLS <- c('{', '}', '(', ')', '[', ']', '.', ',', ':', ';', '+', '-', '*', 
             '/', '&', '|', '<', '>', '=', '~', '$')

input_list <- list()
for (i in seq_along(json_list)) {
  rule <- fromJSON(json_list[[i]])
  rule_condition <- rule$conditions$all$params$conditionstring
  tokenized <- str_extract_all(rule_condition, 
                               '(\\b\\w*[\\.]?\\w+\\b|[\\(\\)\\+\\*\\-\\/])')
  new <- tokenized[[1]][!(tokenized[[1]] %in% c(SYMBOLS, SASFUNC, STATES))]
  x <- gsub("^[0-9]", "", new)
  new2 <- intersect(new,x)
  input_list[[i]] <- new2
}

output_list <- list()
for (i in seq_along(json_list)) {
  result_val <- list()
  rule <- fromJSON(json_list[[i]])
  rule_result <- colnames(rule$event$params$action)
  rule_result <- gsub("^\\s+", "", rule_result)
  rule_result <- sub("\\s+$", "", rule_result)
  output_list[[i]] <- rule_result
}

for (i in seq_along(output_list)) {
  if (length(output_list[[i]]) == 0) {
    output_list[[i]] <- "BLANK"
  }
}

for (i in seq_along(input_list)) {
  if (length(input_list[[i]]) == 0) {
    input_list[[i]] <- "BLANK"
  }
}

find_index = function(list1, list2) {
  result = numeric()
  result2 = numeric()
  for (i in 1:length(list1)) {
    for (j in 1:length(list2)) {
      if (all(list1[[i]] %in% list2[[j]])) {
        result = c(result, i)
        result2 = c(result2, j)
      }
    }
  }
  return(list(result, result2))
}

result <- find_index(output_list, input_list)

nodes = data.frame(v=c(1:length(json_list)))
edges = data.frame(from=result[[1]], to=result[[2]]) 
graph = graph_from_data_frame(d=edges, vertices=nodes, directed = TRUE)

indegree = degree(graph, mode = "in")
print(indegree)
outdegree = degree(graph, mode = "out")
print(outdegree)

classification = character(length(indegree))

for (i in 1:length(indegree)) {
  if (indegree[i] == 0 & outdegree[i] != 0){
    classification[i] = "Progenitor"
  } else if (indegree[i] != 0 & outdegree[i] != 0) {
    classification[i] = "Intermediary" 
  } else if (indegree[i] != 0 & outdegree[i] == 0) {
    classification[i] = "Terminal"   
  } else if (indegree[i] == 0 & outdegree[i] == 0) {
    classification[i] = "Isolated"   
  } else {
    "Mistake"
  }
}
V(graph)$group = classification

data = toVisNetworkData(graph)
nodes = data$nodes
edges = data$edges
visNetwork(nodes, edges, height = "750px", width = "750px") %>%
  visNodes(size = 20, font = list(size = 30)) %>%
  visEdges(arrows = "to") %>%
  visGroups(groupname = "Intermediary", color = "#00E9A3") %>%
  visGroups(groupname = "Progenitor", color = "#68D5FF") %>%
  visGroups(groupname = "Isolated", color = "#5D34B4") %>%
  visGroups(groupname = "Terminal", color = "#FF6746") 
