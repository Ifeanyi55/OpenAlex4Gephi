authorPubNodes <- function(keywords,pub_start_date,pub_end_date){
  
  keywords <- keywords
  pub_start_date <- pub_start_date
  pub_end_date <- pub_end_date
  
  # create search engine function
  search_engine <- function(keywords,pub_start_date,pub_end_date){
    suppressPackageStartupMessages(library(openalexR))
    suppressPackageStartupMessages(library(tidyverse))
    
    options(openalexR.mailto = "idiayeifeanyi@yahoo.com")
    
    # search engine
    works_search <- oa_fetch(
      entity = "works",
      title.search = keywords,
      cited_by_count = ">50",
      from_publication_date = pub_start_date,
      to_publication_date = pub_end_date,
      options = list(sort = "cited_by_count:desc"),
      verbose = FALSE
    )
    
    return(works_search)
    
  }
  
  search_data <- search_engine(keywords,pub_start_date,pub_end_date)
  
  
  # grab authors and group them according to collaboration
  authors_collaboration_groups <- list()
  for (i in 1:nrow(search_data)){
    authors_collaboration_groups[[i]] <- search_data$author[[i]][2]
  }
  
  # grab all authors
  all_authors <- c()
  for (i in 1:length(authors_collaboration_groups)) {
    all_authors <- c(all_authors,authors_collaboration_groups[[i]][[1]])
  }
  
  # get length of each authors collaboration
  authors_length <- c()
  for(authors in 1:length(authors_collaboration_groups)){
    authors_length <- c(authors_length,authors_collaboration_groups[[authors]] |> nrow())
  }
  
  # grab all publications
  publications <- list()
  for (i in 1:nrow(search_data)){
    publications[[i]] <- rep(search_data$display_name[i], each = authors_length[i])
  }
  
  # place all publications in a vector
  all_publications <- c()
  for(i in 1:length(publications)){
    all_publications <- c(all_publications,publications[[i]])
  }
  
  # create author_to_publication data frame
  authors_to_publications <- data.frame(
    Authors = all_authors,
    Publications = all_publications
  )
  
  # stack the df so that authors and publications
  # are together as one column
  stacked_df <- stack(authors_to_publications)
  stacked_df <- unique.data.frame(stacked_df) # remove duplicate rows
  stacked_df <- stacked_df[-2] # delete second column in df
  
  # create author_publications_nodes df
  author_publication_nodes <- data.frame(
    Id = 1:nrow(stacked_df),
    Nodes = stacked_df$values,
    Label = stacked_df$values
  )
  
  
  return(author_publication_nodes)
  
  
}