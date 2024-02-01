# create search engine
search_engine <- function(keywords,pub_start_date,pub_end_date){
  suppressPackageStartupMessages(library(openalexR))
  suppressPackageStartupMessages(library(tidyverse))
  
  options(openalexR.mailto = "idiayeifeanyi@yahoo.com")
  
  # search engine
  works_search <- oa_fetch(
    entity = "works",
    title.search = c(keywords),
    cited_by_count = ">50",
    from_publication_date = pub_start_date,
    to_publication_date = pub_end_date,
    options = list(sort = "cited_by_count:desc"),
    verbose = FALSE
  )
  
  return(works_search)
  
}


# disp_data <- search_engine(keywords = c("machine learning"),
#                                 pub_start_date = "2021-12-01",
#                                 pub_end_date = "2022-08-31")
# 
# 
# disp_data |> view()
# disp_data |> str()
# str(disp_data)
# str(disp_data$author)
# disp_data$author[[1]][3]
