library(tidyverse)
library(tidytext)
library(tm)
library (topicmodels)
library(lda)
library(tictoc)
library(fs)
library(fuzzyjoin)

prob <- read_csv("data/MaxGamma2_DOI_Included_Nov10_2022.csv", locale = locale("sv") )

#current fit
load("data/220625_topic_models_gibbs.RData")



new_fit <- topicNumber.TM[[3]] |> 
    slot("gamma") |> # take the gamma matrix
    as_tibble() |> # convert to data frame
    # add a column with paper titles
    add_column(docs = topicNumber.TM[[3]] |> slot("documents")) |> 
    # make all gamma values into one column for filtering later
    pivot_longer(cols = starts_with("V"), names_to = "topic", values_to = "gamma" ) |> 
    # select only topic 1
    # filter(topic == "V1") |> 
    group_by(docs) |> 
    filter(gamma == max(gamma))
# arrange by descending gamma value (the prob of a document belonging to a topic)
# arrange(desc(gamma)) |> 
# # print only the top 10
# top_n(10)
# 


#old fit: 
load("data/211020_topic_models_gibbs.RData")
old_fit <- topicNumber.TM[[3]] |> 
    slot("gamma") |> # take the gamma matrix
    as_tibble() |> # convert to data frame
    # add a column with paper titles
    add_column(docs = topicNumber.TM[[3]] |> slot("documents")) |> 
    # make all gamma values into one column for filtering later
    pivot_longer(cols = starts_with("V"), names_to = "topic", values_to = "gamma" ) |> 
    # select only topic 1
    # filter(topic == "V1") |> 
    group_by(docs) |> 
    filter(gamma == max(gamma))

## Comparison with All documents in prob file
new_fit$docs[!new_fit$docs %in% prob$Document_OLD_ALL] |> length() # 5261
prob$Document_OLD_ALL[!prob$Document_OLD_ALL %in% new_fit$docs] |> length() # 6108

## Comparison with coded documents in prob file (different file)
new_fit$docs[!new_fit$docs %in% prob$Document_OLD_INCL] |> length() # 56725
prob$Document_OLD_INCL[!prob$Document_OLD_INCL %in% new_fit$docs] |> length() # 1912

## The require merge is then between coded documents and new fit
prob$Document_OLD_INCL[!prob$Document_OLD_INCL %in% new_fit$docs] |> length() # 1912

## Extract the 1912 problematic titles
probs <- prob[!prob$Document_OLD_INCL %in% new_fit$docs,] |> 
    select(Document_OLD_INCL, Topic_OLD_INCL) |> 
    filter(!is.na(Topic_OLD_INCL)) |> 
    rename(docs = Document_OLD_INCL) |> 
    select(-Topic_OLD_INCL)

## The problems are reduced to 1852, I removed all documents that do not have old topic
## since they are not matched by name, they do not have new topic either. There are a lot of 
## problematic titles there that I don't think are real documents, such as "Time" or "Italy".
## Probably human errors.

## transform non-ascii characters:
probs <- probs |> 
    mutate(docs = str_replace_all(docs, "[^[:ascii:]]", replacement = "_")) |> 
    mutate(docs = str_replace_all(docs, "_{2,}", "_")) |>  #remove repetitions of _
    mutate(len = str_length(docs)) 

probs |> #ggplot(aes(len)) + geom_density()
    arrange(len) |> print(n = 30) # now min length is > 20

prob |> 
    rename(docs = Document_OLD_INCL) |> 
    filter(is.na(Topic_OLD_INCL)) |> 
    select(docs) |> print(n = 61)

# There are 5 docs with more than 10 weird characters, with max 17. The problem
# is the higher the threshold for differences, the higher the likelihood that two
# titles that do not correspond are matched.
probs |> 
    mutate(weird = str_count(docs, "_")) |> 
    arrange(desc(weird))

tic()
fuzzy_match <- probs |> 
    stringdist_left_join(new_fit, max_dist = 10)
toc() #1247.888 sec elapsed / 20min

fuzzy_match |> 
    group_by(docs.x) |> 
    add_count() |> 
    arrange(desc(n))


save(fuzzy_match, file = "data/fuzzy_match.RData")
