library(tidyverse)
library(tidytext)
library(tm)
library (topicmodels)
library(lda)
library(tictoc)
library(network)
library(patchwork)

# not working, need manual download
# dat <- read_csv(file = "https://stockholmuniversity.box.com/s/uoii9orvhkoijk1hzg4ahqwdydwhuxlb")
# "https://api.box.com/2.0/files/uoii9orvhkoijk1hzg4ahqwdydwhuxlb"
# dat <- read_csv(file = "data/April27_1-2000.csv") %>% 
#     janitor::clean_names()

fls <- list.files(path = "data/lit_review/")

dat_xls <- paste0("data/lit_review/", str_subset(fls, ".xlsx")) %>% 
    map(readxl::read_excel) %>% 
    map(janitor::clean_names) %>% 
    map(function(x) x %>% select(abstract, title, year, doi) %>% as_tibble()) %>% 
    map(function(x) x %>% mutate(across(.cols = everything(), .fns = as.character))) %>% 
    bind_rows()

dat_csv  <- paste0("data/lit_review/", str_subset(fls, ".csv")) %>% # head() %>% 
    map(read.csv) %>% 
    map(janitor::clean_names) %>% 
    map(function(x) x %>% select(abstract, title, year, doi) %>% as_tibble()) %>% 
    map(function(x) x %>% mutate(across(.cols = everything(), .fns = as.character))) %>% 
    bind_rows()

dat <- bind_rows(dat_csv, dat_xls)
rm(dat_csv, dat_xls)

## clean data: extra words
too_words <- tibble(
    word = c("john", "wiley", "sons", "springer", "verlag", "b.v", "abstract", "press", "reserved", "rights", "author", "taylor", "francis", "elsevier", "i.e.", "e.g.", "publisher", "publishers", "published", "publishing", "ii", "iv", "mdpi", "copyright", "journal", "auhors", "blackwell", "oxford", "cambridge", "publisher", "university", "book", "volume", "gmbh")
)

dtm <- dat %>% 
    select(abstract, title, year) %>%
    filter(!is.na(abstract)) %>%
    unique() %>% #
    unnest_tokens(word, abstract) %>% 
    filter(!is.na(word)) %>% ## 
    anti_join(stop_words) %>% 
    anti_join(too_words) %>%
    filter(!str_detect(word, "[:digit:]")) %>%
    mutate(word = textstem::lemmatize_words(word)) %>% 
    group_by(title) %>%
    count(word, sort = TRUE) %>%
    cast_dtm(document = title, term = word, value = n)

dtm # 73622 records

# I want to know how many records are in total, and if they were all included on the dtm
fls %>% 
    str_split("_") %>% 
    unlist() %>% 
    str_subset(pattern = "hits") %>% 
    str_remove_all(pattern = "hits.csv|hits1-|hits") %>% 
    str_remove_all(pattern = ".xlsx|.csv|1-") %>% 
    str_trim(side = "both") %>% 
    as.numeric() %>% 
    sum() # 72234 records


## Choosing best algorithm
SEED <- 2021
k <- 10


tic()
tset.TM <- list (
    VEM0 = LDA(dtm, k=k, control = list ( seed = SEED)),
    VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
    Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (
        seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
    CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))
toc() #4218s

sapply (tset.TM[1:3], slot, "alpha")

#Finding number of topics
k <- c(25,50,100,250,500)
tic()
topicNumber.TM <- map(
    .x = k,
    .f = function(x) {
        LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
    })
toc() #43955.608 or 12.2hrs
#Finding number of topics
# k <- c(5,10,25,50,100)
# 
# topicNumber.TM <- map(
#     .x = k,
#     .f = function(x) {
#         LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
#     })

save(tset.TM, topicNumber.TM, dtm, file = "data/211020_topic_models_gibbs.RData")


load("data/211020_topic_models_gibbs.RData")


#### visualizations ####
df_topic_number <- tibble(
    topic_number = k,
    entropy = map_dbl (topicNumber.TM, function (x)
        mean(apply(posterior(x)$topics, 1, function (z) - sum(z * log(z)))) # maximize Entropy
    ),
    alpha = map_dbl(topicNumber.TM, slot, "alpha"),
    log_lik = map_dbl(topicNumber.TM, logLik) #,  #maximize loglik
    #perplexity = map_dbl(topicNumber.TM, perplexity) #minimize perplexity
)

df_stats <- tibble(
    model = names(lapply(tset.TM, logLik)),
    loglik = as.numeric(lapply(tset.TM, logLik)), #maximize loglik
    entropy = lapply (tset.TM, function (x) 
        mean(apply(posterior(x)$topics,
                   1, function (z) - sum(z * log(z))))) %>% as.numeric()#maximize ENTROPY
    
)


perp <-  lapply(tset.TM[c(1,2,4)], perplexity)
perp$Gibbs <- NA
# pretty names:
df_stats$model <- c("VEM alpha", "VEM fixed", "Gibbs", "CTM")

g1 <- df_stats %>%
    add_column(perplexity = as.numeric(perp[c(1,2,4,3)])) %>% #minimize perplexity
    pivot_longer(cols = 2:4, names_to = "measure", values_to = "value") %>%
    ggplot(aes(model, value)) + 
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    facet_wrap(.~measure, scales = "free_y") +
    labs(x = "Algorithm", y = "Value", tag = "A") +
    theme_light(base_size = 8) + 
    theme(axis.text.x = element_text(size = 5))
g1

g2 <- df_topic_number %>%
    # mutate(alpha_log = log10(alpha)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "measure", values_to = "value") %>%
    # filter(measure != "alpha") %>%
    ggplot(aes(as.factor(topic_number), value)) +
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    labs(x = "Number of topics", y = "Value", tag = "B") +
    facet_wrap(.~measure, scales = "free", ncol = 4, nrow = 1) +
    theme_light(base_size = 8)

g1/g2

ggsave(
    filename = "topicstats.png",
    path = "figures/",
    plot = last_plot(),
    device = "png",
    width = 6, height = 3, units = "in",
    dpi = 400,
    bg = "white"
)


df_topics25 <- tidy(topicNumber.TM[[3]], matrix = "beta")

g_25 <- df_topics25 %>%
    group_by(topic) %>%
    top_n(25, beta) %>%
    ungroup() %>%
    arrange(topic, - beta) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta)) +
    geom_col(aes(fill = as.factor(topic)),show.legend = FALSE) +
    coord_flip() +
    scale_x_reordered() +
    labs(y = "Probability of word explaining the topic", x = "Word ranking") +
    facet_wrap(.~ topic, scales = "free_y", ncol = 10) +
    theme_light(base_size = 5) +
    theme(axis.text.x = element_text(size = 5))


g_25

ggsave(
    filename = "topics_100.png",
    path = "figures/",
    plot = g_25,
    device = "png",
    width = 10, height = 12, units = "in",
    dpi = 400,
    bg = "white"
)

df_topics25

### documents:
df_documents <- tidy(topicNumber.TM[[3]], matrix = "gamma")

papers_yr <- dat %>%
    #filter(year > 2000, year < 2021) %>%
    group_by(year) %>%
    tally() %>% rename(total_papers = n)


g_stack <-  df_documents %>%
    left_join(
        dat %>%
            select(abstract, title, year) %>%
            filter(!is.na(abstract)) %>%
            unique(),
        by = c("document" = "title") ) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year > 2000, year <2021) %>%
    group_by(topic, year) %>%
    summarize(gamma_yr_tp = sum(gamma), .groups = "drop") %>%
    group_by(year) %>%
    mutate(total = sum(gamma_yr_tp)) %>%
    ungroup() %>%
    mutate(proportion = gamma_yr_tp / total,
           topic = as.factor(topic)) %>%
    ggplot(aes(x = year, y = proportion, group = topic)) +
    # geom_area(aes(fill = topic, color = topic),
    #           position = "stack", show.legend = FALSE, alpha = 0.5, size = 0.25) +
    geom_line(aes(color = topic), show.legend = FALSE, alpha = 0.5, size = 0.5) +
    geom_hline(yintercept = 0.01, colour = "grey50") +
    labs(y = "Relative proportion of topics", x = "Year") +
    theme_classic(base_size = 10)

g_stack #|> plotly::ggplotly()

ggsave(
    filename = "topics_time.png",
    path = "figures/",
    plot = g_stack,
    device = "png",
    width = 4, height = 3, units = "in",
    dpi = 400,
    bg = "white"
)

df_documents |> 
    group_by(topic) |> 
    arrange(desc(gamma)) |> 
    #top_n(10) |> 
    left_join(select(dat, document = title, year)) #|> 
    #write_csv("data/topic_papers.csv")
    #save(x, file = "data/topic_papers.RData")


### networks
load("data/topic_models_gibbs.RData")


authors <- dat %>% 
    select(authors, doi) %>% 
    mutate(authors = str_split(authors, pattern = ", ")) %>% 
    unnest(cols = authors) %>% 
    mutate(presence = 1)

mat <- authors %>% unique() %>% 
    pivot_wider(names_from = authors, values_from = presence, values_fill = 0) %>% 
    select(-doi) %>%
    as.matrix()

tic()
mat <- t(mat) %*% (mat)
toc()


df_authors <- as_tibble(mat, rownames = "author") 

df_authors <- df_authors %>% 
    pivot_longer(2:last_col(), names_to = "co_author", values_to = "n_papers") %>% 
    filter(author != co_author, n_papers > 0)

net <- df_authors %>% 
    # filtering reduces from 232k links and >4.5k authors to 708 links shared by 233 authors
    filter(n_papers > 1) %>% 
    network(., directed = FALSE, matrix.type = "edgelist")

plot.network(
    net, label = NULL, usearrows = FALSE, vertex.cex = 0.5, edge.col = "grey50",
    vertex.col = "goldenrod", edge.lwd = 0.1
    )

networkD3::simpleNetwork(
    Data= df_authors %>% filter(n_papers >1) ,
    Source = "author", Target = "co_author", 
    zoom = TRUE, linkColour = "grey", nodeColour = "blue"
)

## free memory
rm(mat, authors)