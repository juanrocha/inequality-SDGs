library(tidyverse)
library(tidytext)
library(tm)
library (topicmodels)
library(lda)
library(tictoc)
library(network)

# not working, need manual download
# dat <- read_csv(file = "https://stockholmuniversity.box.com/s/uoii9orvhkoijk1hzg4ahqwdydwhuxlb")
# "https://api.box.com/2.0/files/uoii9orvhkoijk1hzg4ahqwdydwhuxlb"
dat <- read_csv(file = "data/April27_1-2000.csv") %>% 
    janitor::clean_names()

## clean data: extra words
too_words <- tibble(
    word = c("paper", "study", "aim", "aims", "objective", "purpose")
)

dtm <- dat %>% 
    select(abstract, title, year) %>%
    filter(!is.na(abstract)) %>%
    unique() %>% # 2193
    unnest_tokens(word, abstract) %>% 
    filter(!is.na(word)) %>% ## Books don't have abstract so they get dropped (n = 8)
    anti_join(stop_words) %>% 
    anti_join(too_words) %>%
    filter(!str_detect(word, "[:digit:]")) %>%
    group_by(title) %>%
    count(word, sort = TRUE) %>%
    cast_dtm(document = title, term = word, value = n)

dtm

## Choosing best algorithm
SEED <- 2021
k <- 10

tset.TM <- list (
    VEM0 = LDA(dtm, k=k, control = list ( seed = SEED)),
    VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
    Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (
        seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
    CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))

sapply (tset.TM[1:3], slot, "alpha")

#Finding number of topics
k <- c(5,10,25,50,100)

topicNumber.TM <- map(
    .x = k,
    .f = function(x) {
        LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
    })

#Finding number of topics
k <- c(5,10,25,50,100)

topicNumber.TM <- map(
    .x = k,
    .f = function(x) {
        LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
    })

save(tset.TM, topicNumber.TM, file = "data/topic_models_gibbs.RData")





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