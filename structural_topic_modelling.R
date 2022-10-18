### Structural Topic Modelling

# Author: Malte Lüken - m.luken@esciencecenter.nl

# Date: 14-06-2022


# Set parameters for script:
#   corpus_path: path to directory with scraped corpus json files
#   fit: Whether topic models should be fit or loaded from disk
params = list(
  corpus_path = file.path('scrape-corpus', 'studies_on_water_scraped'),
  fit = FALSE
)


# Prepare Corpus ----------------------------------------------------------

library(rjson)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(quanteda)
library(stm)
library(stminsights)

# Function for excluding chapters
exclude_sections = function(doc, terms) {
  exclude = str_detect(tolower(names(doc)), pattern = paste(terms, collapse = '|'))
  
  return(doc[!exclude])
}

# Read json files with scraped text from pdfs
json_filenames = list.files(
  params$corpus_path,
  full.names = TRUE
)

docs = lapply(json_filenames, function(filename) {fromJSON(file = filename)})

# Strings for excluding chapters
terms = c(
  'preface', 'foreword', 'acknowledg', 'executive', 'summary', 'table',
  'figure', 'box', 'abbreviation', 'acronym', 'glossary', 'bibliography', 
  'note', 'meta', 'further reading', 'page',  'key messages', 'annex', 
  'refere', 'background materials', 'statistics'
)

# Exclude irrelevant chapters
docs_relevant = lapply(docs, exclude_sections, terms = terms)

# Create list of chapters included in analysis
included_chapters = str_remove_all(
  sapply(
    lapply(docs_relevant, names), paste, collapse = '\n\t'
  ),
  pattern = '\r'
)

write.table(included_chapters, file = 'included_chapters.txt', quote = FALSE)

# Read metadata
meta_data = read.csv('studies_on_water_metadata.csv', sep = ';', nrows = 55) %>%
  select(-Number) %>%
  arrange(title) %>%
  mutate(id = row_number(), .before = 1)

# Create corpus object from text and metadata
docs_corpus = corpus(
  sapply(docs_relevant, function(doc) paste(unlist(doc), collapse = ' ')),
  docnames = sapply(docs, function(doc) doc$meta$title),
  docvars = meta_data
)

# Create tokenized corpus
docs_tokens = docs_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(c( # Remove stopwords
    stopwords('en'),
    stopwords('es'),
    stopwords('nl'),
    stopwords('pt'),
    stopwords('ru'),
    stopwords('pt'),
    stopwords('de'),
    stopwords('fr'),
    stopwords('it')
  )) %>%
  tokens_keep(c('^[a-z]+-?[a-z]*$'), valuetype = 'regex') %>% # Keep only tokens with format [letters](-[letters])
  tokens_wordstem(language = 'en') %>% # Stem words according to English
  tokens_remove(c( # Remove redundant and overlooked tokens
    'oecd', 'water', 'et', 'al', 'x', 'pdf',
    'yes', 'abbrev', 'page', 'pp', 'p', 'er',
    'doi', 'can'
  ))

# Convert tokens to document frequency matrix
docs_dfm = docs_tokens %>%
  dfm() %>%
  dfm_trim(min_termfreq = 2)


# Define Functions for Topic Modelling ------------------------------------

# Function to fit STM
fit_stm = function(k) {
  stm_fit = stm(
    docs_dfm,
    K = k,
    data = docvars(docs_dfm),
    prevalence = ~ s(year) + s(finance) + type + region,
    seed = 2022,
    verbose = FALSE
  )
  
  return(stm_fit)
}

# Function for calculating STM diagnostics
compute_stm_diag = function(stm_obj, dfm_obj) {
  
  exp_topic_props = colMeans(make.dt(stm_obj)[,-1])
  
  topic_corr = topicCorr(stm_obj)
  
  diag(topic_corr$cor) = NA
  
  corr_topics = apply(topic_corr$cor, 2, max, na.rm = TRUE)
  
  cohe_topics = semanticCoherence(stm_obj, dfm_obj, M = 10)
  excl_topics = exclusivity(stm_obj, M = 10)
  
  topic_df = tibble(
    frequency = exp_topic_props,
    correlated = corr_topics,
    coherence = cohe_topics,
    exclusivity = excl_topics
  )
  
  return(topic_df)
}

# Function to plot STM diagnostics
plot_stm_diag = function(stm_obj, file_string) {
  p = ggplot(stm_obj, aes(x = coherence, y = exclusivity, color = correlated)) +
    geom_point() +
    labs(
      x = 'Coherence',
      y = 'Exclusivity',
      color = 'Max. correlation\nwith other topic',
      alpha = 'Prevalence'
    ) +
    scale_color_viridis_c()
  
  ggsave(
    file.path('stm-figures', paste0('stm_', file_string, '_diag.png')),
    p, width = 7, height = 5
  )
}

# Function to compare diagnostics for topic number search
plot_search_diag = function(search_obj) {
  search_df = search_obj$results %>% 
    pivot_longer(cols = -1, names_to = "metric")
  
  p = ggplot(search_df, aes(x = as.numeric(K), y = as.numeric(value))) +
    facet_wrap(vars(metric), scales = 'free_y') +
    geom_line() +
    geom_point() +
    labs(
      x = 'Number of topics (K)',
      y = 'Diagnostic metric'
    )
  
  return(p)
}

# Function to get topic frequency data frame
get_topic_freq_df = function(stm_obj, topic_df) {
  topic_freq_df = topic_df %>%
    mutate(
      label = apply(
        labelTopics(stm_obj, n = 5)$prob, 1, paste, collapse = ', '
      ),
      id = as.character(id)
    ) %>%
    arrange(desc(frequency))
  
  return(topic_freq_df)
}

# Function to plot topic frequency and topic labels
plot_topic_freq_labels = function(topic_freq_df, add_text = 0.03) {
  p = ggplot(topic_freq_df, aes(
    x = frequency,
    y = reorder(id, frequency),
    label = label
  )) +
    geom_col(orientation = 'y', fill = 'indianred', color = 'black') +
    geom_text(aes(x = frequency + add_text)) +
    labs(
      x = 'Topic prevalence',
      y = 'Topic ID'
    )
  
  return(p)
}

# Function to estimate covariate effects
compute_effects =  function(stm_obj) {
  effects = estimateEffect(
    ~ s(year) + s(finance) + type + region,
    stm_obj,
    metadata = docvars(docs_dfm)
  )
  
  return(effects)
}

# Function to plot year effects
plot_effects_year = function(stm_effects) {
  effects_year = get_effects(stm_effects, 'year', type = 'continuous') %>%
    mutate(topic = reorder(factor(topic), desc(proportion)))
  
  p = ggplot(effects_year, aes(
    x = value,
    y = proportion,
    ymin = lower,
    ymax = upper
  )) +
    facet_wrap(vars(topic)) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    labs(
      x = 'Year',
      y = 'Topic prevalence'
    )
  
  return(p)
}

# Function to plot finance effects
plot_effects_finance = function(stm_effects) {
  effects_finance = get_effects(stm_effects, 'finance', type = 'pointestimate') %>%
    mutate(topic = reorder(factor(topic), desc(proportion)))
  
  ggplot(effects_finance, aes(
    x = value,
    y = proportion,
    ymin = lower,
    ymax = upper
  )) +
    facet_wrap(vars(topic)) +
    geom_pointrange(position = position_dodge(0.5), size = 0.25) +
    geom_line(aes(x = as.numeric(value))) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    labs(
      x = 'Focus on finance',
      y = 'Topic prevalence'
    )
}

# Function to plot type effects
plot_effects_type = function(stm_effects) {
  effects_type = get_effects(
    stm_effects, 
    'type',
    type = 'difference',
    cov_val1 = 'Conceptual Piece',
    cov_val2 = 'Case Study'
  ) %>%
    mutate(topic = reorder(factor(topic), desc(difference)))
  
  ggplot(effects_type, aes(
    x = topic,
    y = difference,
    ymin = lower,
    ymax = upper
  )) +
    geom_pointrange(size = 0.5) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    labs(
      x = 'Topic ID',
      y = 'Difference topic prevalence (conceptual - case)'
    )
}

# Function to plot covariate effects
plot_effects = function(stm_effects, file_string) {
  plot_effects_year = plot_effects_year(stm_effects)
  
  ggsave(file.path('stm-figures', paste0('stm_', file_string, '_effects_year.png')),
         plot_effects_year, width = 7, height = 5)
  
  plot_effects_finance = plot_effects_finance(stm_effects)
  
  ggsave(file.path('stm-figures', paste0('stm_', file_string, '_effects_finance.png')),
         plot_effects_finance, width = 7, height = 5)
  
  plot_effects_type = plot_effects_type(stm_effects)
  
  ggsave(file.path('stm-figures', paste0('stm_', file_string, '_effects_type.png')),
         plot_effects_type, width = 7, height = 5)
  
}

# Function to save topic keywords
save_stm_topic_keywords = function(stm_obj, topic_freq_df, file_string, n = 10) {
  topic_labels = labelTopics(stm_obj, n = n)
  
  topic_labels_df = left_join(
    topic_freq_df %>% select(id, frequency),
    as_tibble(
      lapply(
        topic_labels[1:4], function(x) apply(x, 1, paste, collapse = ', ')
      )
    ) %>%
      mutate(id = as.character(row_number())),
    by = 'id'
  ) %>%
    arrange(desc(frequency)) %>%
    pivot_longer(
      cols = c('prob', 'frex', 'lift', 'score'),
      names_to = 'metric'
    ) %>%
    select(-frequency)
  
  topic_labels_df_txt = topic_labels_df %>%
    mutate(
      id = ifelse(row_number() %% 4 == 1, id, ''),
      label = NA
    )
  
  cnt = 0
  
  for (i in seq(4, nrow(topic_labels_df), 4)) {
    topic_labels_df_txt = topic_labels_df_txt %>%
      add_row(.after = i+cnt)
    cnt = cnt + 1
  }
  
  write.table(
    topic_labels_df_txt,
    file = file.path('stm-topic-keywords', paste0('stm_', file_string, '_topic_keywords.txt')),
    sep = '\t',
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    na = ''
  )
  
  write.csv(
    topic_labels_df,
    file = file.path('stm-topic-keywords', paste0('stm_', file_string, '_topic_keywords.csv')),
    row.names = FALSE,
    quote = FALSE,
    na = ''
  )
}

# Function to combine other functions into routine
apply_stm_routine = function(stm_obj, file_string) {
  # Fit STM
  topic_df = compute_stm_diag(stm_obj, docs_dfm) %>%
    mutate(id = row_number())
  
  # Plot diagnostics
  plot_stm_diag(topic_df, file_string)
  
  # Compute covariate effects
  effects = compute_effects(stm_obj)
  
  # Plot covariate effects
  plot_effects(effects, file_string)

  return(topic_df)
}

# Function to get key docs for topics
get_key_docs = function(stm_obj, k, ids) {
  # Create data frame with topic prevalences
  doc_topic_prev__df = data.frame(
    theta = as.vector(stm_obj$theta),
    doc_id = rep(1:length(docs), k),
    topic_id = rep(1:k, each = length(docs))
  )
  
  # Find key documents
  key_docs = findThoughts(
    model = stm_obj,
    texts = names(docs_tokens),
    topics = ids,
    n = 3,
    thresh = 0.05
  )
  
  return(key_docs)
}

# Function to save key documents
save_key_docs = function(thoughts, k, file_string) {
  sink(file.path('stm-key-docs', paste0('stm_', k, '_key_docs', file_string, '.txt')))
  print(thoughts)
  cat('\n')
  sink()
}


# Topic Modelling ---------------------------------------------------------

# Set ggplot theme
theme_set(theme_classic())

# Fit STM and estimate number of topics (K = 0)
if (params$fit) {
  stm_fit_free = fit_stm(0)
  
  save(stm_fit_free, file = file.path('stm-checkpoints', 'stm_fit_free.RData'))
} else {
  load(file.path('stm-checkpoints', 'stm_fit_free.RData'))
}

# Compute diagnostics
topic_df_free = compute_stm_diag(stm_fit_free, docs_dfm)

plot_stm_diag(topic_df_free, 'free')

# Show number of topics with correlation close to 1
sum(topic_df_free$correlated > 0.99) 
# 76

###

# Fit STM with number of topics from free model minus the number of highly correlated topics (K = 38)
nrow(topic_df_free) - sum(topic_df_free$correlated > 0.99)
# 38

if (params$fit) {
  stm_fit_38 = fit_stm(38)

  save(stm_fit_38, file = file.path('stm-checkpoints', 'stm_fit_38.RData'))
} else {
  load(file.path('stm-checkpoints', 'stm_fit_38.RData'))
}

# Compute diagnostics
topic_df_38 = compute_stm_diag(stm_fit_38, docs_dfm)

plot_stm_diag(topic_df_38, '38')

# Show number of topics with correlation close to 1
sum(topic_df_38$correlated > 0.99) 
# 0

###

## Search topic space around K = 38
docs_stm = asSTMCorpus(docs_dfm)

if (params$fit) {
  stm_search_from_38 = searchK(
    docs_stm$documents,
    docs_stm$vocab,
    K = seq(38 - 10, 38 + 10, 2),
    verbose = FALSE
  )
  
  save(stm_search_from_38, file = file.path('stm-checkpoints', 'stm_search_from_38.RData'))
} else {
  load(file.path('stm-checkpoints', 'stm_search_from_38.RData'))
}

# Settle on a number of topics based on diagnostics comparison
final_K = 30

# Compare diagnostics for topic number search
plot_search_diag(stm_search_from_38) +
  scale_x_continuous(breaks = seq(38 - 10, 38 + 10, 4)) +
  geom_vline(xintercept = final_K, linetype = 'dashed')

ggsave(
  file.path('stm-figures', paste0('search_38_diag.png')),
  width = 7, height = 5
)

# Fit STM with 30 topics
if (params$fit) {
  stm_fit_30 = fit_stm(final_K)
  
  save(stm_fit_30, file = file.path('stm-checkpoints', 'stm_fit_30.RData'))
} else {
  load(file.path('stm-checkpoints', 'stm_fit_30.RData'))
}

# Apply routine
topic_df_30 = apply_stm_routine(stm_fit_30, '30')

# Plot topic frequency
topic_freq_df_30 = get_topic_freq_df(stm_fit_30, topic_df_30)

plot_topic_freq_labels(topic_freq_df_30, add_text = 0.05) + xlim(0, 0.15)

ggsave(file.path('stm-figures', 'stm_30_topic_freq.png'), width = 7, height = 5)

# Save topic keywords
save_stm_topic_keywords(stm_fit_30, topic_freq_df_30, '30')

# IDs for key topics in finance
id_key_docs_financing = c(7, 21, 22, 23, 26)

# Find key documents for finance topics
key_docs_finance = get_key_docs(stm_fit_30, final_K, id_key_docs_financing)

save_key_docs(key_docs_finance, final_K, '_finance')

# IDs for key topics in good water governance
id_key_docs_good_water = c(5, 6, 8, 13, 15, 16, 25, 30)

# Find key documents for good water governance topics
key_docs_good_water = get_key_docs(stm_fit_30, final_K, id_key_docs_good_water)

save_key_docs(key_docs_good_water, final_K, '_good_water')

# ID key topics in NL
id_key_docs_nl = final_K

# Find key documents for NL topic
key_docs_nl = get_key_docs(stm_fit_30, final_K, id_key_docs_nl)

save_key_docs(key_docs_nl, final_K, '_nl')

###

# Set number of topics based on thematic topic 'clusters'
small_K = 9

# Fit STM with 9 topics
if (params$fit) {
  stm_fit_9 = fit_stm(small_K)
  
  save(stm_fit_9, file = file.path('stm-checkpoints', 'stm_fit_9.RData'))
} else {
  load(file.path('stm-checkpoints', 'stm_fit_9.RData'))
}

# Apply routine
topic_df_9 = apply_stm_routine(stm_fit_9, '9')

# Plot topic frequency
topic_freq_df_9 = get_topic_freq_df(stm_fit_9, topic_df_9)

plot_topic_freq_labels(topic_freq_df_9, add_text = 0.15) + xlim(0, 0.45)

ggsave(file.path('stm-figures', 'stm_9_topic_freq.png'), width = 7, height = 5)

# Save topic keywords
save_stm_topic_keywords(stm_fit_9, topic_freq_df_9, '9')

# Find key docs for all topics
key_docs_9 = get_key_docs(stm_fit_9, small_K, 1:small_K)

save_key_docs(key_docs_9, small_K, '')

###

# Fit STM with 8 topics
if (params$fit) {
  stm_fit_8 = fit_stm(8)

  save(stm_fit_8, file = file.path('stm-checkpoints', 'stm_fit_8.RData'))
} else {
  load(file.path('stm-checkpoints', 'stm_fit_8.RData'))
}

# Apply routine
topic_df_8 = apply_stm_routine(stm_fit_8, '8')

# Plot topic frequency
topic_freq_df_8 = get_topic_freq_df(stm_fit_8, topic_df_8)

plot_topic_freq_labels(topic_freq_df_8, add_text = 0.15) + xlim(0, 0.45)

ggsave(file.path('stm-figures', 'stm_8_topic_freq.png'), width = 7, height = 5)

# Save topic keywords
save_stm_topic_keywords(stm_fit_8, topic_freq_df_8, '8')

# Find key docs for all topics
key_docs_8 = get_key_docs(stm_fit_8, 8, 1:8)

save_key_docs(key_docs_8, 8, '')

# Save sessionInfo
writeLines(capture.output(sessionInfo()), "session_info.txt")
