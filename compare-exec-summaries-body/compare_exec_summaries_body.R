### Compare Executive Summaries Against Text Body

# Author: Malte Lüken - m.luken@esciencecenter.nl

# Date: 18-07-2022


# Set parameter for script:
#   corpus_path: path to directory with scraped corpus json files
params = list(
  corpus_path = file.path('scrape-corpus', 'studies_on_water_scraped')
)

library(rjson)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(quanteda)


# Prepare Corpus ----------------------------------------------------------

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

# Select executive summaries
select_exec_summaries = function(doc) {
  include = str_detect(tolower(names(doc)), pattern = paste('exec'))
  
  return(doc[include])
}

exec_summaries = lapply(docs, select_exec_summaries)

# Function to collapse docs into single strings
collapse_doc = function(doc) {
  return(paste(unlist(doc), collapse = ' '))
}

# Create corpus object from text and metadata
docs_corpus = corpus(
  c(sapply(docs_relevant, collapse_doc),
    sapply(exec_summaries, collapse_doc)),
  docnames = c(sapply(docs, function(doc) doc$meta$title),
               sapply(docs, function(doc) paste0('exec - ', doc$meta$title))),
  docvars = data.frame(type = rep(c('body', 'exec'), each = 55))
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
dfm_body = docs_tokens %>%
  tokens_subset(type == "body") %>%
  dfm() %>%
  dfm_trim(min_termfreq = 2) # Exclude terms with freq < 2

dfm_exec = docs_tokens %>% 
  tokens_subset(type == "exec") %>%
  dfm() %>%
  dfm_trim(min_termfreq = 2)


# Compare Executive Summaries and Body ------------------------------------

theme_set(theme_classic())

# Calculate freq, prop, and tf-idf
calc_feature_df = function(dfm_obj) {
  features = data.frame(
    feature = featnames(dfm_obj),
    feat_freq = colSums(dfm_obj),
    feat_prop = colMeans(dfm_obj %>%
                           dfm_weight(scheme = 'prop')),
    tf_idf = colSums(dfm_obj %>% dfm_tfidf(scheme_tf = 'prop'))
  )
  
  return(features)
}

features_body = calc_feature_df(dfm_body)
features_exec = calc_feature_df(dfm_exec)

# Get most frequent tokens
top_freq = 20

# Plot frequency
features_body %>% 
  arrange(desc(feat_freq)) %>%
  head(top_freq) %>%
  ggplot(aes(x = feat_freq, y = reorder(feature, feat_freq))) +
    geom_col(fill = 'indianred', color = 'black') +
    labs(x = 'Frequency', y = 'Word stem') +
    scale_x_continuous(limits = c(0, 15000))

ggsave(file.path(
  'compare-exec-summaries-body',
  'body_word_freq.png'
), width = 7, height = 5)

features_exec %>% 
  arrange(desc(feat_freq)) %>%
  head(top_freq) %>%
  ggplot(aes(x = feat_freq, y = reorder(feature, feat_freq))) +
  geom_col(fill = 'indianred', color = 'black') +
  labs(x = 'Frequency', y = 'Word stem') +
  scale_x_continuous(limits = c(0, 800))

ggsave(file.path(
  'compare-exec-summaries-body',
  'exec_word_freq.png'
), width = 7, height = 5)

# Plot proportion
features_body %>%
  arrange(desc(feat_prop)) %>%
  head(top_freq) %>%
  ggplot(aes(x = feat_prop, y = reorder(feature, feat_prop))) +
  geom_col(fill = 'indianred', color = 'black') +
  labs(x = 'Average proportion across documents', y = 'Word stem') +
  scale_x_continuous(limits = c(0, 0.01))

ggsave(file.path(
  'compare-exec-summaries-body',
  'body_word_prop.png'
), width = 7, height = 5)

features_exec %>%
  arrange(desc(feat_prop)) %>%
  head(top_freq) %>%
  ggplot(aes(x = feat_prop, y = reorder(feature, feat_prop))) +
  geom_col(fill = 'indianred', color = 'black') +
  labs(x = 'Average proportionacross documents', y = 'Word stem') +
  scale_x_continuous(limits = c(0, 0.015))

ggsave(file.path(
  'compare-exec-summaries-body',
  'exec_word_prop.png'
), width = 7, height = 5)

# Plot tf-idf
features_body %>%
  arrange(desc(tf_idf)) %>%
  head(top_freq) %>%
  ggplot(aes(x = tf_idf, y = reorder(feature, tf_idf))) +
  geom_col(fill = 'indianred', color = 'black') +
  labs(x = 'tf-idf', y = 'Word stem')

ggsave(file.path(
  'compare-exec-summaries-body',
  'body_word_tf_idf.png'
), width = 7, height = 5)

features_exec %>%
  arrange(desc(tf_idf)) %>%
  head(top_freq) %>%
  ggplot(aes(x = tf_idf, y = reorder(feature, tf_idf))) +
  geom_col(fill = 'indianred', color = 'black') +
  labs(x = 'tf-idf', y = 'Word stem') +
  scale_x_continuous(limits = c(0, 0.1))

ggsave(file.path(
  'compare-exec-summaries-body',
  'exec_word_tf_idf.png'
), width = 7, height = 5)


# Plot proportion distance
features_inter = inner_join(
  features_body, features_exec,
  by = 'feature', suffix = c('_body', '_exec')
) %>%
  mutate(feat_prop_ratio = feat_prop_exec/feat_prop_body,
         feat_prop_dist = feat_prop_exec-feat_prop_body)

features_inter %>%
  arrange(desc(feat_prop_dist)) %>%
  head(top_freq) %>%
  ggplot(aes(x = feat_prop_dist, y = reorder(feature, feat_prop_dist))) +
  geom_col(fill = 'indianred', color = 'black') +
  labs(x = 'Average proportion distance', y = 'Word stem') +
  lims(x = c(0, 0.008))

ggsave(file.path(
  'compare-exec-summaries-body',
  'prop_dist.png'
), width = 7, height = 5)
