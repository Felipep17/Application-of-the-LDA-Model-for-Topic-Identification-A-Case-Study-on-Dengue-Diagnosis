#### Application of the LDA Model for Topic Identification: A Case Study on Dengue Diagnosis ####
#### Authors: Jennyfer Portilla Yela & Andrés Felipe Palomino Montezuma ####

# Clean the environment and console for good practices
rm(list = ls())  # Remove all objects from the environment
gc()             # Trigger garbage collection
cat("\014")      # Clear the console

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
suppressMessages(suppressWarnings(
  p_load(
    readxl,      # Read Excel files
    openxlsx,    # Export Excel files
    tidyverse,   # Collection of data science packages (dplyr, ggplot2, tidyr, etc.)
    stringr,     # String manipulation
    stopwords,   # Handling stopwords
    tidytext,    # Text mining and tokenization
    SnowballC,   # Stemming words
    topicmodels, # Latent Dirichlet Allocation (LDA) for topic modeling
    stringi,     # String processing (accents removal, regex)
    tm,          # Text mining functions
    wordcloud,   # Generate word clouds
    wordcloud2,  # Advanced word cloud visualization
    udpipe,      # Lemmatization and POS tagging
    ldatuning,   # Selection of optimal topics in LDA
    jsonlite,    # JSON format handling
    LDAvis,      # Interactive visualization of LDA topic models
    slam         # Sparse matrix computations for LDA
  )
))

# Set the main directory
main_directory <-"C:/Users/Sebastián Palomino/Desktop/Application-of-the-LDA-Model-for-Topic-Identification-A-Case-Study-on-Dengue-Diagnosis"
setwd(main_directory)

# Define file paths
input_path <- file.path(main_directory, "inputs")
output_path <- file.path(main_directory,"outputs",'model')
output_path_files=file.path(output_path,'files')

#  Create necessary directories if they do not exist
dir.create(input_path, showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)
dir.create(output_path_files, showWarnings = FALSE)

# Load data
data_file <- file.path(input_path, "Datos280124_FINALES_DENGUE_ULTIMA.xlsx")
stopword_file <- file.path(input_path, "Stopword2.xlsx")

Datos <- read_excel(data_file)

####################### Data Cleaning #######################

# Convert all text to lowercase
abstracts <- tolower(Datos$abstract)

# Replace special characters and medical terms
abstracts <- stri_replace_all_regex(
  abstracts,
  pattern = c("&", "≤", "·", '[+*/=<>]', "©", "diagnosis", "zikv", "chikv", "thrombocytopaenia", "hemorrhagic"),
  replacement = c(' ', ' ', ' ', ' ', ' ', "diagnostic", "zika", "chikungunya", "thrombocytopenia", "haemorrhagic"),
  vectorize = FALSE
)

# Remove URLs
abstracts <- str_replace_all(abstracts, "http\\S*", "")

# Remove punctuation
abstracts <- str_replace_all(abstracts, "[[:punct:]]", " ")

# Remove numbers
abstracts <- str_replace_all(abstracts, "[[:digit:]]", " ")

# Remove multiple spaces
abstracts <- str_replace_all(abstracts, "[\\s]+", " ")

# Remove line breaks
abstracts <- str_replace_all(abstracts, "\n", " ")

# Trim leading and trailing spaces
abstracts <- trimws(abstracts)

# Remove mathematical symbols and Greek letters
abstracts <- gsub("[^A-Za-z0-9Á-Úá-ú ]", "", abstracts)

# Load stopword dictionary
stop_words_en <- stopwords::stopwords("en")  # English stopwords
Stopword_health <- read_excel(stopword_file)

# Combine English and health-related stopwords
stop_words_en1 <- append(stop_words_en, Stopword_health$Stopword)

custom_stop_words <- data_frame(
  word = stop_words_en1,
  lexicon = "EN"
)

# Save the cleaned abstracts to output
cleaned_data_file <- file.path(output_path_files, "cleaned_abstracts.xlsx")
write.xlsx(data.frame(abstracts), cleaned_data_file)


####################### Tokenization: Splitting into Words #######################

# Create a data frame with document IDs
text_df <- data.frame(
  line = paste("doc", 1:length(abstracts)), 
  text = abstracts
)

# Assign document numbers to the dataset
Datos$num_doc <- text_df$line

# Tokenize words
text_df_pre <- text_df %>%
  unnest_tokens(word, text, token = "words")

####################### Removing Stopwords #######################

# Remove words that appear in the custom stopwords list
text_df_pre1 <- text_df_pre %>% 
  anti_join(custom_stop_words) %>%
  filter(nchar(word) != 1)  # Remove single-letter tokens

####################### Lemmatization #######################

# Load the UDPipe model for lemmatization
# udpipe::udpipe_download_model('english')  # Uncomment to download the model the first time
model <- udpipe_load_model(file = file.path(input_path,"english-ewt-ud-2.5-191206.udpipe"))

# Annotate the text using the lemmatization model
Lemat <- udpipe_annotate(model, 
                         x = text_df_pre1$word, 
                         doc_id = text_df_pre1$line)

# Convert to tibble format
Lemat1 <- as_tibble(Lemat)

# Count lemmatized words
text_df_pre2 <- Lemat1 %>%
  count(lemma, sort = TRUE)

# Remove stopwords again after lemmatization
text_df_pre3 <- text_df_pre2 %>%
  filter(!(lemma %in% custom_stop_words$word))

# Filter lemmatized words from stopwords
Lemat2 <- Lemat1 %>%
  filter(!(lemma %in% custom_stop_words$word))

# Count the cleaned and lemmatized words
text_df_pre4 <- Lemat2 %>%
  count(lemma, sort = TRUE)

# Save tokenized and lemmatized words
tokenized_file <- file.path(output_path_files, "tokenized_words.xlsx")
lemmatized_file <- file.path(output_path_files, "lemmatized_words.xlsx")

write.xlsx(text_df_pre4, lemmatized_file)
write.xlsx(text_df_pre3, tokenized_file)

####################### Word Cloud #######################

# Create necessary output directories
dir.create(output_path_files, showWarnings = FALSE)
img_path <- file.path(output_path, "img")
dir.create(img_path, showWarnings = FALSE)

wordcloud2(text_df_pre3 %>% filter(n > 50))
dev.off()

# Save word frequencies to output
wordcloud_file <- file.path(output_path_files, "word_frequencies.xlsx")
write.xlsx(text_df_pre3, wordcloud_file)


####################### Compute Term Frequency-Inverse Document Frequency (TF-IDF) #######################

# Filter only relevant parts of speech (nouns, adjectives, others)
n_tfidf_df <- Lemat2 %>% 
  filter(!is.na(lemma)) %>%
  filter(upos %in% c("NOUN", "ADJ", "X")) %>% 
  count(doc_id, lemma)

# Compute TF-IDF
Doce <- bind_tf_idf(n_tfidf_df, lemma, doc_id, n)

# Create a Document-Term Matrix (DTM)
DTM <- cast_dtm(Doce, doc_id, lemma, n)

# Remove low-frequency terms
DTM_filtered <- removeSparseTerms(DTM, sparse = 0.98)

# Save the cleaned DTM
dtm_file <- file.path(output_path_files, "filtered_dtm.rds")
saveRDS(DTM_filtered, dtm_file)

####################### Optimal Number of Topics Selection #######################

# Use LDA tuning package to determine the best number of topics
Res_ntopics <- FindTopicsNumber(
  DTM_filtered,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  mc.cores = 4,
  verbose = TRUE
)

# Save topic tuning results
lda_tuning_file <- file.path(output_path_files, "lda_tuning_results.xlsx")
write.xlsx(Res_ntopics, lda_tuning_file)
FindTopicsNumber_plot(Res_ntopics)
# Save topic selection plot as PNG
topic_selection_plot <- file.path(img_path, "lda_topic_selection.png")
ggsave(topic_selection_plot, dpi = 300, width = 9, height = 6)

####################### Alternative Topic Selection Using Perplexity #######################

possible_k <- 2:20  # Range of topics to test

# Store results
results_list <- data.frame(k_topics = integer(), Perplexity = numeric())

# Iterate through each k value
for (k in possible_k) {
  lda_model <- LDA(DTM_filtered, k = k, method = "Gibbs", control = list(seed = 1234))
  
  # Compute perplexity
  perplexity_value <- perplexity(lda_model, DTM_filtered)
  
  # Store the results
  results_list <- rbind(results_list, data.frame(k_topics = k, Perplexity = perplexity_value))
  
  cat("Evaluated k =", k, "\n")
}

# Save perplexity results
perplexity_file <- file.path(output_path_files, "lda_perplexity_results.xlsx")
write.xlsx(results_list, perplexity_file)

# Save perplexity plot as PNG
perplexity_plot <- file.path(img_path, "lda_perplexity_analysis.png")
ggplot(results_list, aes(x = k_topics, y = Perplexity)) +
  geom_line(color = "#151515") +
  geom_point(color = "#151515", size = 2) +
  labs(title = "LDA Perplexity Analysis", x = "Number of Topics (K)", y = "Perplexity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_light()
ggsave(perplexity_plot, dpi = 300, width = 9, height = 6)

####################### LDA Model Fitting with Optimal K = 4 #######################
library(topicmodels)
set.seed(1234)

ap_lda_sug_metrics4<- LDA(x = DTM_filtered, k = 4, method = "Gibbs",control = list(seed = 1234))

####################### LDA Visualization #######################

# Extract posterior probabilities for visualization
json_data <- list(
  phi = posterior(ap_lda_sug_metrics4)$terms, 
  theta = t(posterior(ap_lda_sug_metrics4)$topics),    
  num_topics = ncol(ap_lda_sug_metrics4@gamma)
)

# Save JSON output
lda_json_file <- file.path(output_path_files, "lda_topics.json")
write_json(json_data, lda_json_file)

# Create visualization using LDAvis
lda_data <- createJSON(
  phi = posterior(ap_lda_sug_metrics4)$terms,      
  theta = posterior(ap_lda_sug_metrics4)$topics,      
  vocab = colnames(posterior(ap_lda_sug_metrics4)$terms),      
  doc.length = as.vector(row_sums(DTM_filtered)),
  term.frequency = col_sums(DTM_filtered)
)

serVis(lda_data)

####################### Extracting Top Terms Per Topic #######################

matriz_beta <- tidy(ap_lda_sug_metrics4, matrix = "beta")

top_terms <- matriz_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 30) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Save top terms
top_terms_file <- file.path(output_path_files, "lda_top_terms.xlsx")
write.xlsx(top_terms, top_terms_file)

# Plot top terms per topic and save
top_terms_plot <- file.path(img_path, "lda_top_terms.png")
ggplot(top_terms, aes(beta, reorder_within(term, beta, topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(x = expression(beta), y = "Term") +
  scale_x_continuous(limits = c(0, 0.15)) +
  theme_light()
ggsave(top_terms_plot, dpi = 300, width = 9, height = 6)

####################### Relevant Terms Per Topic #######################

# Extract top 30 terms per topic
ma <- terms(ap_lda_sug_metrics4, 30)

# Save relevant terms per topic
relevant_terms_file <- file.path(output_path_files, "lda_relevant_terms.xlsx")
write.xlsx(ma, relevant_terms_file)


####################### Topic-Word Distribution #######################

# Get the topic distribution for each document
topic_distribution <- posterior(ap_lda_sug_metrics4)$topics

# Save topic distribution
topic_distribution_file <- file.path(output_path_files, "lda_topic_distribution.xlsx")
write.xlsx(topic_distribution, topic_distribution_file)

