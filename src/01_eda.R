#### Application of the LDA Model for Topic Identification: A Case Study on Dengue Diagnosis ####
#### Authors: Jennyfer Portilla Yela & Andrés Felipe Palomino Montezuma ####

#  Clean environment and console
rm(list = ls())  # Remove all objects from the environment
gc()             # Trigger garbage collection
cat("\014")      # Clear the console

#  Load required libraries using pacman
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
    slam,        # Sparse matrix computations for LDA
    ggraph,      # Graph visualization
    igraph,      # Network analysis
    textcat      # Language detection
  )
))
# Set the main directory
main_directory <-"C:/Users/Sebastián Palomino/Desktop/LDA_dengue_diagnosis" # Change
setwd(main_directory)

#  Define input and output paths
input_path <- file.path(main_directory, "inputs")
output_path <- file.path(main_directory, "outputs",'eda')
img_path <- file.path(output_path, "img")
output_path_files=file.path(output_path,'files')
#  Create necessary directories if they do not exist
dir.create(input_path, showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)
dir.create(img_path, showWarnings = FALSE)

#  Load the dataset
data_file <- file.path(input_path, "Datos280124_FINALES_DENGUE_ULTIMA.xlsx")

Datos <- read_excel(data_file)

# Verify duplicates in the dataset
duplicates_key <- which(duplicated(Datos$key))
duplicates_title <- which(duplicated(Datos$title))

cat("Number of duplicate keys:", length(duplicates_key), "\n")
cat("Number of duplicate titles:", length(duplicates_title), "\n")
####################### Graph: Articles by Year #######################

# Create a table of articles per year
table1 <- table(Datos$year)
df <- as.data.frame(table1)
df$Grupo <- 1  # Add a dummy group variable

# Define the plot
yearly_plot <- ggplot(df, aes(x = Var1, y = Freq, group = Grupo)) +
  geom_area(fill = "steelblue", alpha = 0.7) +
  geom_point(color = "#005a6d", size = 2) +
  labs(title = "Articles by Year", x = "Year", y = "Quantity of Articles") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_light()

# Save the plot to `img/` using `ggsave()`
year_plot_path <- file.path(img_path, "articles_by_year.png")
ggsave(year_plot_path, plot = yearly_plot, dpi = 300, width = 9, height = 6)

############### Graph: Articles by Journal ###############

# Normalize journal frequencies
Anal_revista <- table(Datos$journal) / sum(table(Datos$journal))
Anal_revista <- Anal_revista[order(-Anal_revista)]  # Order from highest to lowest

# Convert to DataFrame
Anal_revista <- data.frame(Anal_revista)
Anal_revista1 <- Anal_revista[1:16,]  # Select the top 16 journals

# Define the plot
journal_plot <- ggplot(Anal_revista1, aes(y = reorder(Var1, Freq), x = Freq * 100)) +
  geom_segment(aes(yend = Var1, xend = 0), color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  labs(title = "Articles by Journal", x = "Percentage of Articles", y = "Journal") +
  theme_light()

# Save the plot
journal_plot_path <- file.path(img_path, "articles_by_journal.png")
ggsave(journal_plot_path, plot = journal_plot, dpi = 300, width = 9, height = 6)

########### Title Analysis (Tokenization, Stopwords, Lemmatization) ###########

# Load required library
library(textcat)

# Detect language of titles
title_languages <- textcat(Datos$title)

# Extract Spanish titles (if any)
spanish_titles <- Datos$title[which(title_languages == "spanish")]

# Save Spanish titles to an output file
spanish_titles_file <- file.path(output_path_files, "spanish_titles.xlsx")
write.xlsx(data.frame(Spanish_Titles = spanish_titles), spanish_titles_file)


###################### Clean Titles ######################

# Convert all titles to lowercase
titles <- tolower(Datos$title)

# Remove URLs
titles <- str_replace_all(titles, "http\\S*", "")

# Remove punctuation
titles <- str_replace_all(titles, "[[:punct:]]", " ")

# Remove numbers
titles <- str_replace_all(titles, "[[:digit:]]", " ")

# Remove multiple spaces
titles <- str_replace_all(titles, "[\\s]+", " ")

# Remove line breaks
titles <- str_replace_all(titles, "\n", " ")

# Trim leading and trailing spaces
titles <- trimws(titles)

# Remove mathematical symbols and Greek letters
titles <- gsub("[^A-Za-z0-9Á-Úá-ú ]", "", titles)

###################### Stopword Dictionary ######################

# Load English stopwords
stop_words_en <- stopwords::stopwords("en")

# Load health-related stopwords from external file
stopword_file <- file.path(input_path, "Stopword1.xlsx")
Stopword_health <- read_excel(stopword_file)

# Combine English and health-related stopwords
stop_words_combined <- append(stop_words_en, Stopword_health$Stopword)

custom_stop_words <- data_frame(
  word = stop_words_combined,
  lexicon = "EN"
)

###################### Tokenization ######################

# Create a DataFrame with document IDs
text_df <- data.frame(
  line = paste("doc", 1:length(titles)), 
  text = titles
)

# Assign document numbers to the dataset
Datos$num_doc <- text_df$line

# Tokenize words
text_df_pre <- text_df %>%
  unnest_tokens(word, text, token = "words")

###################### Removing Stopwords ######################

# Remove words that appear in the custom stopwords list
text_df_pre1 <- text_df_pre %>% 
  anti_join(custom_stop_words) %>%
  filter(nchar(word) != 1)  # Remove single-letter tokens

###################### Lemmatization ######################

# Load the UDPipe model for lemmatization
# udpipe::udpipe_download_model('english')  # Uncomment to download the model the first time
model <- udpipe_load_model(file = file.path(input_path, "english-ewt-ud-2.5-191206.udpipe"))

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

# Replace specific incorrect lemma (if needed)
text_df_pre4$lemma[text_df_pre4$lemma == "indion"] <- "india"

# Save cleaned and tokenized words
tokenized_file <- file.path(output_path, "tokenized_titles.xlsx")
lemmatized_file <- file.path(output_path, "lemmatized_titles.xlsx")

###################### Frequency Plot of Most Common Words ######################

# Define plot
freq_plot <- ggplot(text_df_pre4 %>% filter(n > 30), aes(x = reorder(lemma, n), y = n)) +
  geom_segment(aes(x = lemma, xend = lemma, y = 0, yend = n), color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Most Frequent Words in Titles", x = NULL, y = "Frequency") +
  coord_flip() +
  theme_light()

# Save plot
freq_plot_path <- file.path(img_path, "title_word_frequencies.png")
ggsave(freq_plot_path, plot = freq_plot, dpi = 300, width = 9, height = 6)
###################### Bigram Analysis (Word Pairs) ######################

# Create a tibble for bigrams
pairs <- tibble(txt = titles)

# Tokenize bigrams
bigrams <- pairs %>%
  unnest_tokens(bigram, txt, token = "ngrams", n = 2)

# Count bigram frequencies
bigrams_count <- bigrams %>%
  count(bigram, sort = TRUE)

# Separate bigram words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove stopwords from bigrams
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

# Count filtered bigrams
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

###################### Bigram Network Visualization ######################

# Create graph object from bigrams
bigram_graph <- bigram_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame()

# Save the bigram network plot
bigram_network_path <- file.path(img_path, "bigram_network.png")

g=ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(edge_colour = "steelblue") +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5) +
  theme_light() +
  labs(x = "", y = "") +
  theme(axis.text = element_blank())
ggsave(bigram_network_path, plot = g, dpi = 300, width = 9, height = 6)

########### Abstract Analysis (Cleaning, Tokenization, Lemmatization) ###########

# Convert all abstracts to lowercase
abstracts <- tolower(Datos$abstract)

# Remove special characters and replace medical terms
abstracts <- stri_replace_all_regex(
  abstracts,
  pattern = c("&", "≤", "·", '[+*/=<>]', "©", "diagnosis", "zikv", "chikv"),
  replacement = c(' ', ' ', ' ', ' ', ' ', "diagnostic", "zika", "chikungunya"),
  vectorize = FALSE
)

# Remove URLs, punctuation, numbers, and extra spaces
abstracts <- abstracts %>%
  str_replace_all("http\\S*", "") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("[[:digit:]]", " ") %>%
  str_replace_all("[\\s]+", " ") %>%
  str_replace_all("\n", " ") %>%
  trimws()

# Remove mathematical symbols and Greek letters
abstracts <- gsub("[^A-Za-z0-9Á-Úá-ú ]", "", abstracts)

###################### Tokenization ######################

# Create a DataFrame with document IDs
text_df <- data.frame(
  line = paste("doc", 1:length(abstracts)), 
  text = abstracts
)

# Assign document numbers to the dataset
Datos$num_doc <- text_df$line

# Tokenize words
text_df_pre <- text_df %>%
  unnest_tokens(word, text, token = "words")

###################### Removing Stopwords ######################

# Remove words that appear in the custom stopwords list
text_df_pre1 <- text_df_pre %>% 
  anti_join(custom_stop_words) %>%
  filter(nchar(word) != 1)  # Remove single-letter tokens

###################### Lemmatization ######################

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

# Save cleaned and tokenized words
tokenized_file <- file.path(output_path, "tokenized_abstracts.xlsx")
lemmatized_file <- file.path(output_path, "lemmatized_abstracts.xlsx")
###################### Frequency Plot of Most Common Words ######################

# Define plot
freq_plot <- ggplot(text_df_pre4 %>% filter(n > 300), aes(x = reorder(lemma, n), y = n)) +
  geom_segment(aes(x = lemma, xend = lemma, y = 0, yend = n), color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Most Frequent Words in Abstracts", x = NULL, y = "Frequency") +
  coord_flip() +
  theme_light()

# Save plot
freq_plot_path <- file.path(img_path, "abstract_word_frequencies.png")
ggsave(freq_plot_path, plot = freq_plot, dpi = 300, width = 9, height = 6)

###################### Bigram Analysis (Word Pairs) ######################

# Create a tibble for bigrams
pairs <- tibble(txt = abstracts)

# Tokenize bigrams
bigrams <- pairs %>%
  unnest_tokens(bigram, txt, token = "ngrams", n = 2)

# Count bigram frequencies
bigrams_count <- bigrams %>%
  count(bigram, sort = TRUE)

# Separate bigram words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove stopwords from bigrams
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

# Count filtered bigrams
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

###################### Bigram Network Visualization ######################

# Create graph object from bigrams
bigram_graph <- bigram_counts %>%
  filter(n >= 30) %>%
  graph_from_data_frame()

# Save the bigram network plot
bigram_network_path <- file.path(img_path, "bigram_network_abstracts.png")
h=ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(edge_colour = "steelblue") +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5) +
  theme_light() +
  labs(x = "", y = "") +
  theme(axis.text = element_blank())
ggsave(bigram_network_path, plot = h, dpi = 300, width = 9, height = 6)

