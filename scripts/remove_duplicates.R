#################################
# Remove duplicate documents in a directory
# Project: Religious Lanuguage in Farewell Letters
# Muriël Bouman, May 2024
#################################

# Load required packages
library(quanteda)
library(quanteda.textstats)
library(stringr)

# Function to load documents from a directory
load_documents <- function(directory) {
  files <- list.files(directory, full.names = TRUE)
  documents <- sapply(files, function(file) paste(readLines(file, warn = FALSE), collapse = "\n"), USE.NAMES = TRUE)
  names(documents) <- basename(files)
  return(documents)
}

# Function to remove duplicates based on cosine similarity
remove_duplicates <- function(documents, similarity_threshold = 0.8) {
  # Create a corpus of the documents
  corpus <- corpus(documents)
  
  # Tokenize the corpus
  tokens <- tokens(corpus, remove_punct = TRUE)
  
  # Create a document-feature matrix (dfm)
  dfm <- dfm(tokens)
  
  # Calculate cosine similarity between documents
  similarity_matrix <- textstat_simil(dfm, method = "cosine", margin = "documents")
  
  # Convert similarity matrix to a data frame
  similarity_df <- as.data.frame(as.matrix(similarity_matrix))
  
  # Find duplicates
  duplicates <- c()
  unique_documents <- documents
  for (i in seq_len(nrow(similarity_df))) {
    for (j in seq_len(i - 1)) {
      if (!is.na(similarity_df[i, j]) && similarity_df[i, j] >= similarity_threshold) {
        duplicates <- c(duplicates, rownames(similarity_df)[i])
        unique_documents <- unique_documents[!names(unique_documents) %in% rownames(similarity_df)[i]]
        break
      }
    }
  }
  
  return(list(unique_documents = unique_documents, duplicates = duplicates))
}

# Function to save documents to a directory
save_documents <- function(documents, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
  for (name in names(documents)) {
    writeLines(documents[[name]], file.path(directory, name))
  }
}

# Function to save duplicates to a directory
save_duplicates <- function(documents, duplicates, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
  for (name in duplicates) {
    writeLines(documents[[name]], file.path(directory, name))
  }
}

# Directory with documents
input_directory <- "data/collection-249-farewell-letters/complete-collection"
output_directory <- "data/collection-249-farewell-letters/only-letters-without-duplicates"

# Load documents
documents <- load_documents(input_directory)

# Remove duplicates
result <- remove_duplicates(documents, similarity_threshold = 0.8)
unique_documents <- result$unique_documents
duplicates <- result$duplicates

# Save unique documents to the output directory
save_documents(unique_documents, output_directory)

# Save removed (duplicate) documents to the duplicates directory
save_duplicates(documents, duplicates, duplicates_directory)
