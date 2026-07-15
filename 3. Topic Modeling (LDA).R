# ==============================================================================
# LDA TOPIC MODELING: HUMAN INTERPRETABILITY EXPORT SCRIPT
# Purpose: Load DFM, apply stopwords, run LDA for K = 15, 20, 25, 30, and 
# export top words and documents to Excel for subjective validation.
# ==============================================================================

# --- 1. Libraries ---
library(quanteda)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(openxlsx)

# --- 2. Load Data and Apply Stopwords ---
input_dfm <- "_outputs/dfm_cleaned.rds"
if (!file.exists(input_dfm)) stop("Cleaned DFM not found. Check file path.")

dfm_clean <- readRDS(input_dfm)

aggressive_stops <- c(
  # Agencies
  "doh", "dpwh", "deped", "neda", "dnd", "dole", "dict", "dti", "dot", 
  "dotr", "doe", "dilg", "dfa", "dswd", "dar", "coa", "denr", "pcoo", "dof","doj","dhsud",
  "pco","depdev", "dmw", "dost", "dbm",
  
  # Sector-Specific Words
  "health", "hospitals", "medical", "school", "schools", "education", "learning",
  "tourism", "agrarian", "natural", "energy", "land", "labor", "workers", "agricultural",
  "employment", "environment", "forest", "media", "housing","hfep","ngp","defense","security","social",
  "sbfp","learners","students", "teachers","welfare",
  
  # Regional/Sub-offices and Geographic locations  
  "darro", "darpo", "darpos", "darco", "rfo", "rfos", "fos", "sdo", "sdos", 
  "deos", "penros", "chd", "lto", "sur", "norte","ltfrb","davao","ncr","penro",
  "bai","ati","deo","car","nro",
  
  # Bureaucratic Fluff and Cabinet Secretaries
  "department", "secretary", "regional", "provincial", "region",
  "duque", "cusi", "dominguez", "andanar","cys", "del", "ros", "shall",
  "i", "ii", "iii", "iv", "v", "vi", "vii","viii","ix","x","xi","xii","xiii",
  
  #Additional
  "recommended","recommendation","recommendations", "audit","audits", "auditing", "audited", "auditor", "auditors",
  "agency", "agencies", "management", "report", "reports",
  "total", "amount", "amounting"
)

dfm_blind <- dfm_remove(dfm_clean, pattern = aggressive_stops)
dfm_blind <- dfm_subset(dfm_blind, ntoken(dfm_blind) > 0)
dtm_lda <- quanteda::convert(dfm_blind, to = "topicmodels")

# --- 3. Evaluate Interpretability Across Candidate Ks ---
k_candidates <- c(15)
wb <- createWorkbook()

for (k in k_candidates) {
  
  message("  Processing K = ", k, "...")
  
  # Run the LDA model (using Gibbs sampling and iter = 500 for relatively quick checking)
  lda_model <- LDA(dtm_lda, k = 15, method = "Gibbs", control = list(seed = 1234, iter = 500))
  saveRDS(lda_model, "_outputs/lda_k15.rds")
  
  # --- STEP A: EXTRACT TOP WORDS (For Cohesion & Granularity Checks) ---
  topic_words <- tidy(lda_model, matrix = "beta")
  
  top_terms <- topic_words %>%
    group_by(topic) %>%
    slice_max(beta, n = 15) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    group_by(topic) %>%
    mutate(rank = row_number()) %>%
    select(topic, rank, term) %>%
    pivot_wider(names_from = topic, values_from = term, names_prefix = "Topic_") %>%
    select(-rank)
  
  sheet_name_words <- paste0("Words_K", k)
  addWorksheet(wb, sheet_name_words)
  writeData(wb, sheet_name_words, top_terms)
  
  # --- STEP B: EXTRACT TOP DOCUMENTS (For the Document Verification Check) ---
  topic_docs <- tidy(lda_model, matrix = "gamma")
  
  top_documents <- topic_docs %>%
    group_by(topic) %>%
    slice_max(gamma, n = 3) %>%
    ungroup() %>%
    arrange(topic, -gamma) %>%
    rename(Topic = topic, Document_ID = document, Probability = gamma)
  
  sheet_name_docs <- paste0("Docs_K", k)
  addWorksheet(wb, sheet_name_docs)
  writeData(wb, sheet_name_docs, top_documents)
}

# --- 4. Export Final File ---
output_file <- "_outputs/Human_Interpretability_Test.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)