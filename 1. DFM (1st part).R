# Purpose: read PDFs, extract text (with OCR fallback), build corpus and DFM,
#          compute diagnostics and save intermediate artifacts.
#
# NOTE: This script does NOT perform content cleaning beyond tokenization.
#       These are performed in "DFM (2nd pass)"

# --- Libraries
library(pdftools)
library(quanteda)
library(tidyverse)
library(tesseract)
library(quanteda.textplots)

# --- Configuration
# Look for the data inside the 'data' folder located next to this script
data_dir <- "data/Audit Reports"
output_dir <- file.path(dirname(data_dir), "_outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# --- Helpers
safe_pdf_text <- function(path) {
  pages <- tryCatch(pdf_text(path), error = function(e) NA_character_)
  if (all(is.na(pages)) || sum(nchar(pages)) < 200) {
    message("OCR fallback on: ", path)
    imgs <- pdf_convert(path, dpi = 300)
    ocr_text <- sapply(imgs, function(img) {
      tryCatch(ocr(img), error = function(e) {
        warning("OCR failed for image: ", img, " (", conditionMessage(e), ")")
        return(NA_character_)
      })
    }, USE.NAMES = FALSE)
    return(paste(ocr_text[!is.na(ocr_text)], collapse = "\n"))
  }
  paste(pages, collapse = "\n")
}

# Robust agency detection using a list and filename fallback
detect_agency <- function(text, filename, agencies) {
  if (!is.na(text) && nzchar(text)) {
    found <- agencies[str_detect(text, regex(paste0("\\b(", paste0(agencies, collapse = "|"), ")\\b"), ignore_case = TRUE))]
    if (length(found) > 0) return(found[1])
  }
  # fallback: try to parse agency from filename
  agency_from_file <- filename %>%
    str_remove("\\.pdf$") %>%
    str_replace_all("[-_]", " ") %>%
    str_extract("^[A-Za-z ]{3,60}") %>%
    str_trim()
  if (!is.na(agency_from_file) && nzchar(agency_from_file)) return(agency_from_file)
  return(NA_character_)
}

# Year detection from filename or text
detect_year <- function(text, filename) {
  # 1. Highest Priority: The Filename
  yr_file <- str_extract(filename, "(19|20)\\d{2}")
  if (!is.na(yr_file)) return(as.integer(yr_file))
  
  # 2. Looks for specific phrases like "Year Ended December 31, 2022", "CY 2022", etc.
  anchor_pattern <- "(?i)(?:year ended december 31,|calendar year|\\bcy\\b|audit report)\\s*[:,-]?\\s*(20\\d{2}|19\\d{2})"
  yr_anchor <- str_match(text, anchor_pattern)[, 2] 
  if (!is.na(yr_anchor)) return(as.integer(yr_anchor))
  
  # 3. Last Resort Fallback: Most Frequent Valid Year
  if (!is.na(text) && nzchar(text)) {
    all_years <- str_extract_all(text, "(19|20)\\d{2}")[[1]]
    
    if (length(all_years) > 0) {
      all_years <- as.integer(all_years)
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      valid_years <- all_years[all_years >= 1990 & all_years <= current_year]
      
      if (length(valid_years) > 0) {
        freq_table <- table(valid_years)
        most_frequent_year <- as.integer(names(freq_table)[which.max(freq_table)])
        return(most_frequent_year)
      }
    }
  }
  
  return(NA_integer_)
}

# --- 1. Read PDF file paths
pdf_files <- list.files(path = data_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
if (length(pdf_files) == 0) stop("No PDF files found in data_dir: ", data_dir)
message("Found ", length(pdf_files), " PDF files")

# --- 2. Extract text (with OCR fallback)
texts_list <- lapply(pdf_files, safe_pdf_text)
names(texts_list) <- basename(pdf_files)

# Save raw extracted texts for reproducibility
raw_texts <- tibble(file_path = pdf_files, filename = basename(pdf_files), text = unlist(texts_list))
saveRDS(raw_texts, file = file.path(output_dir, "raw_texts.rds"))

# --- 3. Build metadata tibble and filter out missing texts
meta <- raw_texts %>%
  mutate(doc_id = paste0("doc", row_number())) %>%
  select(doc_id, file_path, filename, text)

meta_ok <- meta %>% filter(!is.na(text) & nzchar(text))
if (nrow(meta_ok) == 0) stop("No valid extracted text found after extraction")

# --- 4. Create quanteda corpus
corp <- corpus(meta_ok, text_field = "text", docid_field = "doc_id")

# --- 5. Docvars: agency and year
agencies <- c(
  "Department of Health", "Department of Agrarian Reform", "Department of Education",
  "Department of Public Works and Highways", "Department of Agriculture",
  "Department of Social Welfare and Development", "Department of Environment and Natural Resources",
  "Department of the Interior and Local Government", "Department of Foreign Affairs",
  "Department of Finance", "Department of Budget and Management", "Department of Tourism",
  "Department of Trade and Industry", "Department of Justice", "Department of Labor and Employment",
  "Department of Energy", "Department of Transportation", "Department of Science and Technology",
  "Department of National Defense", "Office of the President", "Presidential Communications Office",
  "Department of Human Settlements and Urban Development", "National Economic and Development Authority",
  "Department of Information and Communications Technology", "Department of Migrant Workers",
  "Department of Economy Planning and Development"
)

docvars(corp, "agency") <- mapply(function(txt, fname) detect_agency(txt, fname, agencies),
                                  meta_ok$text, meta_ok$filename, USE.NAMES = FALSE)

docvars(corp, "year") <- mapply(detect_year, meta_ok$text, meta_ok$filename, USE.NAMES = FALSE)

# Save corpus metadata snapshot
saveRDS(docvars(corp), file = file.path(output_dir, "docvars_snapshot.rds"))

# --- 6. Tokenization 
toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE
) %>%
  tokens_tolower()

# 1. Compound domain phrases
issue_phrases <- phrase(c(
  "cash advance",
  "unliquidated cash advance",
  "unliquidated fund",           
  "notice of disallowance",
  "audit observation",
  "procurement process",
  "procurement delay",           
  "procurement delays",          
  "bids and awards",
  "variation order",
  "change order"
))
toks <- tokens_compound(toks, issue_phrases)

# 2. Remove stopwords and clean remaining tokens
toks <- toks %>%
  tokens_remove(pattern = stopwords("en")) %>%
  tokens_keep(pattern = "^[a-z_]+$", valuetype = "regex") %>% 
  tokens_keep(min_nchar = 3)

# Save token object for inspection
saveRDS(toks, file = file.path(output_dir, "tokens_preclean.rds"))

# --- 7. Build DFM and trim
dfm_tokens <- dfm(toks)
dfm_tokens <- dfm_trim(dfm_tokens, min_termfreq = 5, min_docfreq = 3)

# Attach docvars back to dfm
docvars(dfm_tokens) <- docvars(corp)

# Save dfm for reproducibility
saveRDS(dfm_tokens, file = file.path(output_dir, "dfm_tokens.rds"))

