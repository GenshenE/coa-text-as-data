# Single script: read raw_texts.rds, remove leading boilerplate, extract agency & year metadata,
# compound phrases, remove custom stopwords, build & save cleaned_texts and dfm_cleaned.
# Outputs:
#   _outputs/cleaned_texts.rds
#   _outputs/dfm_cleaned.rds
#   _outputs/cleaning_diagnostics.rds
#   _outputs/docmap_dfm_cleaned.rds / .csv
#   _outputs/plot_doc_lengths.png
#   _outputs/plot_top_20_terms.png

# --- Libraries
library(tidyverse)
library(quanteda)
library(stringi)

# --- Config
input_rds <- "_outputs/raw_texts.rds"     
output_clean_texts <- "_outputs/cleaned_texts.rds"
output_dfm <- "_outputs/dfm_cleaned.rds"
output_diag <- "_outputs/cleaning_diagnostics.rds"
output_docmap_rds <- "_outputs/docmap_dfm_cleaned.rds"
output_docmap_csv <- "_outputs/docmap_dfm_cleaned.csv"
output_dropped <- "_outputs/dfm_cleaned_dropped_docs.csv"
output_plot_lengths <- "_outputs/plot_doc_lengths.png"
output_plot_terms <- "_outputs/plot_top_20_terms.png"

min_termfreq <- 5
min_docfreq  <- 3
min_token_length <- 3

# Ensure output directory exists
if (!dir.exists(dirname(output_clean_texts))) dir.create(dirname(output_clean_texts), recursive = TRUE)

# --- Boilerplate and agency canonical map
boilerplate_patterns <- c(
  "EXECUTIVE SUMMARY", "A\\.\\s*Introduction", "\\bINTRODUCTION\\b",
  "TABLE OF CONTENTS", "ANNEX", "ANNEXES", "SUMMARY OF FINDINGS",
  "THIS REPORT WAS PREPARED BY", "THIS AUDIT WAS CONDUCTED", "FOR MORE INFORMATION, CONTACT",
  "ALL RIGHTS RESERVED", "PAGE \\d+", "PAGE \\d+ OF \\d+", "REPUBLIC OF THE PHILIPPINES",
  "DEAR [A-Za-z]+", "SIRS?\\b|MADAM\\b|TO:\\b|CC:\\b|SUBJECT:\\b"
)

# canonical_map
canonical_map <- tibble::tribble(
  ~canonical, ~patterns,
  "Office of the President", "Office of the President|Office-of-the-President|\\bOP\\b",
  "Presidential Communications Office", "Presidential Communications Office|PCO|Presidential Comm",
  "Department of Economy Planning and Development", "Economy\\s*Planning|National Economic and Development Authority|NEDA|Economic and Development Authority|Department of Economy",
  "Department of Human Settlements and Urban Development", "Human Settlements|Housing and Urban Development|HSUD|Human Settlement",
  "Department of Information and Communications Technology", "Information and Communications Technology|ICT|DICT",
  "Department of Migrant Workers", "Migrant Workers|DMW|Department of Migrant",
  "Department of Health", "Department of Health|DOH",
  "Department of Agrarian Reform", "Department of Agrarian Reform",
  "Department of Education", "Department of Education",
  "Department of Public Works and Highways", "Department of Public Works and Highways",
  "Department of Agriculture", "Department of Agriculture",
  "Department of Social Welfare and Development", "Department of Social Welfare and Development",
  "Department of Environment and Natural Resources", "Department of Environment and Natural Resources",
  "Department of the Interior and Local Government", "Department of the Interior and Local Government",
  "Department of Foreign Affairs", "Department of Foreign Affairs",
  "Department of Finance", "Department of Finance",
  "Department of Budget and Management", "Department of Budget and Management",
  "Department of Tourism", "Department of Tourism",
  "Department of Trade and Industry", "Department of Trade and Industry",
  "Department of Justice", "Department of Justice",
  "Department of Labor and Employment", "Department of Labor and Employment",
  "Department of Energy", "Department of Energy",
  "Department of Transportation", "Department of Transportation",
  "Department of Science and Technology", "Department of Science and Technology",
  "Department of National Defense", "Department of National Defense",
  "Department of Human Settlements and Urban Development", "Department of Human Settlements and Urban Development"
)

# --- Helpers

# normalize filename or short string
normalize_name <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  nm <- x %>% str_remove("\\.pdf$") %>% str_replace_all("[-_]+", " ")
  nm <- stringi::stri_trans_general(nm, "Latin-ASCII")
  nm <- str_squish(nm)
  tolower(nm)
}

# match canonical patterns against a string
match_canonical <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  for (i in seq_len(nrow(canonical_map))) {
    pat <- canonical_map$patterns[i]
    if (str_detect(s, regex(pat, ignore_case = TRUE))) return(canonical_map$canonical[i])
  }
  return(NA_character_)
}

# extract agency from filename (preferred)
extract_agency_from_filename <- function(filename) {
  nm <- normalize_name(filename)
  cand <- match_canonical(nm)
  if (!is.na(cand)) return(cand)
  # explicit abbreviation map
  abbr_map <- c(NEDA = "Department of Economy Planning and Development",
                DICT = "Department of Information and Communications Technology",
                PCO  = "Presidential Communications Office",
                DMW  = "Department of Migrant Workers",
                HSUD = "Department of Human Settlements and Urban Development",
                OP   = "Office of the President",
                DOH  = "Department of Health")
  for (k in names(abbr_map)) {
    if (str_detect(nm, regex(paste0("\\b", k, "\\b"), ignore_case = TRUE))) return(abbr_map[[k]])
  }
  # fallback: take first 1-4 words as candidate (title case)
  parts <- unlist(strsplit(nm, "\\s+"))
  if (length(parts) >= 1) {
    candidate <- paste(parts[1:min(4, length(parts))], collapse = " ")
    return(str_to_title(candidate))
  }
  return(NA_character_)
}

# extract agency from raw text using stricter anchored checks
extract_agency_from_text <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_character_)
  # examine first ~800 chars split into lines
  lines <- unlist(strsplit(substr(text, 1, 800), "\\r?\\n"))
  lines <- str_squish(lines)
  lines <- lines[lines != ""]
  # check first 10 lines for canonical patterns or exact-line matches
  for (ln in head(lines, 10)) {
    cand <- match_canonical(ln)
    if (!is.na(cand)) return(cand)
    for (i in seq_len(nrow(canonical_map))) {
      if (str_detect(ln, regex(paste0("^", canonical_map$canonical[i], "$"), ignore_case = TRUE))) {
        return(canonical_map$canonical[i])
      }
    }
  }
  # last resort: search entire text but require word boundaries
  for (i in seq_len(nrow(canonical_map))) {
    pat <- paste0("\\b(", canonical_map$patterns[i], ")\\b")
    if (str_detect(text, regex(pat, ignore_case = TRUE))) return(canonical_map$canonical[i])
  }
  return(NA_character_)
}

# detect year (filename preferred)
detect_year <- function(text, filename) {
  yr_file <- str_extract(filename, "(19|20)\\d{2}")
  if (!is.na(yr_file)) return(as.integer(yr_file))
  
  anchor_pattern <- "(?i)(?:year ended december 31,|calendar year|\\bcy\\b|audit report)\\s*[:,-]?\\s*(20\\d{2}|19\\d{2})"
  yr_anchor <- str_match(text, anchor_pattern)[, 2] 
  if (!is.na(yr_anchor)) return(as.integer(yr_anchor))
  
  if (!is.na(text) && nzchar(text)) {
    all_years <- str_extract_all(text, "(19|20)\\d{2}")[[1]]
    if (length(all_years) > 0) {
      all_years <- as.integer(all_years)
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      valid_years <- all_years[all_years >= 1990 & all_years <= current_year]
      if (length(valid_years) > 0) {
        freq_table <- table(valid_years)
        return(as.integer(names(freq_table)[which.max(freq_table)]))
      }
    }
  }
  return(NA_integer_)
}

# safe print helper to avoid na.print issues
safe_print <- function(x, n = 100) {
  if (!is.character(getOption("na.print"))) options(na.print = "NA")
  if (is.table(x) || is.matrix(x) || is.vector(x)) {
    x2 <- tryCatch(as.data.frame(x), error = function(e) x)
    if (is.data.frame(x2)) {
      x2[is.na(x2)] <- "NA"
      print(utils::head(x2, n))
    } else {
      print(x)
    }
  } else if (is.data.frame(x)) {
    x[is.na(x)] <- "NA"
    print(utils::head(x, n))
  } else {
    print(x)
  }
}

# --- Load raw texts
if (!file.exists(input_rds)) stop("Input file not found: ", input_rds)
raw <- readRDS(input_rds)
if (!("text" %in% colnames(raw))) stop("raw_texts.rds must contain a 'text' column")

# --- Apply cleaning: extract agency (filename preferred), strip leading header, conservative global clean, extract year
message("Cleaning ", nrow(raw), " documents (header strip + conservative cleaning)")

# helper to strip leading header (keeps raw_text intact)
strip_leading_header <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(txt)
  txt2 <- gsub("\r\n", "\n", txt)
  txt2 <- gsub("\r", "\n", txt2)
  parts <- unlist(strsplit(txt2, "\n\\s*\n", perl = TRUE))
  if (length(parts) <= 1) return(txt2)
  first_block <- str_trim(parts[1])
  bp_regex <- paste0("(?i)", paste(c(boilerplate_patterns, canonical_map$patterns), collapse = "|"))
  if (str_detect(first_block, regex(bp_regex, ignore_case = TRUE)) || nchar(first_block) < 10) {
    return(paste(parts[-1], collapse = "\n\n"))
  }
  return(txt2)
}

conservative_global_clean <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(NA_character_)
  s <- txt
  s <- stri_replace_all_regex(s, "[\\x00-\\x1F\\x7F]+", " ")
  s <- stri_replace_all_regex(s, "\\b([A-Z]{2,}\\s+){3,}", " ")
  bp_regex <- paste0("(?i)(", paste(boilerplate_patterns, collapse = "|"), ")")
  s <- stri_replace_all_regex(s, bp_regex, " ")
  s <- stri_replace_all_regex(s, "(?i)\\b(Respectfully submitted|Very truly yours|Sincerely|Yours truly)\\b[\\s\\S]{0,200}", " ")
  s <- gsub("\\s+", " ", s)
  s <- trimws(s)
  s
}

cleaned <- raw %>%
  mutate(
    doc_id = if ("filename" %in% colnames(.)) filename else paste0("doc", row_number()),
    raw_text = text
  ) %>%
  rowwise() %>%
  mutate(
    # prefer filename extraction for your filename convention
    agency_from_filename = extract_agency_from_filename(filename),
    # also attempt text extraction from raw_text (before header stripping)
    agency_from_text = extract_agency_from_text(raw_text),
    # prefer filename; if filename yields NA, use text
    agency = ifelse(!is.na(agency_from_filename) & nzchar(agency_from_filename),
                    agency_from_filename,
                    ifelse(!is.na(agency_from_text) & nzchar(agency_from_text), agency_from_text, NA_character_)),
    agency_source = case_when(
      !is.na(agency_from_filename) & agency == agency_from_filename ~ "filename",
      !is.na(agency_from_text) & agency == agency_from_text ~ "text",
      TRUE ~ NA_character_
    ),
    stripped = strip_leading_header(raw_text),
    clean_text = conservative_global_clean(stripped),
    year = detect_year(raw_text, filename)
  ) %>%
  ungroup() %>%
  select(file_path, filename, doc_id, agency, agency_source, year, raw_text, stripped, clean_text)

# Save cleaned texts for reproducibility
saveRDS(cleaned, file = output_clean_texts)
message("Saved cleaned texts to ", output_clean_texts)

# --- Build corpus and tokens
doc_ids <- cleaned$doc_id
corp_clean <- corpus(cleaned$clean_text, docnames = doc_ids)

# attach docvars (keep filename, file_path, agency, year, agency_source)
docvars(corp_clean, "filename") <- cleaned$filename
docvars(corp_clean, "file_path") <- cleaned$file_path
docvars(corp_clean, "agency") <- cleaned$agency
docvars(corp_clean, "agency_source") <- cleaned$agency_source
docvars(corp_clean, "year") <- cleaned$year

# Save a docmap immediately for reproducibility
docmap_initial <- tibble(
  docname = docnames(corp_clean),
  filename = docvars(corp_clean)$filename,
  file_path = docvars(corp_clean)$file_path,
  agency = docvars(corp_clean)$agency,
  agency_source = docvars(corp_clean)$agency_source,
  year = docvars(corp_clean)$year,
  stringsAsFactors = FALSE
)
saveRDS(docmap_initial, file = output_docmap_rds)
write.csv(docmap_initial, file = output_docmap_csv, row.names = FALSE)

# --- Phrase compounding
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

toks <- tokens(corp_clean,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_compound(issue_phrases)

# --- 1.5 Consolidate substantive word variations
toks <- tokens_replace(
  toks, 
  pattern = c("accounts", "accounting", "projects"), 
  replacement = c("account", "account", "project")
)

# --- Custom stopwords: conservative list (do not remove agency if you want sector signal)
custom_stop <- c(
  "executive", "summary", "introduction", "dated", "page", "pages", 
  "section", "annex", "annexes", "republic", "philippines", "year", "date",
  "table", "figure", "iii", "december"
)

# --- Audit Boilerplate: Remove administrative noise
audit_boilerplate <- c(
  "recommend", "recommended", "recommends", "recommending", "recommendation", "recommendations",
  "audit", "audits", "auditing", "audited", "auditor", "auditors", 
  "agency", "agencies", "management", "report", "reports", "coa", 
  "total", "amount", "amounting"
)

toks <- toks %>%
  tokens_remove(pattern = stopwords("en")) %>%
  tokens_remove(pattern = custom_stop) %>%
  tokens_remove(pattern = audit_boilerplate) %>%
  tokens_keep(pattern = "^[a-z_]+$", valuetype = "regex") %>% 
  tokens_keep(min_nchar = min_token_length)

# Save token object for inspection
saveRDS(toks, file = file.path(dirname(output_dfm), "tokens_preclean.rds"))

# --- Build DFM and trim
dfm_clean <- dfm(toks)
# record docnames before trimming
docnames_before_trim <- docnames(dfm_clean)
dfm_clean <- dfm_trim(dfm_clean, min_termfreq = min_termfreq, min_docfreq = min_docfreq)
docnames_after_trim <- docnames(dfm_clean)

# If any documents were dropped by trimming, record them
dropped_docs <- setdiff(docnames_before_trim, docnames_after_trim)
if (length(dropped_docs) > 0) {
  write.csv(tibble(dropped = dropped_docs), file = output_dropped, row.names = FALSE)
  message("Documents dropped by dfm_trim: ", length(dropped_docs), " (saved to ", output_dropped, ")")
}

# Re-attach docvars 
if (!is.null(docvars(corp_clean))) {
  dv <- docvars(corp_clean)
  # set rownames to corpus docnames
  rownames(dv) <- docnames(corp_clean)
  # subset dv to the DFM doc order (this will preserve only docs present in dfm_clean)
  dv_sub <- dv[docnames(dfm_clean), , drop = FALSE]
  # coerce any list-columns to character safely
  dv_sub <- dv_sub %>% mutate(across(everything(), function(col) {
    if (is.list(col)) {
      v <- sapply(col, function(z) if (is.null(z)) NA_character_ else as.character(z), USE.NAMES = FALSE)
      return(v)
    } else {
      return(col)
    }
  }))
  docvars(dfm_clean) <- dv_sub
}

# Best-effort: fill any remaining NA agencies from filename-derived values
if ("agency" %in% names(docvars(dfm_clean))) {
  na_idx <- is.na(docvars(dfm_clean)$agency) | docvars(dfm_clean)$agency == ""
  if (any(na_idx)) {
    derived_agency <- sapply(docnames(dfm_clean), function(nm) {
      fname <- docmap_initial$filename[match(nm, docmap_initial$docname)]
      extract_agency_from_filename(fname)
    }, USE.NAMES = FALSE)
    docvars(dfm_clean)$agency[na_idx] <- derived_agency[na_idx]
    # mark source for filled ones
    docvars(dfm_clean)$agency_source[na_idx] <- "filename_fallback"
  }
}

# Save cleaned DFM
saveRDS(dfm_clean, file = output_dfm)
message("Saved cleaned DFM to ", output_dfm)

# --- Diagnostics
diag <- list(
  n_docs = ndoc(dfm_clean),
  n_terms = nfeat(dfm_clean),
  top_terms = topfeatures(dfm_clean, 100),
  top_prefixes = {
    prefixes <- substr(cleaned$clean_text, 1, 120)
    sort(table(tolower(gsub("\\s+", " ", prefixes))), decreasing = TRUE)[1:20]
  },
  agency_counts = if ("agency" %in% colnames(docvars(dfm_clean))) sort(table(docvars(dfm_clean)$agency), decreasing = TRUE) else NULL,
  year_counts = if ("year" %in% colnames(docvars(dfm_clean))) sort(table(docvars(dfm_clean)$year, useNA = "ifany"), decreasing = TRUE) else NULL,
  dropped_docs = dropped_docs
)
saveRDS(diag, file = output_diag)
message("Saved diagnostics to ", output_diag)

# Print quick diagnostics to console (use safe_print to avoid na.print issues)
message("Cleaned DFM dimensions: ", paste(dim(dfm_clean), collapse = " x "))
message("Top 30 features after cleaning:")
safe_print(topfeatures(dfm_clean, 30))

if (!is.null(diag$agency_counts) && length(diag$agency_counts) > 0) {
  message("Top agencies (doc counts):")
  safe_print(head(diag$agency_counts, 40))
} else {
  message("No non-missing agency counts available in dfm docvars (all NA or none).")
}

if (!is.null(diag$year_counts)) {
  message("Year distribution (including NA):")
  safe_print(diag$year_counts)
} else {
  message("No 'year' docvar present in dfm docvars.")
}

# --- Visual Diagnostics Generation

# 1. Document Length Distribution Histogram
# Extract token counts per document
doc_lengths <- rowSums(dfm_clean)
df_lengths <- data.frame(length = doc_lengths)
median_len <- median(df_lengths$length)
mean_len <- mean(df_lengths$length)

p_len <- ggplot(df_lengths, aes(x = length)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = median_len), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_len), color = "green4", linetype = "dotted", size = 1) +
  theme_minimal() +
  labs(title = "Document length distribution",
       x = "Tokens per document",
       y = "Count")

ggsave(output_plot_lengths, plot = p_len, width = 8, height = 5)
message("Saved Document Length Histogram to ", output_plot_lengths)

# 2. Top 20 Terms Bar Chart
top_20 <- topfeatures(dfm_clean, 20)
df_top_20 <- data.frame(term = names(top_20), frequency = top_20)

p_terms <- ggplot(df_top_20, aes(x = reorder(term, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Terms",
       x = "Term",
       y = "Frequency")

ggsave(output_plot_terms, plot = p_terms, width = 8, height = 6)
message("Saved Top 20 Terms Bar Chart to ", output_plot_terms)

# Save a final docmap aligned to the DFM doc order for downstream reproducibility
docmap_final <- tibble(
  docname = docnames(dfm_clean),
  filename = docvars(dfm_clean)$filename,
  file_path = docvars(dfm_clean)$file_path,
  agency = if ("agency" %in% names(docvars(dfm_clean))) docvars(dfm_clean)$agency else NA_character_,
  agency_source = if ("agency_source" %in% names(docvars(dfm_clean))) docvars(dfm_clean)$agency_source else NA_character_,
  year = if ("year" %in% names(docvars(dfm_clean))) docvars(dfm_clean)$year else NA_integer_
)
saveRDS(docmap_final, file = output_docmap_rds)
write.csv(docmap_final, file = output_docmap_csv, row.names = FALSE)
