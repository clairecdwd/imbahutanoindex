# =============================================================================
# IMBA Hutano - run the full analysis pipeline
#   01 data preparation -> 02 analysis -> 03 formatted tables
# Run from the repository root:  source("run_all.R")
# =============================================================================

# 01 rebuilds the minimal dataset from RESTRICTED source data; it is skipped if
# the source files are not present (the shipped data/imba_analysis_dataset.Rds
# is used instead). Point IMBA_DATA_RAW at the source files to rebuild.
data_raw <- Sys.getenv("IMBA_DATA_RAW", "data_raw/")
if (file.exists(file.path(data_raw, "IE_analysis.Rds"))) {
  message(">> 01_data_preparation.R")
  source("R/01_data_preparation.R")
} else {
  message(">> Skipping 01 (restricted source data not found in '", data_raw,
          "'); using shipped data/imba_analysis_dataset.Rds")
}

# main analysis -> main tables
message(">> 02_analysis.R")
source("R/02_analysis.R")
message(">> 03_tables.R")
source("R/03_tables.R")

# supplementary analysis -> supplementary tables
message(">> 02b_analysis_supplement.R")
source("R/02b_analysis_supplement.R")
message(">> 03b_tables_supplement.R")
source("R/03b_tables_supplement.R")

# additional analyses requested at peer review -> reviewer tables
message(">> 05_analysis_reviewer.R")
source("R/05_analysis_reviewer.R")
message(">> 05b_tables_reviewer.R")
source("R/05b_tables_reviewer.R")

message(">> Done. Tables: imba_tables.docx, imba_supplementary_tables.docx, imba_reviewer_tables.docx")
