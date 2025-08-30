# Function to classify genes using implicit return
classify_gene <- function(logFC, padj) { # nolint
  if (is.na(padj)) padj <- 1
  if (logFC > 1 && padj < 0.05) {
    "Upregulated"
  } else if (logFC < -1 && padj < 0.05) {
    "Downregulated"
  } else {
    "Not_Significant"
  }
}

# Create Results folder if it does not exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

# Input files
files <- c("DEGs_data_1.csv", "DEGs_data_2.csv")

# Process each dataset
for (file in files) {
  df <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

  df$padj[is.na(df$padj)] <- 1
  df$status <- mapply(classify_gene, df$logFC, df$padj)

  out_file <- file.path("Results", paste0("processed_", file))
  write.csv(df, out_file, row.names = FALSE)

  cat("\nSummary for:", file, "\n")
  print(table(df$status))
}
