library(tidyverse)
library(AKaerial)
library(fs)
library(jinjar)

load("data/YKD_Report.RData")

sppdf <- data.frame(sppAOU = sppAOU, 
    sppArea = ifelse(sppAOU %in% c("EMGO", "GWFG", "BRAN", "CCGO", "TAVS", "SWAN", "SACR"), 
                     "YKG", "YKD"), 
    spptext = spptext, 
    figlab = paste0("fig-", sppAOU), 
    tbllab = paste0("tbl-", sppAOU),
    maplab = paste0("fig-", sppAOU, "map"))

# Parse the jinja template
template <- parse_template(
  path("appendices.qmd.jinja"),
  .config = jinjar_config(
    trim_blocks = TRUE,
    lstrip_blocks = TRUE
  )
)

# Render quarto
quarto <- render(
  template,
  sppdata = sppdf
)

# Search/replace stuff I couldn't figure out how to embed directly
quarto <- gsub("PAGEBREAK", "{{< pagebreak >}}", quarto)

# Write Quarto doc
write_lines(
  quarto,
  "_appendices.qmd"
)

