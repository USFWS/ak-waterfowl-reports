library(tidyverse)
library(AKaerial)
library(fs)
library(jinjar)

load("data/YKD_Report.RData")

sppdf <- data.frame(sppAOU = sppAOU, 
    sppArea = ifelse(sppAOU %in% c("EMGO", "GWFG", "BRAN", "CCGO", "TAVS", "SWAN", "SACR"), 
                     "YKG", "YKD"), 
    spptext = spptext, 
    index11 = ifelse(sppAOU %in% c("SWAN", "UNSC", "JAEG", "RTLO", "PALO"), 
                     "total", "itotal"),
    index22 = ifelse(sppAOU %in% c("SWAN", "UNSC", "JAEG", "RTLO", "PALO"), 
                     "sing1pair2", "ibb"),
    figlab = paste0("fig-", sppAOU), 
    tbllab = paste0("tbl-", sppAOU),
    maplab = paste0("fig-", sppAOU, "map"))
#put in preferred order
sppdf <- sppdf[which(sppdf$sppAOU %in% c("EMGO", "GWFG", "BRAN", "CCGO", "TAVS", 
                                       "SWAN", "NOSH", "AMWI", "MALL", "NOPI", 
                                       "GWTE", "UNSC", "SPEI", "COEI", "SUSC", 
                                       "BLSC", "LTDU", "RBME", "SACR", "JAEG", 
                                       "RTLO", "PALO")),]
#replace name of green-winged teal
sppdf$spptext[sppdf$sppAOU=="GWTE"] <- "Green-winged teal"
#name of index
sppdf$name11 = ifelse(sppdf$index11 == "itotal", "Indicated Total Birds", "Total Birds")
sppdf$name22 = ifelse(sppdf$index22 == "ibb", "Indicated Breeding Birds", "Breeding Birds")
sppdf$figcap = ifelse(sppdf$index11 == "itotal", "cap21", "cap22")
sppdf$tabcap = ifelse(sppdf$index11 == "itotal", "cap31", "cap32")


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

