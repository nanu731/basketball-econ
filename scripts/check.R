#!/usr/bin/env Rscript
# Syntax checks and offline public-boundary tests; no network or installation.
files <- sort(list.files(c("R", "scripts", "tests"), pattern = "\\.R$",
                         full.names = TRUE, recursive = TRUE))
for (file in files) parse(file)
cat("Syntax checked:", length(files), "R files\n")
source("tests/run.R")
source("tests/sensitivity.R")

source("tests/browser-scores.R")
source("tests/browser-publication.R")
