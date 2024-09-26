# Illumina Sample Sheet Checker

This Shiny application checks Illumina sample sheets for common issues such as sample name duplication, barcode (index) similarity, and ensures that indices only contain valid nucleotides. It is designed to help users verify their sample sheets before sequencing, preventing issues in downstream processes.

## Features

- Upload a CSV file containing sample sheet data.
- Check for duplicate sample names within the same lane with different indices.
- Check if sample names exceed 40 characters or contain invalid characters.
- Ensure that `index` and `index2` only contain valid characters (A, C, G, T, N).
- Calculate Hamming distances between barcodes (index) within the same lane to identify similar barcodes.
- Color-coded output:
  - Red: Invalid `index` or `index2` (contains invalid characters).
  - Yellow/Red: Similar barcodes (based on Hamming distance).
  - Green: Valid sample names and barcodes.
  - Red: Sample name does not meet the Illumina requirements (invalid characters or too long).
- Option to download the processed CSV file with errors and corrections.

## Availability

- https://pettestor.shinyapps.io/illumina_sample_sheet_checker/

## Prerequisites

- R version 3.6.0 or higher.
- The following R packages:
  - `shiny`
  - `dplyr`
  - `stringdist`
  - `DT`
  - `janitor`

To install the required packages, run the following in your R console:

```r
install.packages(c("shiny", "dplyr", "stringdist", "DT", "janitor"))
