# TEMPORARY FILE FOR MANUAL CHECKING
# WARNING: the file paths are hard coded, so this script will work *only*
# if your file paths match exactly those listed here

source("data-prep.R")

Dirs <- read_csv("../ImCo-secure-data-prep/secure-metadata.csv") %>%
  rename(sub_num = public_id, 
         Dir = chatterlab_id)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You must enter at least one object name.", call. = FALSE)
} 

txt <- data.to.export %>%
  left_join(Dirs, by = "sub_num") %>%
  filter(object == as.character(args[1])) %>%
  mutate(check = paste0("images/", Dir, "/", image)) %>%
  distinct() %>%
  select(check)

if (nrow(txt) > 0) {
  write.table(txt, "../manual-checks/specific-objects/objects-to-check.txt", sep = "\t", 
              row.names = FALSE, col.names = FALSE, 
              quote = FALSE)
}

csv <- data.to.export %>%
  left_join(Dirs, by = "sub_num") %>%
  filter(object == as.character(args[1])) %>%
  mutate(check = paste0("images/", Dir, "/", image)) %>%
  distinct() %>%
  select(sub_num, image)

if (nrow(csv) > 0) {
  write_csv(csv, "../manual-checks/specific-objects/objects-to-check.csv")
  message(paste0("You have ", nrow(csv), " object(s) to check."))
} else {
  message("Hooray! There are no objects to check.")
}