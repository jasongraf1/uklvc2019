
# -------------------------------------------------------------------------
# File: 00_load_data.R
# Load in datasets
# -------------------------------------------------------------------------

source(".Rprofile")

# Genitives data ----------------------------------------------------------

gens <- read.delim("data/brown_gens_simple.txt")

gens[c(2:4,6,7,12,13,16:22)] <- lapply(gens[c(2:4,6,7,12,13,16:22)], as.factor)

gens <- gens %>%
  mutate(
    Possessor.Animacy3 = relevel(Possessor.Animacy3, ref = "inanimate")
  )

# Split into separate dataframes
gen_data_list <- split(gens, list(gens$Corpus, gens$Genre)) %>%
  lapply(FUN = droplevels)

# Shorten names for convenience
names(gen_data_list) <- names(gen_data_list) %>%
  gsub('-f', ' F', .) %>% # get rid of hyphen
  abbreviate(minlength = 7L)

