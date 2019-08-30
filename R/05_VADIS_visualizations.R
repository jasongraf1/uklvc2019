# -------------------------------------------------------------------------
# File: 05_VADIS_visualizations.R
# Plot visualizations of VADIS analysis
# -------------------------------------------------------------------------

# Load in the datasets
source("R/00_load_data.R")

# set ggplot2 options
theme_set(theme_minimal())

line1_dist <- read.csv("output/line1_dist_matrix.csv") %>%
  as.dist()
line2_dist <- read.csv("output/line2_dist_matrix.csv") %>%
  as.dist()
line3_dist <- read.csv("output/line3_dist_matrix.csv") %>%
  as.dist()

# Neighbor Net ------------------------------------------------------------

# Plot a neighbornet of the distance matrix for the 2nd line of evidence
nnet2 <- neighborNet(coef_line$distance.matrix)
labs <- nnet2$tip.label
labs <- gsub("AF", "AdvtrFic", labs)
labs <- gsub("GF", "GenFic", labs)
labs <- gsub("NF", "NonFic", labs)
labs <- gsub("Pr", "Press", labs)
labs <- gsub("Lr", "Acad", labs)
labs <- gsub("Brwn\\.(.*)", "\\1\\.60s", labs)
labs <- gsub("Frwn\\.(.*)", "\\1\\.90s", labs)
nnet2$tip.label <- labs
# cols <- ifelse(grepl("60s", labs), "#0073C2FF", "#CD534CFF")
cols <- ifelse(grepl("60s", labs), "blue", "red")
tiff("figures/nnet_plot.tiff", width = 5.5, height = 4, pointsize = 1/300, units = 'in', res = 300)
par(mar = c(0, 1, 0, 0))
set.seed(98)
plot(nnet2, "2D", tip.color = cols)
dev.off()

# MDS plot ----------------------------------------------------------------

# Plot an MDS analysis of the distance matrix for the 2nd line of evidence

# Plot using the mean posterior estimates
mds2 <- cmdscale(line2_dist, k = 3) %>%
  as.data.frame() %>%
  rename(x = "V1", y = "V2", z = "V3") %>%
  mutate(
    Variety = rownames(.),
    Time = factor(ifelse(grepl("Brwn", Variety), "1960s", "1990s")),
    Genre = factor(gsub("[BF]rwn.", "", Variety)),
    Genre = fct_recode(Genre,
                       AdvtrFic = "AF",
                       GenFic = "GF",
                       NonFic = "NF",
                       Press = "Pr",
                       Acad = "Lr"),
    Label = paste(Genre, gsub("19", "", Time), sep = ".")
  )

ggplot(mds2, aes(x, y, color = Genre)) +
  geom_text(aes(label = Variety)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "", y = "")

# A nice think about using Bayesian models is that we can derive an estimate of
# the uncertainty in our measures of (dis)similarity by sampling directly from
# the posterior estimates.

# Define function for sampling from the posteriors. I'm using the 50% HPDI here,
# so only coefficient values falling between the 1st and 3rd quartiles are considered.
# The function draws n samples from the posterior distributions and creates a
# table of coefficients for each variety. Basically creating n versions of the
# 'coef_line$coef.table' each with coeffients randomly sampled from the specified HPDI.
get_post_samples <- function(mod_list, n = 200, lower = .25, upper = .75){
  sample_HDI <- function(x, n, lower, upper) {
    lq <- quantile(x, lower) # lower quantile
    uq <- quantile(x, upper) # upper quantile
    sample(x[x > lq & x < uq], n)
    # need to include error catch for when sample size is too high for interval
  }
  output_list <- vector("list")
  for(i in seq_along(mod_list)){
    cat(i, ", ", sep = "")
    m <- mod_list[[i]]
    if(class(m) == "brmsfit"){
      sample_df <- brms::posterior_samples(m, pars = '^b')[,-1]
      trimmed_df <- lapply(
        sample_df,
        FUN = function(x) sample_HDI(x, n, lower, upper)) %>%
        do.call("cbind", .) %>%
        as.data.frame()
      output_list[[i]] <- trimmed_df
    }
  }
  names(output_list) <- names(mod_list)
  return(output_list)
}

# Define function for creating a list of the MDS plots. The idea is to make an MDS
# plot for each of our n samples, and then superimpose these plots onto one
# another. This gives us a cloud of points representing the variability in our
# similarity estimates. Denser clouds represent more reliable estimates.
# `mds.org` is the original MDS based on the posterior means.
create_mds_list <- function(sample_list, mds.orig){
  k <- ncol(mds.orig) # number of dimensions
  mds_list <- vector("list")
  ncoef <- ncol(sample_list[[1]]) # number of coefficients
  nvar <- length(sample_list) # number of varieties
  coefs <- names(sample_list[[1]])
  vars <- names(sample_list)
  n <- nrow(sample_list[[1]])
  for(i in 1:n){
    cat(i, ", ", sep = "")
    cur_list <- vector("list")
    for(j in seq_along(sample_list)){
      cur_list[[j]] <- sample_list[[j]][i,]
    }
    cur_df <- do.call("rbind", cur_list) %>%
      as.data.frame()
    rownames(cur_df) <- vars
    cur_dist <- dist(cur_df, "euclidean")
    # We normalize the distances following the VADIS method
    dmy <- data.frame(a = sample(c(1,-1), size = ncol(cur_df), replace = T))
    dmy$b <- -dmy$a # exact opposite of a
    maxD <- max(dist(t(dmy), "euclidean"))
    cur_dist_norm <- cur_dist/maxD
    # create mds plot
    cur_mds <- cmdscale(cur_dist_norm, k = k) %>%
      as.data.frame()
    # Align the axes. The cmdscale function can arbitrarily flip the axes, so
    # in order to properly superimpose the plots we need to make sure the axes
    # are correctly aligned. Do this by checking to see that each dimension in
    # the new MDS is positively correlated with the MDs based on the means
    for (dm in 1:k){
      if(cor(mds.orig[, dm], cur_mds[, dm]) < 0) {
        cur_mds[, dm] <- cur_mds[, dm]*-1
      }}
    mds_list[[i]] <- cur_mds
  }
  names(mds_list) <- paste0("sample.", 1:n)
  return(mds_list)
}

# Draw 200 samples from the 50% HPDI posterior distributions of each predictor
draw_samples <- get_post_samples(gen_brms_list, n = 200)
# Create the mds plots for each random draw
mds_samples <- create_mds_list(draw_samples, mds2[,1:3])

# Now plot the results
plot1 <- bind_rows(mds_samples, .id = "run") %>%
  mutate(
    Variety = factor(rep(names(gen_brms_list), 200)),
    Time = factor(ifelse(grepl("Brwn", Variety), "1960s", "1990s")),
    Genre = factor(gsub("[BF]rwn.", "", Variety)),
    Genre = fct_recode(Genre,
      AdvtrFic = "AF",
      GenFic = "GF",
      NonFic = "NF",
      Press = "Pr",
      Acad = "Lr"),
    Label = paste(Genre, gsub("19", "", Time), sep = ".")
  ) %>%
  rename(x = "V1", y = "V2", z = "V3") %>%
  ggplot(aes(x, y, col = Genre, shape = Time)) +
  geom_point(alpha = .5) +
  geom_label(data = mds2, aes(label = Label, fill = Genre), size = 4, col = "white",
             fontface = "bold") +
  # geom_label(data = mds2[mds2$Corpus == "F",],
  #            aes(label = Variety, col = Genre), size = 4, fill = "white",
  #            fontface = "bold") +
  scale_color_jco(guide = "none") +
  scale_fill_jco(guide = "none") +
  scale_shape_manual(values = c(16, 17)) +
  # scale_color_brewer(palette = "Set1") +
  # scale_fill_brewer(palette = "Set1") +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  labs(x = "", y = "")

tiff("figures/mds_plot.tiff", width = 6, height = 4, pointsize = 1/300, units = 'in', res = 300)
plot1
dev.off()


