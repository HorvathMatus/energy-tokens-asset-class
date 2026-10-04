# =============================================================================
# 11_classification.R   (Referee point #22)
# Unsupervised classification: do energy tokens cluster with crypto assets or
# with energy/green assets? Four complementary, model-free views:
#   (1) Hierarchical clustering on correlation distance (Ward.D2) -> dendrogram
#   (2) PCA of the return correlation matrix -> PC1/PC2 loading map
#   (3) Distance correlation (nonlinear) of each token vs crypto vs energy block
#   (4) Network community detection (Louvain) on the correlation graph
#
# All 30 assets; all six tokens shown.
# Outputs:
#   Output/identity/cluster_membership.csv, dcor_tokens.csv, communities.csv
#   figures: dendrogram_assets.png, pca_loadings.png
# =============================================================================

rm(list = ls())
suppressMessages({
  library(tidyverse)
  library(igraph)
})

load("Data/returns_full30.RData")   # R30, GROUP, PAPER30, block vectors
dir.create("Output/identity", showWarnings = FALSE, recursive = TRUE)
fig_dir <- "Output/figures/identity"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Group palette
grp_levels <- c("Energy token", "Cryptocurrency", "Crypto index",
                "ESG / green finance", "Commodity", "Energy equity")
grp_cols <- c("Energy token"        = "#e63946",
              "Cryptocurrency"      = "#6a4c93",
              "Crypto index"        = "#1d3557",
              "ESG / green finance" = "#2a9d8f",
              "Commodity"           = "#e9c46a",
              "Energy equity"       = "#457b9d")
grp_of <- function(a) GROUP$group[match(a, GROUP$asset)]

# ── Correlation matrix (pairwise complete) ───────────────────────────────────
X  <- as.matrix(R30[, PAPER30])
C  <- cor(X, use = "pairwise.complete.obs")

# ── (1) Hierarchical clustering ──────────────────────────────────────────────
D  <- as.dist(1 - C)
hc <- hclust(D, method = "ward.D2")

png(file.path(fig_dir, "dendrogram_assets.png"), width = 2400, height = 1300, res = 220)
op <- par(mar = c(7, 4, 2, 1))
# colour leaf labels by group
dend_order <- hc$order
lab_cols <- grp_cols[grp_of(PAPER30)][dend_order]
plot(hc, hang = -1, cex = 0.8, main = "", xlab = "", sub = "",
     ylab = "Correlation distance (1 - rho)", labels = FALSE)
# overlay coloured labels (only these; default labels suppressed above)
mtext(side = 1, line = 0.2, at = seq_along(dend_order),
      text = PAPER30[dend_order], col = lab_cols, cex = 0.7, las = 2)
par(op); dev.off()

# Cut into k clusters and record membership
cm_list <- list()
for (kk in c(3, 4, 5, 6)) {
  cl <- stats::cutree(hc, k = kk)        # named vector, names = asset labels
  cm_list[[as.character(kk)]] <- data.frame(
    asset   = PAPER30,
    group   = grp_of(PAPER30),
    k       = kk,
    cluster = as.integer(cl[PAPER30])    # align by name -> one value per asset
  )
}
cluster_membership <- do.call(rbind, cm_list)
write_csv(cluster_membership, "Output/identity/cluster_membership.csv")

# Which cluster do tokens vs crypto land in (k = 4)?
k4 <- cluster_membership %>% filter(k == 4)
crypto_clusters <- k4 %>% filter(group %in% c("Cryptocurrency", "Crypto index")) %>%
  count(cluster) %>% arrange(desc(n))
token_clusters  <- k4 %>% filter(group == "Energy token") %>% select(asset, cluster)

# ── (2) PCA of the correlation matrix ────────────────────────────────────────
eig <- eigen(C, symmetric = TRUE)
ev  <- pmax(eig$values, 0)
load12 <- tibble(
  asset = PAPER30,
  group = factor(grp_of(PAPER30), levels = grp_levels),
  PC1   = eig$vectors[, 1] * sqrt(ev[1]),
  PC2   = eig$vectors[, 2] * sqrt(ev[2])
)
# orient PC1 so crypto loads positive
if (mean(load12$PC1[load12$group == "Cryptocurrency"]) < 0) load12$PC1 <- -load12$PC1
if (mean(load12$PC2[load12$group == "Commodity"]) < 0)      load12$PC2 <- -load12$PC2
var_expl <- round(100 * ev / sum(ev), 1)

p_pca <- ggplot(load12, aes(PC1, PC2, colour = group, label = asset)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(size = 2.4) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 30, show.legend = FALSE) +
  scale_colour_manual(values = grp_cols, name = NULL) +
  labs(x = paste0("PC1 (", var_expl[1], "% var)"),
       y = paste0("PC2 (", var_expl[2], "% var)")) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "pca_loadings.png"), p_pca,
       width = 9, height = 7, dpi = 300)

# ── (3) Distance correlation (nonlinear) vs crypto / energy block ────────────
dcor_fast <- function(x, y) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]; n <- length(x)
  A <- as.matrix(dist(x)); B <- as.matrix(dist(y))
  Ac <- A - rowMeans(A)[row(A)] - colMeans(A)[col(A)] + mean(A)
  Bc <- B - rowMeans(B)[row(B)] - colMeans(B)[col(B)] + mean(B)
  dcov <- sqrt(max(mean(Ac * Bc), 0))
  vx <- sqrt(max(mean(Ac * Ac), 0)); vy <- sqrt(max(mean(Bc * Bc), 0))
  if (vx * vy == 0) return(NA_real_)
  dcov / sqrt(vx * vy)
}
crypto_f <- rowMeans(scale(R30[, c(CRYPTO, CRYPTO_IX)]))
energy_f <- rowMeans(scale(R30[, c(COMMOD, ENERGY_EQ)]))
dcor_tokens <- map_dfr(TOKENS_ALL, function(tok) {
  tibble(token = tok,
         dcor_crypto = round(dcor_fast(R30[[tok]], crypto_f), 3),
         dcor_energy = round(dcor_fast(R30[[tok]], energy_f), 3))
}) %>% mutate(ratio = round(dcor_crypto / dcor_energy, 2))
write_csv(dcor_tokens, "Output/identity/dcor_tokens.csv")

# ── (4) Network community detection (Louvain) ────────────────────────────────
Cpos <- C; Cpos[Cpos < 0.20] <- 0; diag(Cpos) <- 0   # keep moderate+ positive links
g <- graph_from_adjacency_matrix(Cpos, mode = "undirected",
                                 weighted = TRUE, diag = FALSE)
set.seed(2026)
comm <- cluster_louvain(g, weights = E(g)$weight)
communities <- tibble(asset = PAPER30, group = grp_of(PAPER30),
                      community = membership(comm)[PAPER30])
write_csv(communities, "Output/identity/communities.csv")

# ── Console summary ──────────────────────────────────────────────────────────
cat("\n", strrep("=", 70), "\n", sep = "")
cat("(1) HIERARCHICAL CLUSTERING (k = 4): token cluster membership\n")
cat(strrep("=", 70), "\n", sep = "")
print(token_clusters)
cat("\nCrypto assets concentrate in cluster(s):\n"); print(crypto_clusters)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("(3) DISTANCE CORRELATION (nonlinear): token vs crypto vs energy block\n")
cat(strrep("=", 70), "\n", sep = "")
print(dcor_tokens)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("(4) LOUVAIN COMMUNITIES: do tokens share crypto's community?\n")
cat(strrep("=", 70), "\n", sep = "")
print(communities %>% arrange(community, group), n = 30)

message("\nSaved tables to Output/identity/ and figures to ", fig_dir)
