# =============================================================================
# 20_jcm_network_figures.R
# Figures for the JCM token-centred systems (token, 24 other assets, four
# electricity markets, aligned trading sessions), estimated by
# julia/network_jcm.jl. Same styles as 06_graphs_frequencies_tokens.R and
# 07_heatmaps.R:
#   Total_<TOKEN>.png        total connectedness and its three bands
#   NET_<band>_<TOKEN>.png   net connectedness of the token in each band
#   heatmap_total_<TOKEN>.png, network_total_circular_<TOKEN>.png
# Output: Output/figures/results_jcm/
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(lubridate); library(igraph); library(ggraph) })

load("Data/returns_full30.RData")                        # R30 (dates of the Julia input rows)
out_dir <- "Output/figures/results_jcm"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ORDER24 <- c("BTC", "ETH", "SOL", "AVAX", "DPI", "IDEX", "SPTOP20", "EUA", "NEE", "ENPH", "PBW", "PBD",
             "FENY", "QCLN", "ICLN", "B", "CL", "HH", "GC", "HG", "LIT", "ESGD", "ESGU", "BGRN")
ELEC <- c("EL_DE", "EL_SI", "EL_ITN", "EL_AU")
TOK <- c(PWR = "POWR", SNC = "SNC", EWT = "EWT")
events <- tibble(xmin = as.Date(c("2021-01-01", "2022-04-01")), xmax = as.Date(c("2021-04-01", "2022-10-01")),
                 event = c("Event 1", "Event 2"))
rd <- function(f) t(read.csv(f, header = FALSE))[, -1] |> apply(2, as.numeric)

for (sys in names(TOK)) {
  tag <- paste0(sys, "_elecs"); nm <- c(TOK[[sys]], ORDER24, ELEC); N <- length(nm)
  C <- rd(sprintf("julia/Output/Chorizon_%s.csv", tag))
  L <- rd(sprintf("julia/Output/CIhorizon1_%s.csv", tag)); U <- rd(sprintf("julia/Output/CIhorizon2_%s.csv", tag))
  dates <- tail(R30$Date, nrow(C))

  # ── total connectedness and bands ─────────────────────────────────────────
  series <- c("Total connectedness" = 1, "Long term (>20 days)" = 2, "1 month (5–20 days)" = 3, "1 week (1–5 days)" = 4)
  d <- bind_rows(lapply(names(series), function(s) tibble(Time = dates, Series = s, Value = C[, series[[s]]],
                                                          lo = L[, series[[s]]], hi = U[, series[[s]]])))
  cols <- c("Total connectedness" = "black", "1 week (1–5 days)" = "darkgreen", "1 month (5–20 days)" = "blue",
            "Long term (>20 days)" = "red")
  fills <- c("Total connectedness" = "dimgrey", "1 week (1–5 days)" = "darkgreen", "1 month (5–20 days)" = "blue",
             "Long term (>20 days)" = "red")
  p_tot <- ggplot(d, aes(Time)) + theme_classic() +
    geom_rect(data = events, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = event),
              inherit.aes = FALSE, alpha = 0.12) +
    scale_fill_manual(values = c("Event 1" = "lightgreen", "Event 2" = "orange"), guide = "none") +
    ggnewscale::new_scale_fill() +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = Series), alpha = 0.2, show.legend = FALSE) +
    scale_fill_manual(values = fills, guide = "none") +
    geom_line(aes(y = Value, colour = Series), linewidth = 0.8) +
    scale_colour_manual(name = NULL, values = cols, breaks = names(cols)) +
    ylim(0, 100) + scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
    labs(y = "Connectedness (%)", x = "Date") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
          legend.position = "inside", legend.position.inside = c(0.98, 1.03), legend.justification = c(1, 1),
          legend.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA))
  ggsave(file.path(out_dir, sprintf("Total_%s.png", TOK[[sys]])), p_tot, width = 5000, height = 3000, dpi = 500,
         units = "px", bg = "transparent")

  # ── net connectedness of the token in each band (token is column 1) ───────
  base <- c(long = 8 + 16 * N, medium = 8 + 17 * N, short = 8 + 18 * N)
  for (b in names(base)) {
    k <- base[[b]]
    dn <- tibble(Time = dates, v = C[, k], lo = L[, k], hi = U[, k])
    p <- ggplot(dn, aes(Time)) + theme_classic() +
      geom_rect(data = events, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = event),
                inherit.aes = FALSE, alpha = 0.12) +
      scale_fill_manual(values = c("Event 1" = "lightgreen", "Event 2" = "orange"), guide = "none") +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey70") + geom_line(aes(y = v)) +
      geom_hline(yintercept = 0, col = "red") +
      scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + labs(y = "Dynamic NET", x = "Date") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    ggsave(file.path(out_dir, sprintf("NET_%s_%s.png", b, TOK[[sys]])), p, width = 5000, height = 3000, dpi = 500,
           units = "px", bg = "transparent")
  }

  # ── average pairwise table: heatmap and network ───────────────────────────
  A <- as.matrix(read.csv(sprintf("julia/Output/avg_tab_total_%s.csv", tag), header = TRUE))
  dimnames(A) <- list(nm, nm)
  dfh <- as.data.frame(as.table(A)) %>% mutate(Var1 = factor(Var1, nm), Var2 = factor(Var2, nm))
  p_h <- ggplot(dfh, aes(Var2, Var1, fill = Freq)) + geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA)) + labs(x = NULL, y = NULL)
  ggsave(file.path(out_dir, sprintf("heatmap_total_%s.png", TOK[[sys]])), p_h, width = 3200, height = 3200, dpi = 500,
         units = "px", bg = "transparent")

  el <- data.frame(from = rep(nm, each = N), to = rep(nm, times = N), weight = as.vector(t(A))) %>%
    filter(weight > 0.05, from != to)
  g <- graph_from_data_frame(el, directed = FALSE, vertices = data.frame(name = nm))
  V(g)$degree <- degree(g)
  p_n <- ggraph(g, layout = "circle") +
    geom_edge_link(aes(alpha = weight), colour = "gray70", width = 0.5) +
    geom_node_point(aes(size = degree), colour = "steelblue", alpha = 0.7) +
    geom_node_text(aes(label = name), size = 4, fontface = "bold") +
    scale_edge_alpha(range = c(0.1, 0.5)) + scale_size_continuous(range = c(8, 25)) +
    coord_fixed(clip = "off") + theme_void() +
    theme(legend.position = "none", plot.margin = margin(40, 40, 40, 40),
          plot.background = element_rect(fill = "transparent", colour = NA))
  ggsave(file.path(out_dir, sprintf("network_total_circular_%s.png", TOK[[sys]])), p_n, width = 3000, height = 3000,
         dpi = 300, units = "px", bg = "transparent")

  # summary numbers for the text
  tc <- colMeans(C[, 1:4]); net_tok <- colMeans(C[, base])
  own <- mean(diag(A)); tok_from <- A[1, ]; tok_to <- A[, 1]
  cat(sprintf("%s total %.1f  long %.1f  medium %.1f  short %.1f | token net (long, medium, short) %s | max total %.1f on %s\n",
              TOK[[sys]], tc[1], tc[2], tc[3], tc[4], paste(round(net_tok, 3), collapse = ", "),
              max(C[, 1]), format(dates[which.max(C[, 1])])))
  cat("  top sources of token variance:", paste(sprintf("%s %.3f", names(sort(tok_from[-1], TRUE))[1:6],
                                                        sort(tok_from[-1], TRUE)[1:6]), collapse = ", "), "\n")
  cat("  share of token net-short days > 0:", round(mean(C[, base["short"]] > 0), 2), "\n")
}
