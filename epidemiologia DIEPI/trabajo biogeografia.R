library(vegan)
library(betapart)
data(varespec)
varespec
help(varespec)
?varespec
nrow(varespec)
ncol(varespec)
va_pa <- decostand(varespec, "pa")
va_pa
# ---- Índices de disimilitud ----
d_jac <- vegdist(va_pa, "jaccard")                     # Jaccard
d_sor <- vegdist(va_pa, method="bray", binary=TRUE)    # Sørensen binario
d_bray <- vegdist(varespec, "bray")                    # Bray-Curtis (abundancias)
d_jac
d_jac
d_jac %>% view
View(d_jac)
as.data.frame(d_jac)
# ---- Índices de disimilitud ----
d_jac <- vegdist(va_pa, "jaccard")                     # Jaccard
d_sor <- vegdist(va_pa, method="bray", binary=TRUE)    # Sørensen binario
d_bray <- vegdist(varespec, "bray")                    # Bray-Curtis (abundancias)
library(tidyverse)
dist_to_long <- function(d) {
  m <- as.matrix(d)
  tibble::as_tibble(m, rownames = "i") %>%
    pivot_longer(-i, names_to = "j", values_to = "d")
}
plot_dist_heatmap <- function(d, title = NULL, reorder = TRUE) {
  m <- as.matrix(d)
  # reordenar por clustering jerárquico (útil para "ver estructura")
  if (reorder) {
    ord <- hclust(as.dist(m))$order
    m <- m[ord, ord, drop = FALSE]
  }
  df <- dist_to_long(as.dist(m))
  # dejar solo triángulo inferior (evita duplicados)
  levs <- rownames(m)
  df <- df %>%
    mutate(i = factor(i, levels = levs),
           j = factor(j, levels = levs)) %>%
    filter(as.integer(i) > as.integer(j))
  ggplot(df, aes(j, i, fill = d)) +
    geom_tile() +
    coord_equal() +
    labs(x = NULL, y = NULL, fill = "D", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}
plot_dist_heatmap(d_jac,  "Jaccard (PA)")
plot_dist_heatmap(d_sor,  "Sørensen binario (PA)")
plot_dist_heatmap(d_bray, "Bray–Curtis (abund.)")
library(tidyverse)
dist_to_long <- function(d) {
  m <- as.matrix(d)
  tibble::as_tibble(m, rownames = "i") %>%
    pivot_longer(-i, names_to = "j", values_to = "d")
}
plot_dist_heatmap <- function(d, title = NULL, reorder = TRUE) {
  m <- as.matrix(d)
  # reordenar por clustering jerárquico (útil para "ver estructura")
  if (reorder) {
    ord <- hclust(as.dist(m))$order
    m <- m[ord, ord, drop = FALSE]
  }
  df <- dist_to_long(as.dist(m))
  # dejar solo triángulo inferior (evita duplicados)
  levs <- rownames(m)
  df <- df %>%
    mutate(i = factor(i, levels = levs),
           j = factor(j, levels = levs)) %>%
    filter(as.integer(i) > as.integer(j))
  ggplot(df, aes(j, i, fill = d)) +
    geom_tile() +
    coord_equal() +
    labs(x = NULL, y = NULL, fill = "D", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    library(tidyverse)
  dist_to_long <- function(d) {
    m <- as.matrix(d)
    tibble::as_tibble(m, rownames = "i") %>%
      pivot_longer(-i, names_to = "j", values_to = "d")
  }
  plot_dist_heatmap <- function(d, title = NULL, reorder = TRUE) {
    m <- as.matrix(d)
    # reordenar por clustering jerárquico (útil para "ver estructura")
    if (reorder) {
      ord <- hclust(as.dist(m))$order
      m <- m[ord, ord, drop = FALSE]
    }
    df <- dist_to_long(as.dist(m))
    # dejar solo triángulo inferior (evita duplicados)
    levs <- rownames(m)
    df <- df %>%
      mutate(i = factor(i, levels = levs),
             j = factor(j, levels = levs)) %>%
      filter(as.integer(i) > as.integer(j))
    ggplot(df, aes(j, i, fill = d)) +
      geom_tile() +
      coord_equal() +
      labs(x = NULL, y = NULL, fill = "D", title = title) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_viridis_c(option = "C", direction = -1)
  }
  plot_dist_heatmap(d_jac,  "Jaccard (PA)")
  plot_dist_heatmap(d_sor,  "Sørensen binario (PA)")
  plot_dist_heatmap(d_bray, "Bray–Curtis (abund.)")
}
plot_dist_heatmap(d_jac,  "Jaccard (PA)")
library(tidyverse)
dist_to_long <- function(d) {
  m <- as.matrix(d)
  tibble::as_tibble(m, rownames = "i") %>%
    pivot_longer(-i, names_to = "j", values_to = "d")
}
plot_dist_heatmap <- function(d, title = NULL, reorder = TRUE) {
  m <- as.matrix(d)
  # reordenar por clustering jerárquico (útil para "ver estructura")
  if (reorder) {
    ord <- hclust(as.dist(m))$order
    m <- m[ord, ord, drop = FALSE]
  }
  df <- dist_to_long(as.dist(m))
  # dejar solo triángulo inferior (evita duplicados)
  levs <- rownames(m)
  df <- df %>%
    mutate(i = factor(i, levels = levs),
           j = factor(j, levels = levs)) %>%
    filter(as.integer(i) > as.integer(j))
  ggplot(df, aes(j, i, fill = d)) +
    geom_tile() +
    coord_equal() +
    labs(x = NULL, y = NULL, fill = "D", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_viridis_c(option = "C", direction = -1)
}
plot_dist_heatmap(d_jac,  "Jaccard (PA)")
plot_dist_heatmap(d_sor,  "Sørensen binario (PA)")
plot_dist_heatmap(d_bray, "Bray–Curtis (abund.)")
library(tidyverse)
plot_dist_heatmap <- function(
    d,
    title = NULL,
    reorder = TRUE,
    show_values = TRUE,
    digits = 2,
    label_size = 3
) {
  m <- as.matrix(d)
  # Reordenar por clustering jerárquico
  if (reorder) {
    ord <- hclust(as.dist(m))$order
    m <- m[ord, ord, drop = FALSE]
  }
  df <- tibble::as_tibble(m, rownames = "i") %>%
    pivot_longer(-i, names_to = "j", values_to = "d")
  levs <- rownames(m)
  df <- df %>%
    mutate(
      i = factor(i, levels = levs),
      j = factor(j, levels = levs),
      label = formatC(d, format = "f", digits = digits)
    ) %>%
    filter(as.integer(i) > as.integer(j))   # triángulo inferior
  p <- ggplot(df, aes(j, i, fill = d)) +
    geom_tile(color = "grey85") +
    coord_equal() +
    labs(x = NULL, y = NULL, fill = "D", title = title) +
    scale_fill_viridis_c(option = "C", direction = -1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  if (show_values) {
    p <- p + geom_text(aes(label = label), size = label_size)
  }
  p
}
scale_fill_viridis_c(option = "C", direction = -1)
plot_dist_heatmap(
  d_jac,
  title = "Jaccard (presencia–ausencia)",
  label_size = 3.5,
  digits = 2
)
plot_dist_heatmap(
  d_bray,
  title = "Bray–Curtis (abundancias)",
  label_size = 2.8,
  digits = 3
)
plot_dist_heatmap(
  d_jac,
  title = "Jaccard (presencia–ausencia)",
  label_size = 3.5,
  digits = 2
)
plot_dist_heatmap(
  d_bray,
  title = "Bray–Curtis (abundancias)",
  label_size = 2.8,
  digits = 1
)
library(tidyverse)
plot_dist_heatmap <- function(
    d,
    title = NULL,
    reorder = TRUE,
    show_values = TRUE,
    digits = 2,
    label_size = 3
) {
  m <- as.matrix(d)
  # Reordenar por clustering jerárquico
  if (reorder) {
    ord <- hclust(as.dist(m))$order
    m <- m[ord, ord, drop = FALSE]
  }
  df <- tibble::as_tibble(m, rownames = "i") %>%
    pivot_longer(-i, names_to = "j", values_to = "d")
  levs <- rownames(m)
  df <- df %>%
    mutate(
      i = factor(i, levels = levs),
      j = factor(j, levels = levs),
      label = formatC(d, format = "f", digits = digits)
    ) %>%
    filter(as.integer(i) > as.integer(j))   # triángulo inferior
  p <- ggplot(df, aes(j, i, fill = d)) +
    geom_tile(color = "grey85") +
    coord_equal() +
    labs(x = NULL, y = NULL, fill = "D", title = title) +
    scale_fill_viridis_c(option = "C", direction = -1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  if (show_values) {
    p <- p + geom_text(aes(label = label), size = label_size, fill = 'white', color = 'white')
  }
  p
}
plot_dist_heatmap(
  d_jac,
  title = "Jaccard (presencia–ausencia)",
  label_size = 3.5,
  digits = 2
)
plot_dist_heatmap(
  d_bray,
  title = "Bray–Curtis (abundancias)",
  label_size = 2.8,
  digits = 1
)
varespec[c(5,25),]
varespec
varespec %>% rownames()
varespec %>% rownames() %>% sort
varespec %>% rownames() %>% as.numeric() %>% sort
varespec[c("5", "25"),]
plot_dist_heatmap(
  d_jac,
  title = "Jaccard (presencia–ausencia)",
  label_size = 3.5,
  digits = 3
)
plot_dist_heatmap(
  d_bray,
  title = "Bray–Curtis (abundancias)",
  label_size = 2.8,
  digits = 1
)
plot_dist_heatmap(
  d_jac,
  title = "Jaccard (presencia–ausencia)",
  label_size = 3.5,
  digits = 3
)
plot_dist_heatmap(
  d_bray,
  title = "Bray–Curtis (abundancias)",
  label_size = 2.8,
  digits = 2
)
plot_dist_heatmap(
  d_jac,
  title = "Jaccard (presencia–ausencia)",
  label_size = 3.5,
  digits = 3
)
plot_dist_heatmap(
  d_bray,
  title = "Bray–Curtis (abundancias)",
  label_size = 2.8,
  digits = 1
)
plot_dist_heatmap(
  d_jac,
  title = "Jaccard (presencia–ausencia)",
  label_size = 3.5,
  digits = 3
)
plot_dist_heatmap(
  d_bray,
  title = "Bray–Curtis (abundancias)",
  label_size = 2.8,
  digits = 2
)
varespec[c("5", "25"),]
varespec[c("5", "28"),]