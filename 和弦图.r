# ============================================================
#  Jump.xlsx → Chord diagram + Directional asymmetry bars
#  固定国家配色（来自你给的表），未覆盖者 → 自动低饱和色
#  输出：chord.pdf + asymmetry_bars.pdf + flows_aggregated.csv + asymmetry_tests.csv
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(circlize)
  library(Cairo)     # 可编辑 PDF
  library(scales)    # 自动调色
})

# ---------------- 参数 ----------------
INPUT_FILE <- "Jump.xlsx"
OUT_DIR    <- "out"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

TITLE_CHORD <- "Global transmission routes (Chord diagram)"
TITLE_BAR   <- "Directional asymmetry (binomial test, BH-adjusted)"
font_family <- "Arial"

TOP_EDGES <- 0              # 显示权重前N条边；0=全部
MIN_COUNT_THRESHOLD <- 0    # 过滤掉自环和小于阈值的边
TOP_PAIRS_FOR_BARS <- 15    # 显著性柱图前K对

# ---- 固定配色（来自你给的表）----
COUNTRY_COLORS <- c(
  "USA"            = "#4E79A7",
  "United Kingdom" = "#9ACFD3",
  "China"          = "#E15759",
  "Canada"         = "#F9D29D",
  "Australia"      = "#9ACFD3",
  "Japan"          = "#BFD99A",
  "Italy"          = "#F3CFC6",
  "Ireland"        = "#AACFE5",
  "Thailand"       = "#F28E2B",
  "Denmark"        = "#FEFBBA",
  "Germany"        = "#EDC949",
  "Spain"          = "#FBE3D5",
  "South Korea"    = "#F6B293",
  "Mexico"         = "#76B7B2",
  "Viet Nam"       = "#F28E2B",
  "Switzerland"    = "#DC6D57",
  "Belgium"        = "#3888C0",
  "Colombia"       = "#68ACD5",
  "Poland"         = "#AACFE5",
  "Russia"         = "#D2E3F3",
  "Suriname"       = "#F4F9FE"
)

# ---------------- 数据准备 ----------------
raw <- read_excel(INPUT_FILE)
colnames(raw) <- str_trim(colnames(raw))
need <- c("From","To","Count")
stopifnot(all(need %in% names(raw)))

flow <- raw %>%
  mutate(Count = as.numeric(Count)) %>%
  filter(!is.na(From), !is.na(To), From != To) %>%
  group_by(From, To) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Count))

if (MIN_COUNT_THRESHOLD > 0) flow <- filter(flow, Count > MIN_COUNT_THRESHOLD)
if (TOP_EDGES > 0)          flow <- slice_head(flow, n = TOP_EDGES)

write.csv(flow, file.path(OUT_DIR, "flows_aggregated.csv"), row.names = FALSE)

# ---------------- 节点颜色 ----------------
nodes <- sort(unique(c(flow$From, flow$To)))

# 自动生成低饱和色（未覆盖的国家）
auto_pal <- function(nms) {
  pal <- hue_pal(l = 45, c = 60)(length(nms))  # 低饱和
  names(pal) <- nms
  pal
}
grid.col <- auto_pal(nodes)
overlap  <- intersect(names(COUNTRY_COLORS), nodes)
grid.col[overlap] <- COUNTRY_COLORS[overlap]

# ---------------- 弦图 ----------------
CairoPDF(file.path(OUT_DIR, "chord.pdf"), width = 10, height = 10, family = font_family)
circos.clear()
circos.par(
  gap.after    = rep(6, length(nodes)),
  track.margin = c(0.01, 0.01)
)

link_df <- flow %>% rename(from = From, to = To, value = Count)

chordDiagram(
  x = link_df,
  order = nodes,
  grid.col = grid.col,
  transparency = 0.3,
  directional = 1,
  direction.type = "arrows",
  link.arr.type = "big.arrow",
  link.sort = TRUE, link.largest.ontop = TRUE,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = 0.15)
)

# 外圈标签（大字号，英文）
circos.trackPlotRegion(
  track.index = 1, panel.fun = function(x, y) {
    sector.name <- get.cell.meta.data("sector.index")
    circos.text(
      x = get.cell.meta.data("xcenter"),
      y = get.cell.meta.data("ylim")[1] + mm_y(0.5),
      labels = sector.name,
      facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
      cex = 1.5, font = 2, col = "black"
    )
  }, bg.border = NA
)

title(TITLE_CHORD, cex.main = 1.6, line = -2, family = font_family)
circos.clear()
dev.off()

# ---------------- 不对称性分析 ----------------
pair_tab <- flow %>%
  mutate(pair = if_else(From < To, paste(From, To, sep = " | "),
                        paste(To, From, sep = " | "))) %>%
  mutate(dir  = if_else(From < To, "AB", "BA")) %>%
  select(pair, dir, Count) %>%
  pivot_wider(names_from = dir, values_from = Count, values_fill = 0)

binom_p <- function(AB, BA) {
  tot <- AB + BA
  if (tot == 0) return(1)
  stats::binom.test(AB, n = tot, p = 0.5, alternative = "two.sided")$p.value
}

asym <- pair_tab %>%
  rowwise() %>%
  mutate(total = AB + BA,
         p     = binom_p(AB, BA)) %>%
  ungroup() %>%
  mutate(FDR = p.adjust(p, method = "BH")) %>%
  arrange(FDR)

write.csv(asym, file.path(OUT_DIR, "asymmetry_tests.csv"), row.names = FALSE)

# ---------------- 柱图 ----------------
stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  "ns"
}

sub_pairs <- head(asym$pair, n = min(TOP_PAIRS_FOR_BARS, nrow(asym)))

sub_main <- asym %>%
  filter(pair %in% sub_pairs) %>%
  mutate(label = factor(pair, levels = sub_pairs)) %>%
  pivot_longer(cols = c("AB","BA"), names_to = "dir", values_to = "Count")

star_df <- asym %>%
  filter(pair %in% sub_pairs) %>%
  mutate(label = factor(pair, levels = sub_pairs),
         y = pmax(AB, BA) * 1.08,
         star = vapply(FDR, stars, character(1))) %>%
  select(label, y, star)

pbar <- ggplot(sub_main, aes(label, Count, fill = dir)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, color = "grey20") +
  geom_text(data = star_df, aes(x = label, y = y, label = star),
            inherit.aes = FALSE, size = 5, family = font_family) +
  scale_fill_manual(values = c(AB = "#5B8FF9", BA = "#F6BD16"),
                    labels = c("AB" = "A→B (alphabetical)", "BA" = "B→A")) +
  labs(x = NULL, y = "Counts", title = TITLE_BAR) +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = font_family),
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    panel.grid.minor = element_blank()
  )

ggsave(filename = file.path(OUT_DIR, "asymmetry_bars.pdf"),
       plot = pbar, width = 11, height = 5.8, device = cairo_pdf)

message("[OK] Files saved:",
        "\n - ", file.path(OUT_DIR, "chord.pdf"),
        "\n - ", file.path(OUT_DIR, "asymmetry_bars.pdf"),
        "\n - ", file.path(OUT_DIR, "flows_aggregated.csv"),
        "\n - ", file.path(OUT_DIR, "asymmetry_tests.csv"))

