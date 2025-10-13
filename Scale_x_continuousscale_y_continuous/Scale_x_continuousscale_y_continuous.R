# Libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(ggforce)
library(ggrastr)
library(Cairo)

# 1) Load data
df <- read_excel("PCA_top10.xlsx", sheet = "PCA_top10")

# 2) Build feature matrix (drop IDs) and remove zero-variance columns
arg_data <- df %>% select(-SampleID, -GroupID)
nzv <- sapply(arg_data, function(x) var(x, na.rm = TRUE))
arg_data <- arg_data[, nzv > 0 & !is.na(nzv), drop = FALSE]

# 3) Scale features and run PCA
arg_data_scaled <- scale(arg_data)
pca_result <- prcomp(arg_data_scaled)

# 4) Scores + grouping
pca_data <- as.data.frame(pca_result$x)
pca_data$GroupID <- df$GroupID

# 5) Colors (10 serovars). Unmapped groups fall back to gray.
my_color <- c(
  "Anatum"        = "#F28E2B",
  "Derby"         = "#EDC949",
  "4,[5],12:i:-"  = "#E15759",
  "Infantis"      = "#76B7B2",
  "Johannesburg"  = "#4E79A7",
  "Typhimurium"   = "#F28D8D",
  "London"        = "#9ACFD3",
  "Uganda"        = "#BFD99A",
  "Eko"           = "#F9D29D",
  "Adelaide"      = "#F3CFC6"
)
present_groups <- sort(unique(pca_data$GroupID))
if (any(!present_groups %in% names(my_color))) {
  extra <- setNames(rep("#CCCCCC", sum(!present_groups %in% names(my_color))),
                    present_groups[!present_groups %in% names(my_color)])
  my_color <- c(my_color, extra)
}

# 6) Percent variance explained
pve <- summary(pca_result)$importance[2, 1:2] * 100

# 7) Plot (rasterize only the point layer)
pca_plot <- ggplot(pca_data, aes(PC1, PC2, color = GroupID, fill = GroupID)) +
  geom_vline(xintercept = 0, color = "#708090", linetype = 5) +
  geom_hline(yintercept = 0, color = "#708090", linetype = 5) +
  ggforce::geom_mark_ellipse(aes(PC1, PC2, colour = GroupID),
                             expand = unit(5, "mm"),
                             alpha = 0.25, n = 100, linewidth = 0.5) +
  ggforce::geom_mark_ellipse(data = subset(pca_data, GroupID == "4,[5],12:i:-"),
                             aes(PC1, PC2, colour = GroupID),
                             expand = unit(5, "mm"),
                             alpha = 0.05, n = 100, linewidth = 1.0) +
  ggrastr::geom_point_rast(shape = 21, size = 2, aes(fill = GroupID),
                           alpha = 0.5, raster.dpi = 300) +
  scale_fill_manual(values = my_color) +
  scale_color_manual(values = my_color) +
  scale_x_continuous(limits = c(-10, 40), breaks = seq(-10, 40, 10)) +
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, 10)) +
  xlab(sprintf("PC1 (%.2f%%)", pve[1])) +
  ylab(sprintf("PC2 (%.2f%%)", pve[2])) +
  labs(fill = "Group", color = "Group",
       title = "PCA of ARGs in top 10 Salmonella serotypes") +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 20),
    panel.border = element_rect(color = "#000000", linewidth = 1),
    legend.background = element_rect(color = "#FFFFFF", linetype = 1),
    legend.text  = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.title   = element_text(size = 15),
    axis.text    = element_text(size = 15)
  )

print(pca_plot)

# 8) Export (Cairo PDF keeps vector text and embeds raster points)
ggsave("pca_plot_with_ellipse_bolded.pdf",
       plot = pca_plot,
       device = cairo_pdf,
       width = 10, height = 8, units = "in")



