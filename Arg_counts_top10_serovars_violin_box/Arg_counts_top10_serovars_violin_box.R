# Libraries
library(ggplot2)
library(ggsignif)
library(readxl)
library(gghalves)
library(dplyr)
library(ggrastr)
library(Cairo)

# 1) Load data
df <- read_excel("Counts.xlsx")
df$Counts <- as.numeric(df$Counts)

# 2) Mean ARG counts per serovar and ordering
mean_values <- df %>%
  group_by(Serovar) %>%
  summarise(Mean_Counts = mean(Counts, na.rm = TRUE), .groups = "drop") %>%
  arrange(Mean_Counts)

mean_values$Serovar <- factor(mean_values$Serovar, levels = mean_values$Serovar)
df$Serovar          <- factor(df$Serovar, levels = levels(mean_values$Serovar))

# 3) Colors
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

# 4) Significance comparisons: last (highest mean) vs previous three
lv <- levels(df$Serovar)
k  <- min(3, length(lv) - 1)
ref <- lv[length(lv)]
others <- lv[(length(lv) - 1):(length(lv) - k)]
my_comparisons <- lapply(others, function(x) c(ref, x))

# 5) Plot (rasterizing only the jitter layer)
p <- ggplot(df, aes(x = Serovar, y = Counts)) +
  geom_half_violin(aes(fill = Serovar),
                   side = "r", scale = "width",
                   position = position_nudge(x = 0.2),
                   alpha = 0.8, color = "black", size = 0.2) +
  ggrastr::geom_jitter_rast(aes(color = Serovar),
                            size = 1.0,
                            position = position_jitter(width = 0.1),
                            alpha = 0.5, raster.dpi = 300) +
  geom_boxplot(aes(fill = Serovar),
               width = 0.25,
               position = position_nudge(x = 0.15),
               outlier.shape = NA, color = "black", size = 0.2) +
  geom_signif(comparisons = my_comparisons,
              step_increase = 0.1,
              y_position = seq(26, 18, length.out = k),
              map_signif_level = TRUE,
              textsize = 4.5,
              tip_length = 0.01,
              vjust = 0.5) +
  geom_text(data = mean_values,
            aes(x = Serovar, y = Mean_Counts,
                label = sprintf("%.1f", Mean_Counts)),
            vjust = 1.0, size = 3.5, fontface = "bold", color = "black") +
  scale_fill_manual(values = my_color, name = "ST",
                    guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = my_color, guide = "none") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, by = 5),
                     expand = c(0, 0)) +
  theme_classic(base_size = 16) +
  labs(x = NULL,
       y = "Statistical analysis of Salmonella ARGs in the top 10 serotypes") +
  theme(
    plot.title  = element_text(size = 12, face = "bold"),
    legend.title= element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key  = element_rect(fill = NA, color = NA),
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.text    = element_text(size = 14, color = "black"),
    axis.text.x  = element_text(face = "bold"),
    axis.line    = element_line(size = 0.5, color = "black")
  )

print(p)

# 6) Export (Cairo PDF keeps vector text and embeds raster points)
ggsave("serovar_ARGs_violin_box_raster_points.pdf",
       plot = p,
       device = cairo_pdf,
       width = 9, height = 6, units = "in")

