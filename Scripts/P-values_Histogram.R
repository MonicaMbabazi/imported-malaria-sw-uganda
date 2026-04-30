library(tidyverse)
library(igraph)
library(ggraph)
library(lubridate)
library(tidygraph)

rm(list=ls())

## Read in the files
maziba<- read.csv("Results/Maziba_p-values.csv", header = T) %>%
  dplyr::mutate(Site = "Maziba")

muko<- read.csv("Results/Muko_p-values.csv", header = T) %>%
  dplyr::mutate(Site = "Muko")

kamwezi<- read.csv("Results/Kamwezi_p-values.csv", header = T) %>%
  dplyr::mutate(Site = "Kamwezi")
## Bind
all_sites <- dplyr::bind_rows(maziba, muko, kamwezi)

sig_pairs <- all_sites %>%
  filter(sig_0.05_BH == 1, ibd > 0.125)

min_r_by_site <- sig_pairs %>%
  group_by(Site) %>%
  summarise(min_ibd = min(ibd, na.rm = TRUE))

## Plot
p <- ggplot(all_sites, aes(x = ibd, fill = Site)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  
  # Facet by site
  facet_wrap(~Site, scales = "free_y") +
  
  # Vertical line for threshold (r = 0.125)
  geom_vline(xintercept = 0.125, linetype = "dashed", color = "black") +
  
  # Vertical line for minimum significant r
  geom_vline(data = min_r_by_site,
             aes(xintercept = min_ibd),
             color = "red",
             linewidth = 1) +
  
  theme_minimal() +
  labs(
    x = "IBD (r)",
    y = "Count",
    title = "Distribution of Pairwise Relatedness (IBD) by Site"
  )
p
ggsave("Figures/Fig_S3.png", p, width = 10, height = 8, units = "in", bg = "white")
##--------------------------------------------------------------
line_df <- min_r_by_site %>%
  mutate(type = "Minimum significant r") %>%
  rename(xintercept = min_ibd) %>%
  bind_rows(
    data.frame(
      Site = unique(all_sites$Site),
      xintercept = 0.125,
      type = "Threshold (r = 0.125)"
    )
  )

##
p1 <- ggplot(all_sites, aes(x = ibd)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.6) +
  
  facet_wrap(~Site, scales = "free_y") +
  
  # Add vertical lines with legend
  geom_vline(
    data = line_df,
    aes(xintercept = xintercept, linetype = type, color = type),
    linewidth = 1
  ) +
  
  scale_linetype_manual(
    values = c(
      "Threshold (r = 0.125)" = "dashed",
      "Minimum significant r" = "solid"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Threshold (r = 0.125)" = "black",
      "Minimum significant r" = "red"
    )
  ) +
  
  theme_minimal() +
  labs(
    x = "IBD (r)",
    y = "Count",
    linetype = "Reference lines",
    color = "Reference lines"
  )
p1
ggsave("Figures/Fig_S3.png1.png", p1, width = 10, height = 8, units = "in", bg = "white")
##-------------------------------------------------------------

p2 <- ggplot(all_sites, aes(x = ibd)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.6) +
  
  facet_grid(rows = vars(Site), scales = "free_y") +
  
  geom_vline(
    data = line_df,
    aes(xintercept = xintercept, linetype = type, color = type),
    linewidth = 1
  ) +
  
  geom_text(
    data = min_r_by_site,
    aes(x = min_ibd, y = Inf, label = round(min_ibd, 3)),
    vjust = 1.5,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  scale_linetype_manual(
    values = c(
      "Threshold (r = 0.125)" = "dashed",
      "Minimum significant r" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      "Threshold (r = 0.125)" = "black",
      "Minimum significant r" = "red"
    )
  ) +
  theme_minimal() +
  labs(
    x = "IBD (r)",
    y = "Count",
    linetype = "Reference lines",
    color = "Reference lines"
  )
p2 <- p2 +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
p2

ggsave("Figures/Fig_S3.png2.png", p2, width = 10, height = 8, units = "in", bg = "white")
