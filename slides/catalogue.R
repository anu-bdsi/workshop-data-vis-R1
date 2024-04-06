library(tidyverse)
library(ozmaps)

# dummy data --------------------------------------------------------------------
set.seed(1)
n1 <- 200
df1 <- tibble(x = runif(n1, 0, 10),
              y =  3 * x + rnorm(n1, 0, 10),
              z = rnorm(n1, 0, 2),
              g1 = sample(letters[1:4], size = n1, replace = TRUE),
              g2 = sample(letters[1:4], size = n1, replace = TRUE)) %>% 
  mutate(g1 = fct_reorder(g1, y, sum))
df2 <- diamonds %>% 
  sample_n(80)
df3 <- ToothGrowth %>% 
  mutate(dosef = factor(dose)) %>% 
  group_by(dosef, supp) %>% 
  summarise(mlen = factor(mean(len)))
sumdf1 <- df1 %>% 
  group_by(g1) %>% 
  summarise(y = sum(y))
sumdf2 <- df1 %>% 
  group_by(g1, g2) %>% 
  summarise(y = sum(y))

# colors ------------------------------------------------------------------

# generate 5 from colorspace and discard the tails which are too white
reds <- c("#7F000D", "#A9565A", "#CA9496", "#E2CBCB")
yellows <- c("#6A3F00", "#97742F", "#BAA588", "#D4CCC3")
green <- "#006400"
purples <- c("#312271", "#4F4293", "#6D60BB", "#8B80D1", "#A79FE1", "#C2BCF0", 
             "#DAD6FA", "#EDEBFF", "#F9F9F9")


# theme styles ------------------------------------------------------------
theme_base <- list(theme_void(base_size = 18) + 
                     theme(plot.margin = margin(10, 10, 10, 10),
                           plot.title.position = "plot",
                           plot.title = element_text(margin = margin(t = -4, b = 10),
                                                     size = 14, face = "bold")),
                   guides(fill = "none"))

theme_rank <- c(theme_base,
                list(theme(plot.background = element_rect(fill = "#DCBFC9", 
                                                          color = NA)),
                     scale_fill_manual(values = reds)))

theme_corr <- c(theme_base,
                list(theme(plot.background = element_rect(fill = "#EDDBB6", 
                                                          color = NA))))

theme_dist <- c(theme_base,
                list(theme(plot.background = element_rect(fill = "#D7FBCD", 
                                                          color = NA))))

theme_other <- c(theme_base,
                 list(theme(plot.background = element_rect(fill = "#FFE5FF", 
                                                           color = NA)),
                      scale_fill_manual(values = purples)))

theme_border <- theme(plot.background = element_rect(color = "black",
                                                     size = 3))


theme_yaxis <- theme(axis.line.y = element_line(color = "black", size = 1),
                     axis.ticks.y = element_line(color = "black", 
                                                 linetype = "solid", 
                                                 size = 1, 
                                                 lineend = NULL),
                     axis.ticks.length.y = unit(0, "lines"))


theme_xaxis <- theme(axis.line.x = element_line(color = "black", size = 1),
                     axis.ticks.x = element_line(color = "black", 
                                                 linetype = "solid", 
                                                 size = 1, 
                                                 lineend = NULL),
                     axis.ticks.length.x = unit(0, "lines"))


# plots -------------------------------------------------------------------


ggplot(faithful, aes(eruptions)) +
  geom_histogram(fill = green, bins = 10, color = "white") +
  theme_dist +
  theme_xaxis +
  theme_yaxis +
  ggtitle("HISTOGRAM") +
  theme_border

ggsave("slides/images/catalogue-histogram.png", width = 2, height = 2)

faithful |> 
  mutate(group = cut(eruptions, 4),
         group = factor(group, labels = paste0("Group ", 1:4))) |> 
  ggplot(aes(eruptions)) +
  geom_histogram(fill = green, bins = 10, color = "white") +
  theme_dist +
  theme_xaxis +
  theme_yaxis +
  ggtitle("HISTOGRAM") +
  theme_border +
  facet_wrap(~group, scale = "free") +
  theme(strip.text = element_text(color = "black", margin = margin(t = 4, b = 4, r = 4, l = 4), face = "bold"), 
        strip.background = element_rect(color = "black", fill = "white",  linewidth = 1.4))

ggsave("slides/images/catalogue-histogram-facet.png", width = 4, height = 4)

ggplot(faithful, aes(eruptions)) +
  geom_density(fill = green, color = NA) +
  theme_dist +
  theme_xaxis +
  theme_yaxis +
  theme_border +
  ggtitle("DENSITY PLOT")

ggsave("slides/images/catalogue-density.png", width = 2, height = 2)

ggplot(faithful, aes(eruptions)) +
  geom_density(color = "white", linewidth = 4) +
  geom_density(color = "black", linewidth = 2) +
  theme_dist +
  theme_xaxis +
  theme_yaxis +
  theme_border +
  ggtitle("DENSITY PLOT")

ggsave("slides/images/catalogue-density2.png", width = 2, height = 2)


ggplot(faithful, aes(eruptions)) +
  geom_histogram(fill = green, bins = 15, color = "white",
                 aes(y = stat(density))) +
  geom_density(color = "white", linewidth = 4) +
  geom_density(color = "black", linewidth = 2) +
  theme_dist +
  theme_xaxis +
  theme_yaxis +
  theme_border +
  ggtitle("HISTOGRAM + \nDENSITY PLOT")

ggsave("slides/images/catalogue-histogram-density.png", width = 2, height = 2)


ggplot(faithful, aes(eruptions, "")) +
  geom_boxplot(fill = "white", color = green, width = 0.3) +
  theme_dist +
  theme_xaxis +
  theme_border +
  ggtitle("BOXPLOT")

ggsave("slides/images/catalogue-boxplot.png", width = 2, height = 2)


ggplot(faithful, aes(eruptions, "")) +
  geom_violin(fill = green) +
  theme_dist +
  theme_xaxis +
  theme_border +
  ggtitle("VIOLIN PLOT")

ggsave("slides/images/catalogue-violin.png", width = 2, height = 2)


ggplot(faithful, aes(eruptions)) +
  geom_dotplot(fill = green) +
  theme_dist +
  theme_xaxis +
  theme_border +
  ggtitle("DOT PLOT")

ggsave("slides/images/catalogue-dot.png", width = 2, height = 2)


ggplot(sumdf1, aes(g1, y)) +
  geom_col(fill = reds[1]) +
  theme_rank +
  theme_yaxis +
  theme_border +
  ggtitle("BARPLOT") 

ggsave("slides/images/catalogue-barplot.png", width = 2, height = 2)


ggplot(sumdf2, aes(g1, y, fill = g2)) +
  geom_col() +
  theme_rank +
  theme_yaxis +
  theme_border + 
  ggtitle("STACKED\nBARPLOT")

ggsave("slides/images/catalogue-stacked-barplot.png", width = 2, height = 2)

ggplot(sumdf2, aes(g1, y, fill = g2)) +
  geom_col(position = "fill", data = ~filter(.x, g1==sumdf2$g1[1])) +
  theme_rank +
  ggtitle("PIE CHART") + 
  coord_polar("y")

ggsave("slides/images/catalogue-piechart.png", width = 2, height = 2)


ggplot(sumdf2, aes(g1, y, fill = g2)) +
  geom_col(position = "fill") +
  theme_rank +
  ggtitle("DONUT CHART") + 
  scale_x_discrete(expand = c(0.4, 0)) +
  coord_polar("y")

ggsave("slides/images/catalogue-donutchart.png", width = 2, height = 2)


ggplot(sumdf2, aes(g1, y, fill = g2)) +
  geom_col(position = "dodge") +
  theme_rank +
  theme_yaxis +
  theme_border + 
  ggtitle("GROUPED\nBARPLOT")

ggsave("slides/images/catalogue-grouped-barplot.png", width = 2, height = 2)


ggplot(sumdf2, aes(g1, y, fill = g2)) +
  geom_col(position = "fill") +
  theme_rank +
  theme_yaxis +
  theme_border + 
  ggtitle("STACKED\nPERCENTAGE\nBARPLOT")

ggsave("slides/images/catalogue-stacked-percentage-barplot.png", width = 2, height = 2)


ggplot(df2, aes(carat, price)) +
  geom_point(color = yellows[1]) +
  theme_corr +
  theme_yaxis +
  theme_xaxis +
  theme_border + 
  ggtitle("SCATTER PLOT")

ggsave("slides/images/catalogue-scatter.png", width = 2, height = 2)


ggplot(df2, aes(carat, price)) +
  geom_hex() +
  theme_corr +
  theme_yaxis +
  theme_xaxis +
  theme_border + 
  scale_fill_gradient(high = "white", low = yellows[1]) +
  ggtitle("HEX PLOT")

ggsave("slides/images/catalogue-hex.png", width = 2, height = 2)


ggplot(df2, aes(carat, price)) +
  geom_density_2d(color = yellows[1]) +
  theme_corr +
  theme_yaxis +
  theme_xaxis +
  theme_border + 
  ggtitle("2D DENSITY PLOT")


ggsave("slides/images/catalogue-2d-density.png", width = 2, height = 2)

ggplot(economics, aes(date, uempmed)) +
  geom_line(color = yellows[1]) +
  theme_corr +
  theme_yaxis +
  theme_xaxis +
  theme_border + 
  ggtitle("LINE PLOT")

ggsave("slides/images/catalogue-line.png", width = 2, height = 2)

ggplot(df3, aes(dosef, supp, fill = mlen)) +
  geom_tile(color = "black", size = 1.2) +
  theme_other +
  theme_border + 
  ggtitle("HEATMAP")

ggsave("slides/images/catalogue-heatmap.png", width = 2, height = 2)


oz_sf <- ozmap_data("states")
oz_sf %>%
  mutate(value = factor(rnorm(n()))) %>%
  ggplot(aes(fill = value)) +
  geom_sf() +
  theme_other +
  theme_border + 
  ggtitle("CHOROPLETH\nMAP")
ggsave("slides/images/catalogue-map.png", width = 2, height = 2)
