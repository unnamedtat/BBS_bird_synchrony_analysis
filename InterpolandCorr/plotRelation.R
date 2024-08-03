source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

############################绘制变化####################################
suppressMessages(library(readr))
suppressMessages(library(tidyr))
suppressMessages(library(Kendall))

data_filtered <- readr::read_csv(file.path(workflow_window_dir,'fit.csv')) %>%
  filter(BCR==0)

data_sorted <- data_filtered %>%
  arrange(AOU, years)

plot <- data_sorted %>%
  pivot_longer(cols = c(intercept, slope), names_to = "parameter", values_to = "value") %>%
  ggplot(aes(x = years, y = value, color = parameter, group = parameter)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AOU, scales = "free_y") +
  labs(title = "Intercept and Slope Changes Over Years by AOU",
       x = "Years",
       y = "Correlation") +
  theme_minimal() +
  theme(legend.title = element_blank())
print(plot)
ggsave(file.path(picure_dir,"AOU_changes_plot.png"), plot, width = 12, height = 8, dpi = 300)

###############################趋势检验#######################################
mk_results <- data_sorted %>%
  group_by(AOU) %>%
  summarise(
    intercept_trend = list(MannKendall(intercept)),
    slope_trend = list(MannKendall(slope))
  ) %>%
  mutate(
    intercept_p_value = map_dbl(intercept_trend, ~.$sl),
    intercept_tau = map_dbl(intercept_trend, ~.$tau),
    slope_p_value = map_dbl(slope_trend, ~.$sl),
    slope_tau = map_dbl(slope_trend, ~.$tau)
  ) %>%
  select(-intercept_trend, -slope_trend)

print(mk_results)

write_csv(mk_results, file.path(workflow_window_dir,'mann_kendall_results.csv'))


###########################检验相关性############################
suppressMessages(library(Hmisc))
suppressMessages(library(ggplot2))

intercept_wide <- data_filtered %>%
  select(AOU, years, intercept) %>%
  tidyr::pivot_wider(names_from = AOU, values_from = intercept)

slope_wide <- data_filtered %>%
  select(AOU, years, slope) %>%
  tidyr::pivot_wider(names_from = AOU, values_from = slope)

# 移除years列并转换为矩阵
intercept_matrix <- intercept_wide %>% select(-years) %>% as.matrix()
slope_matrix <- slope_wide %>% select(-years) %>% as.matrix()

# 计算相关性矩阵
intercept_cor <- cor(intercept_matrix, use = "pairwise.complete.obs")
slope_cor <- cor(slope_matrix, use = "pairwise.complete.obs")

create_heatmap <- function(cor_matrix, title) {
  cor_df <- as.data.frame(cor_matrix) %>%
    mutate(AOU1 = rownames(.))

  cor_long <- cor_df %>%
    pivot_longer(cols = -AOU1, names_to = "AOU2", values_to = "correlation")
  ggplot(data = cor_long, aes(x = AOU1, y = AOU2, fill = correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.title = element_blank()) +
    labs(title = title, fill = "Correlation")
}

# 创建并保存热图
intercept_heatmap <- create_heatmap(intercept_cor, "Intercept Correlation Heatmap between AOUs")
slope_heatmap <- create_heatmap(slope_cor, "Slope Correlation Heatmap between AOUs")

print(intercept_heatmap)
print(slope_heatmap)

ggsave(file.path(picure_dir,"intercept_correlation_heatmap.png"), intercept_heatmap, width = 12, height = 10, dpi = 300)
ggsave(file.path(picure_dir,"slope_correlation_heatmap.png"), slope_heatmap, width = 12, height = 10, dpi = 300)

write.csv(intercept_cor, file.path(workflow_window_dir,"intercept_correlations.csv"))
write.csv(slope_cor, file.path(workflow_window_dir,"slope_correlations.csv"))