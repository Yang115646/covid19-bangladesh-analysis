# ============================
# 1. 加载包
# ============================
library(tidyverse)
library(ggplot2)
library(ggpmisc)

# ============================
# 2. 数据导入与清洗
# ============================
data_ydy <- read_csv("D:/R_zuoyedewenjian/COVID-19 BD Dataset-5 May.csv")
data_ydy <- data_ydy[-60, ]
data_ydy$Date <- as.Date(data_ydy$Date, format = "%m/%d/%Y")

# ============================
# 3. 转换为长格式（所有指标在一列）
# ============================
data_ydy_long <- data_ydy %>%
  select(
    Date,
    `Daily new confirmed cases`,
    `Daily new deaths`,
    `Daily new recovered`,
    `Active Cases`,
    `Daily New Tests`,
    `Total confirmed cases`,
    `Total deaths`,
    `Total recovered`,
    `Total Tests`
  ) %>%
  pivot_longer(
    cols = -Date,
    names_to = "indicator",
    values_to = "value"
  )

# ============================
# 4. 中文标签映射表
# ============================
indicator_cn <- c(
  "Active Cases" = "现存病例",
  "Daily new confirmed cases" = "每日新增确诊",
  "Daily new deaths" = "每日新增死亡",
  "Daily new recovered" = "每日新增治愈",
  "Daily New Tests" = "每日新增检测数",
  "Total confirmed cases" = "累计确诊病例",
  "Total deaths" = "累计死亡病例",
  "Total recovered" = "累计治愈病例",
  "Total Tests" = "累计检测数"
)

# ============================
# 5. ★★★ 分面图（解决量级差异） ★★★
# ============================
# 这是取代之前 p_beautiful 的图
ggplot(data_ydy_long, aes(x = Date, y = value, color = indicator)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.6, alpha = 0.5) +
  
  # 分面：每个指标独立子图，Y轴自由缩放
  facet_wrap(
    ~ indicator,
    scales = "free_y",          # 关键：Y轴独立缩放
    ncol = 3,                   # 每行3个子图
    labeller = labeller(indicator = indicator_cn)
  ) +
  
  # 颜色（配了9种颜色，每个指标一种）
  scale_color_manual(
    values = c(
      "Active Cases" = "pink",
      "Daily new confirmed cases" = "red",
      "Daily new deaths" = "blue",
      "Daily new recovered" = "green",
      "Daily New Tests" = "orange",
      "Total confirmed cases" = "#E69F00",
      "Total deaths" = "purple",
      "Total recovered" = "darkgreen",
      "Total Tests" = "cyan"
    ),
    labels = indicator_cn
  ) +
  
  labs(
    title = "COVID-19 各指标趋势（分面图）",
    subtitle = "每个子图Y轴独立缩放，解决量级差异问题",
    x = "日期",
    y = "人数",
    color = "指标类型"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ============================
# 问题2：累计确诊/死亡/治愈的增长曲线（分面版，不堆叠）
data_total <- data_ydy_long %>%
  filter(indicator %in% c("Total confirmed cases", "Total deaths", "Total recovered"))

ggplot(data_total, aes(x = Date, y = value, fill = indicator)) +
  geom_area(alpha = 0.6, position = "identity") +  # 关键改动：不堆叠
  facet_wrap(~ indicator, scales = "free_y", ncol = 1,
             labeller = labeller(indicator = c(
               "Total confirmed cases" = "累计确诊",
               "Total deaths" = "累计死亡",
               "Total recovered" = "累计治愈"
             ))) +
  scale_fill_manual(
    values = c("orange", "purple", "lightgreen"),
    labels = c("累计确诊", "累计死亡", "累计治愈")
  ) +
  labs(
    title = "累计指标增长趋势（分面图）",
    subtitle = "各指标Y轴独立缩放，清晰观察增长曲线",
    x = "日期",
    y = "人数",
    fill = "指标类型"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )
# 8. 问题3：新增确诊与新增死亡相关性
# ============================
data_corr <- data_ydy_long %>%
  filter(indicator %in% c("Daily new confirmed cases", "Daily new deaths")) %>%
  pivot_wider(names_from = indicator, values_from = value)

ggplot(data_corr, aes(x = `Daily new confirmed cases`, y = `Daily new deaths`)) +
  geom_point(alpha = 0.7, color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "每日新增确诊与死亡的相关性",
    x = "每日新增确诊数",
    y = "每日新增死亡数"
  ) +
  theme_minimal()


# ============================
# 问题4：疫情爆发初期与后期的每日新增确诊数差异（分面箱线图）
data_box <- data_ydy_long %>%
  filter(indicator == "Daily new confirmed cases") %>%
  mutate(period = ifelse(Date < as.Date("2020-04-06"), "爆发初期", "疫情后期"))

ggplot(data_box, aes(x = period, y = value, fill = period)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  scale_fill_manual(values = c("red", "lightgreen")) +
  facet_wrap(~ period, ncol = 1, scales = "free_y") +  # 分面显示
  labs(
    title = "疫情不同阶段新增确诊数分布",
    subtitle = "爆发初期 vs 疫情后期",
    x = "时段",
    y = "每日新增确诊数"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(size = 11, face = "italic"),
    legend.position = "none"
  )
# 10. 问题5：死亡占比趋势
# ============================
data_ratio <- data_ydy_long %>%
  filter(indicator %in% c("Total confirmed cases", "Total deaths")) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate(death_ratio = (`Total deaths` / `Total confirmed cases`) * 100)

ggplot(data_ratio, aes(x = Date, y = death_ratio)) +
  geom_line(color = "purple", linewidth = 0.8) +
  geom_hline(yintercept = mean(data_ratio$death_ratio), color = "red", linetype = "dashed") +
  labs(
    title = "累计死亡占累计确诊的比例变化",
    x = "日期",
    y = "死亡占比（%）"
  ) +
  theme_minimal()