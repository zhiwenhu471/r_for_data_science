# 图一安装和加载必要的包
required_packages <- c("ggplot2", "gganimate", "dplyr", "tidyr", "showtext", "gifski", "av")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# 加载字体
font_add_google("Dancing Script", "cursive_font")  
showtext_auto()


current_style_id <- 2  # 【在此切换风格 1-6】

#这里我以style 2 为例

styles <- list(
  "1" = list(  # 经典金色
    bg_col = "#0f0a19",  # 深蓝紫色夜空
    tree_cols = c("#0B3D0B", "#144514", "#006400", "#228B22"),
    trunk_col = "#3e2723",
    decor_cols = c("#FFD700", "#FFA500", "#FF8C00", "#FF4500"),
    star_col = "#FFD700",
    text_col = "#FFD700",
    snow_col = "white",
    ribbon = TRUE,
    ribbon_col = "#D4AF37",
    ribbon_width = 1.5,
    light_cols = c("#FFD700", "#FFA500", "#FF8C00")
  ),
  "2" = list(  # 银白冰雪
    bg_col = "#0a1220",  # 深蓝背景
    tree_cols = c("#2F4F4F", "#5F9EA0", "#708090", "#87CEEB"),
    trunk_col = "#696969",
    decor_cols = c("#B0C4DE", "#E6E6FA", "#F0F8FF", "#F8F8FF"),
    star_col = "#B0C4DE",
    text_col = "#B0C4DE",
    snow_col = "#E0FFFF",
    ribbon = TRUE,
    ribbon_col = "#C0C0C0",
    ribbon_width = 1.5,
    light_cols = c("#B0C4DE", "#E6E6FA", "#F0F8FF")
  ),
  "3" = list(  # 暖黄复古
    bg_col = "#1a0f0a",  # 深棕背景
    tree_cols = c("#556B2F", "#6B8E23", "#8B7355", "#A0522D"),
    trunk_col = "#5D4037",
    decor_cols = c("#CD853F", "#DAA520", "#B8860B", "#8B4513"),
    star_col = "#FFCC00",
    text_col = "#DEB887",
    snow_col = "#FAF0E6",
    ribbon = FALSE,
    ribbon_col = NA,
    ribbon_width = 0,
    light_cols = c("#CD853F", "#DAA520", "#B8860B")
  ),
  "4" = list(  # 多彩霓虹
    bg_col = "#050814",  # 近黑色背景
    tree_cols = c("#006400", "#228B22", "#32CD32", "#7CFC00"),
    trunk_col = "#4E342E",
    decor_cols = c("#FF0000", "#00FF00", "#0000FF", "#FF00FF", "#FFFF00"),
    star_col = "#FFD700",
    text_col = "#FF6347",
    snow_col = "white",
    ribbon = TRUE,
    ribbon_col = "#9370DB",  # 紫色丝带
    ribbon_width = 1.0,
    light_cols = c("#FF0000", "#00FF00", "#0000FF", "#FF00FF")
  ),
  "5" = list(  # 极简黑白
    bg_col = "#000000",
    tree_cols = c("#111111", "#222222", "#333333", "#444444"),
    trunk_col = "#222222",
    decor_cols = c("#FFFFFF", "#F0F0F0", "#E0E0E0", "#D0D0D0"),
    star_col = "#FFFFFF",
    text_col = "#FFFFFF",
    snow_col = "#888888",
    ribbon = TRUE,
    ribbon_col = "#FFFFFF",
    ribbon_width = 0.8,
    light_cols = c("#FFFFFF", "#F0F0F0", "#E0E0E0")
  ),
  "6" = list(  
    bg_col = "#1F0F12",  
    tree_cols = c("#D87093", "#FF69B4", "#FFB6C1", "#FFC0CB"),
    trunk_col = "#4A3728",
    decor_cols = c("#FFFFFF", "#FFD700", "#FF1493", "#FFB6C1"),
    star_col = "#FFD700",
    text_col = "#FFC0CB",
    snow_col = "#FFF0F5",
    ribbon = TRUE,
    ribbon_col = "#F8F8FF",
    ribbon_width = 0.8,
    light_cols = c("#FFD700", "#FF1493", "#FFB6C1")
  )
)

cfg <- styles[[as.character(current_style_id)]]


set.seed(2025)  

# 生成星星
get_star_polygon <- function(x_center, y_center, radius) {
  angles <- seq(pi/2, 2.5 * pi, length.out = 11)[-11]
  radii <- rep(c(radius, radius * 0.4), 5)
  data.frame(
    x = x_center + radii * cos(angles),
    y = y_center + radii * sin(angles)
  )
}

# 生成树
generate_tree_data <- function(cfg) {
  n_leaves <- 12000
  h <- runif(n_leaves, 0, 1)
  base_r <- (1 - h)
  layer_cycle <- (h * 8) %% 1
  r <- base_r * 0.6 * (0.5 + 0.5 * (1 - layer_cycle)^0.5)
  theta <- runif(n_leaves, 0, 2 * pi)
  
  df_tree <- data.frame(
    x = r * cos(theta),
    y = h - 0.5,
    z = r * sin(theta),
    col = sample(cfg$tree_cols, n_leaves, replace = TRUE, 
                 prob = c(0.4, 0.3, 0.2, 0.1)),  
    size = runif(n_leaves, 0.5, 1.5),
    type = "tree",
    alpha = 0.9 + 0.1 * (1 - h)  
  )
  
  # 生成树干
  n_trunk <- 1500
  h_trunk <- runif(n_trunk, -0.7, -0.45)
  r_trunk <- 0.1
  theta_trunk <- runif(n_trunk, 0, 2 * pi)
  
  df_trunk <- data.frame(
    x = r_trunk * cos(theta_trunk),
    y = h_trunk,
    z = r_trunk * sin(theta_trunk),
    col = cfg$trunk_col,
    size = 1.0,
    type = "trunk",
    alpha = 1
  )
  
  # 增加装饰
  n_decor <- 800
  h_dec <- runif(n_decor, 0.1, 0.95)
  base_r_dec <- (1 - h_dec)
  r_dec <- base_r_dec * 0.58 * (0.6 + 0.4 * runif(n_decor))
  theta_dec <- runif(n_decor, 0, 2 * pi)
  
  df_decor <- data.frame(
    x = r_dec * cos(theta_dec),
    y = h_dec - 0.5,
    z = r_dec * sin(theta_dec),
    col = sample(cfg$decor_cols, n_decor, replace = TRUE),
    size = runif(n_decor, 1.8, 3.5),
    type = "decor",
    alpha = 1
  )
  
  # 在树上增加丝带
  df_ribbon <- NULL
  if (cfg$ribbon) {
    n_rib <- 4000
    h_rib <- seq(0.1, 0.95, length.out = n_rib)
    base_r_rib <- (1 - h_rib) * 0.58
    theta_rib <- 8 * pi * h_rib  # 螺旋角度
    
    df_ribbon <- data.frame(
      x = base_r_rib * cos(theta_rib),
      y = h_rib - 0.5,
      z = base_r_rib * sin(theta_rib),
      col = cfg$ribbon_col,
      size = cfg$ribbon_width,
      type = "ribbon",
      alpha = 0.7 + 0.3 * sin(10 * h_rib)  # 波动透明度
    )
  }
  
  # 小灯串
  n_lights <- 400
  h_lights <- runif(n_lights, 0.15, 0.9)
  base_r_lights <- (1 - h_lights) * 0.62
  r_lights <- base_r_lights * (0.7 + 0.3 * runif(n_lights))
  theta_lights <- runif(n_lights, 0, 2 * pi)
  
  df_lights <- data.frame(
    x = r_lights * cos(theta_lights),
    y = h_lights - 0.5,
    z = r_lights * sin(theta_lights),
    col = sample(cfg$light_cols, n_lights, replace = TRUE),
    size = runif(n_lights, 2.0, 3.0),
    type = "light",
    alpha = 0.9 + 0.1 * sin(5 * h_lights)  # 闪烁效果
  )
  
  # 合并所有数据
  bind_rows(df_trunk, df_tree, df_decor, df_ribbon, df_lights)
}

# 生成雪花数据
generate_snow <- function(n_flakes = 400) {
  data.frame(
    x = runif(n_flakes, -1.2, 1.2),
    y = runif(n_flakes, -0.8, 1.2),
    z = runif(n_flakes, -1.2, 1.2),
    col = cfg$snow_col,
    size = runif(n_flakes, 0.3, 1.8),
    type = "snow",
    alpha = runif(n_flakes, 0.6, 0.95),
    speed = runif(n_flakes, 0.01, 0.03)
  )
}


static_data <- generate_tree_data(cfg)
snow_data <- generate_snow(400)
star_shape <- get_star_polygon(0, 0.4, 0.035)

# 设置动画参数
n_frames <- 120  # 增加帧数使动画更平滑
fps_val <- 30    # 提高帧率

# 处理每一帧
process_frame <- function(frame_id) {
  angle <- 2 * pi * (frame_id / n_frames)
  
  # 旋转树数据
  tree_rot <- static_data %>%
    mutate(
      x_rot = x * cos(angle) - z * sin(angle),
      z_rot = z * cos(angle) + x * sin(angle),
      y_final = y,
      # 根据深度调整透明度
      depth_factor = (z_rot + 1.5) / 3
    )
  
  # 雪花飘落（优化轨迹）
  snow_curr <- snow_data %>%
    mutate(
      y_final = -0.8 + (y - frame_id * speed - (-0.8)) %% 2.0,
      x_rot = x + 0.1 * sin(frame_id * 0.05 + y * 10),  # 轻微水平摆动
      z_rot = z,
      depth_factor = 1
    )
  
  # 合并并计算投影
  bind_rows(tree_rot, snow_curr) %>%
    mutate(
      depth = 1 / (2.8 - z_rot),  # 调整深度系数
      x_proj = x_rot * depth * 1.8,
      y_proj = y_final * depth * 1.8,
      size_vis = size * depth * 1.3,
      alpha_vis = alpha * depth_factor * 
        ifelse(type == "snow", 1, 0.7 + 0.3 * depth_factor)
    ) %>%
    arrange(depth) %>%  # 根据深度排序，模拟3D遮挡
    mutate(frame = frame_id)
}


all_frames <- lapply(1:n_frames, process_frame) %>% bind_rows()


# ========== 将文字并入帧数据，避免 labels 校验失败 ==========
all_frames <- all_frames |>
  dplyr::mutate(
    # 每帧都携带文字列，但仅首帧显示
    label_text = ifelse(frame == 1L, "Merry Christmas", ""),
    # 让文字层级略高于树体，避免被遮挡
    depth_text = 1 / (2.5 - z_rot + 0.01)
  )

# ========== 创建动画（全部图层来自同一数据框） ==========
p <- ggplot() +
  # 1) 树体、装饰、丝带、灯、雪花
  geom_point(
    data = all_frames,
    aes(x = x_proj, y = y_proj,
        color = I(col), size = I(size_vis), alpha = I(alpha_vis)),
    shape = 19
  ) +
  # 2) 顶部星星
  geom_polygon(
    data = star_shape,
    aes(x = x, y = y),
    fill = cfg$star_col,
    color = alpha("white", 0.8),
    linewidth = 0.5
  ) +
  # 3) 标题文字（数据驱动，避免 annotate）
  geom_text(
    data = all_frames,
    aes(x = 0, y = 0.65, label = label_text),
    family = if ("cursive_font" %in% sysfonts::font_families()) "cursive_font" else "serif",
    color = alpha(cfg$text_col, 0.9),
    size = 14,
    hjust = 0.5, vjust = 0.5,
    show.legend = FALSE
  ) +
  # 4) 固定坐标与主题
  coord_fixed(xlim = c(-0.75, 0.75), ylim = c(-0.75, 0.85)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = cfg$bg_col, color = NA),
    panel.background = element_rect(fill = cfg$bg_col, color = NA),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  # 5) 动画过渡
  transition_manual(frame)

# ========== 渲染与保存 ==========
animation <- animate(
  p,
  nframes = n_frames,
  fps = fps_val,
  width = 800,
  height = 1000,
  renderer = gifski_renderer(loop = TRUE)
)

anim_save("christmas_tree_enhanced.gif", animation)
print(animation)  # RStudio 预览