# Load packages ----

library(tidyverse)
library(janitor)
library(googlesheets4)
library(likert)
library(hrbrthemes)
library(scales)

# load fonts ----
library(sysfonts)
library(showtext)
font_add_google("Roboto Condensed")
font_add_google("IBM Plex Sans", family = "IBMPlexSans")
showtext_auto()
showtext_opts(dpi = 300)

# Fetch dataset ----

master <-
  read_csv("data/surveilans_02_masterdata_new.csv")

metadata <- 
  read_csv("data/surveilans_01_metadata_new.csv") %>% 
  dplyr::filter(is.na(dimensi) == FALSE)

# Section 3 ----

section3 <- 
  master %>% 
  select(starts_with("k03")) %>% 
  janitor::remove_empty() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "code",
    values_to = "level"
  ) %>% 
  group_by(code, level) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(
    level = factor(
      level,
      levels = c(
        "Sangat tidak setuju",
        "Tidak setuju",
        "Netral",
        "Setuju",
        "Sangat Setuju"
      )
    ),
    type = case_when(
      level == "Netral" ~ "mid",
      level %in% c("Sangat tidak setuju",
                   "Tidak setuju") ~ "low",
      level %in% c("Setuju",
                   "Sangat Setuju") ~ "high"
    ),
    type = factor(type, levels = c("low", "mid", "high"))
  ) %>% 
  left_join(
    metadata %>% 
      select(code = label_indikator, item = nama_indikator_ringkas)
  )

section3_plot <- 
  section3 %>%
  mutate(code = str_wrap(item, 30)) %>% 
  ggplot(aes(pct, code, fill = level)) +
  geom_col(
    data = ~ .x %>%
      filter(type %in% c("mid", "high")) %>% 
      mutate(
        pct = if_else(
          type == "mid",
          pct / 2,
          pct
        ),
        level = fct_rev(level),
        code = fct_reorder(code, pct, sum)
      ),
    width = 0.8
  ) +
  geom_col(
    data = ~ .x %>%
      filter(type %in% c("low", "mid")) %>% 
      mutate(
        pct = if_else(
          type == "mid",
          -1 * pct / 2,
          -1 * pct
        )
      ),
    width = 0.8
  ) +
  geom_text(
    data = ~.x %>% 
      group_by(code, type) %>% 
      summarise(
        n = sum(n),
        pct_label = percent(sum(pct), accuracy = 1)
      ) %>% 
      ungroup() %>% 
      filter(!is.na(type)) %>% 
      mutate(
        pct = case_when(
          type == "low" ~ - 1.05,
          type == "mid" ~ 0,
          type == "high" ~ 1.05
        ),
        hjust = case_when(
          type == "low" ~ - 0,
          type == "mid" ~ 0.5,
          type == "high" ~ 1
        )
      ),
    aes(pct, code, label = pct_label, hjust = hjust),
    inherit.aes = FALSE,
    family = font_ps,
    size = 2
  ) +
  annotate(
    geom = "text",
    x = c(-1.05, 1.05),
    y = Inf,
    label = c("% total\ntidak sependapat", "% total\nsependapat"),
    family = font_ps,
    fontface = "italic",
    size = 1.75,
    colour = "gray40",
    hjust = c(0, 1),
    vjust = 0
  ) +
  scale_x_continuous(labels = function(x) percent(abs(x))) +
  scale_fill_manual(
    values = c(
      "Sangat tidak setuju" = "#4C9294",
      "Tidak setuju" = "#69B0B1",
      "Netral" = "#F4D562",
      "Setuju" = "#D78DB3",
      "Sangat Setuju" = "#A63A72"
    ),
    breaks = c(
      "Sangat tidak setuju",
      "Tidak setuju",
      "Netral",
      "Setuju",
      "Sangat Setuju"
    ),
    drop = FALSE
  ) +
#   scale_fill_brewer(
#     palette = "Spectral",
#     direction = -1,
#     breaks = c(
#       "Sangat tidak setuju",
#       "Tidak setuju",
#       "Netral",
#       "Setuju",
#       "Sangat Setuju"
#     ),
#     drop = FALSE,
#     guide = guide_legend(override.aes = list(colour = "gray40", size = 0.09))
#   ) +
  labs(
    title = "Kendala dan Hambatan Teknis",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_ipsum_rc(
    grid = FALSE,
    ticks = FALSE
  ) +
  theme(
    plot.background = element_rect(fill = "#F4F2EA", colour = NA),
    panel.background = element_rect(fill = "#F4F2EA", colour = NA),
    strip.text = element_text(hjust = 0.5, face = "italic"),
    panel.spacing = unit(0.5, "lines"),
    plot.title = element_text(size = 16,
                              margin = margin(0, 0, 20, 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = rel(0.5)),
    legend.text = element_text(
      size = 6),
    legend.key.width = unit(1, "lines"),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = "outfile/section3.png",
  plot = section3_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# Section 4 ----

section4 <- 
  master %>% 
  select(starts_with("k04")) %>% 
  janitor::remove_empty() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "code",
    values_to = "level"
  ) %>% 
  group_by(code, level) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(
    level = factor(
      level,
      levels = c(
        "Sangat tidak setuju",
        "Tidak setuju",
        "Netral",
        "Setuju",
        "Sangat Setuju"
      )
    ),
    type = case_when(
      level == "Netral" ~ "mid",
      level %in% c("Sangat tidak setuju",
                   "Tidak setuju") ~ "low",
      level %in% c("Setuju",
                   "Sangat Setuju") ~ "high"
    ),
    type = factor(type, levels = c("low", "mid", "high"))
  ) %>% 
  left_join(
    metadata %>% 
      select(code = label_indikator, item = nama_indikator_ringkas)
  )

section4_plot <- 
  section4 %>%
  mutate(code = str_wrap(item, 30)) %>% 
  ggplot(aes(pct, code, fill = level)) +
  geom_col(
    data = ~ .x %>%
      filter(type %in% c("mid", "high")) %>% 
      mutate(
        pct = if_else(
          type == "mid",
          pct / 2,
          pct
        ),
        level = fct_rev(level),
        code = fct_reorder(code, pct, sum)
      ),
    width = 0.8
  ) +
  geom_col(
    data = ~ .x %>%
      filter(type %in% c("low", "mid")) %>% 
      mutate(
        pct = if_else(
          type == "mid",
          -1 * pct / 2,
          -1 * pct
        )
      ),
    width = 0.8
  ) +
  geom_text(
    data = ~.x %>% 
      group_by(code, type) %>% 
      summarise(
        n = sum(n),
        pct_label = percent(sum(pct), accuracy = 1)
      ) %>% 
      ungroup() %>% 
      filter(!is.na(type)) %>% 
      mutate(
        pct = case_when(
          type == "low" ~ - 1.05,
          type == "mid" ~ 0,
          type == "high" ~ 1.05
        ),
        hjust = case_when(
          type == "low" ~ - 0,
          type == "mid" ~ 0.5,
          type == "high" ~ 1
        )
      ),
    aes(pct, code, label = pct_label, hjust = hjust),
    inherit.aes = FALSE,
    family = font_ps,
    size = 2
  ) +
  annotate(
    geom = "text",
    x = c(-1.05, 1.05),
    y = Inf,
    label = c("% total\ntidak sependapat", "% total\nsependapat"),
    family = font_ps,
    fontface = "italic",
    size = 1.75,
    colour = "gray40",
    hjust = c(0, 1),
    vjust = 0
  ) +
  scale_x_continuous(labels = function(x) percent(abs(x))) +
  scale_fill_manual(
    values = c(
      "Sangat tidak setuju" = "#4C9294",
      "Tidak setuju" = "#69B0B1",
      "Netral" = "#F4D562",
      "Setuju" = "#D78DB3",
      "Sangat Setuju" = "#A63A72"
    ),
    breaks = c(
      "Sangat tidak setuju",
      "Tidak setuju",
      "Netral",
      "Setuju",
      "Sangat Setuju"
    ),
    drop = FALSE
  ) +
  #   scale_fill_brewer(
  #     palette = "Spectral",
  #     direction = -1,
  #     breaks = c(
  #       "Sangat tidak setuju",
  #       "Tidak setuju",
  #       "Netral",
  #       "Setuju",
  #       "Sangat Setuju"
  #     ),
  #     drop = FALSE,
#     guide = guide_legend(override.aes = list(colour = "gray40", size = 0.09))
#   ) +
labs(
  title = "Kendala dan Hambatan Perilaku",
  x = NULL,
  y = NULL,
  fill = NULL
) +
  theme_ipsum_rc(
    grid = FALSE,
    ticks = FALSE
  ) +
  theme(
    plot.background = element_rect(fill = "#F4F2EA", colour = NA),
    panel.background = element_rect(fill = "#F4F2EA", colour = NA),
    strip.text = element_text(hjust = 0.5, face = "italic"),
    panel.spacing = unit(0.5, "lines"),
    plot.title = element_text(size = 16,
                              margin = margin(0, 0, 20, 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = rel(0.5)),
    legend.text = element_text(
      size = 6),
    legend.key.width = unit(1, "lines"),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "outfile/section4.png",
  plot = section4_plot,
  width = 8,
  height = 5,
  dpi = 300
)



# Section 5 ----

section5 <- 
  master %>% 
  select(starts_with("k05")) %>% 
  janitor::remove_empty() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "code",
    values_to = "level"
  ) %>% 
  group_by(code, level) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(
    level = factor(
      level,
      levels = c(
        "Sangat tidak setuju",
        "Tidak setuju",
        "Netral",
        "Setuju",
        "Sangat Setuju"
      )
    ),
    type = case_when(
      level == "Netral" ~ "mid",
      level %in% c("Sangat tidak setuju",
                   "Tidak setuju") ~ "low",
      level %in% c("Setuju",
                   "Sangat Setuju") ~ "high"
    ),
    type = factor(type, levels = c("low", "mid", "high"))
  ) %>% 
  left_join(
    metadata %>% 
      select(code = label_indikator, item = nama_indikator_ringkas)
  )

section5_plot <- 
  section5 %>%
  mutate(code = str_wrap(item, 30)) %>% 
  ggplot(aes(pct, code, fill = level)) +
  geom_col(
    data = ~ .x %>%
      filter(type %in% c("mid", "high")) %>% 
      mutate(
        pct = if_else(
          type == "mid",
          pct / 2,
          pct
        ),
        level = fct_rev(level),
        code = fct_reorder(code, pct, sum)
      ),
    width = 0.8
  ) +
  geom_col(
    data = ~ .x %>%
      filter(type %in% c("low", "mid")) %>% 
      mutate(
        pct = if_else(
          type == "mid",
          -1 * pct / 2,
          -1 * pct
        )
      ),
    width = 0.8
  ) +
  geom_text(
    data = ~.x %>% 
      group_by(code, type) %>% 
      summarise(
        n = sum(n),
        pct_label = percent(sum(pct), accuracy = 1)
      ) %>% 
      ungroup() %>% 
      filter(!is.na(type)) %>% 
      mutate(
        pct = case_when(
          type == "low" ~ - 1.05,
          type == "mid" ~ 0,
          type == "high" ~ 1.05
        ),
        hjust = case_when(
          type == "low" ~ - 0,
          type == "mid" ~ 0.5,
          type == "high" ~ 1
        )
      ),
    aes(pct, code, label = pct_label, hjust = hjust),
    inherit.aes = FALSE,
    family = font_ps,
    size = 2
  ) +
  annotate(
    geom = "text",
    x = c(-1.05, 1.05),
    y = Inf,
    label = c("% total\ntidak sependapat", "% total\nsependapat"),
    family = font_ps,
    fontface = "italic",
    size = 1.75,
    colour = "gray40",
    hjust = c(0, 1),
    vjust = 0
  ) +
  scale_x_continuous(labels = function(x) percent(abs(x))) +
  scale_fill_manual(
    values = c(
      "Sangat tidak setuju" = "#4C9294",
      "Tidak setuju" = "#69B0B1",
      "Netral" = "#F4D562",
      "Setuju" = "#D78DB3",
      "Sangat Setuju" = "#A63A72"
    ),
    breaks = c(
      "Sangat tidak setuju",
      "Tidak setuju",
      "Netral",
      "Setuju",
      "Sangat Setuju"
    ),
    drop = FALSE
  ) +
  #   scale_fill_brewer(
  #     palette = "Spectral",
  #     direction = -1,
  #     breaks = c(
  #       "Sangat tidak setuju",
  #       "Tidak setuju",
  #       "Netral",
  #       "Setuju",
  #       "Sangat Setuju"
  #     ),
  #     drop = FALSE,
#     guide = guide_legend(override.aes = list(colour = "gray40", size = 0.09))
#   ) +
labs(
  title = "Kendala dan Hambatan Aspek Organisasi",
  x = NULL,
  y = NULL,
  fill = NULL
) +
  theme_ipsum_rc(
    grid = FALSE,
    ticks = FALSE
  ) +
  theme(
    plot.background = element_rect(fill = "#F4F2EA", colour = NA),
    panel.background = element_rect(fill = "#F4F2EA", colour = NA),
    strip.text = element_text(hjust = 0.5, face = "italic"),
    panel.spacing = unit(0.5, "lines"),
    plot.title = element_text(size = 16,
                              margin = margin(0, 0, 20, 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = rel(0.5)),
    legend.text = element_text(
      size = 6),
    legend.key.width = unit(1, "lines"),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "outfile/section5.png",
  plot = section5_plot,
  width = 8,
  height = 5,
  dpi = 300
)
