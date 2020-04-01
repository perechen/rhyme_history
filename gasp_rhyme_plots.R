library(tidyverse)
library(paletteer)
library(ggrepel)

rhyme_inexact <-
  # read table as csv
  read_csv("gaparov_data/gasp_inexact.csv") %>% 
  # replace NA with 0
  replace_na(list(femin_iotated = 0,
              femin_approx = 0,
              femin_nonexact = 0,
              masc_closed_nonexact = 0,
              masc_open_nonexact = 0,
              masc_closed_open = 0)) %>% 
  select(-base_sound) %>%
  # transform to "long"
  gather(key=rhyme_form, value=n, femin_iotated:masc_closed_open) %>% 
  # get clausula types
  mutate(rhyme_claus = str_replace_all(rhyme_form, "^(.*?)_.*", "\\1"),
         # get decades
         decade = floor(period/10)*10)


# get summarised data by periods
rhyme_summarised = rhyme_inexact %>% 
  group_by(rhyme_form, rhyme_claus, decade) %>% 
  summarise(sd = sd(n),
            mean_decade = mean(n))


# chronology for "crysis" by Gasparov
rect <- tibble(xmin=c(1780,1900), xmax=c(1840,1935), ymin=-Inf, ymax=Inf)  

# crysis labels
crysis = tibble(x=c(1800,1920), y=c(80,80), label=c("Кризис I", "Кризис II"))

# lomonosov label
lomonosov = tibble(x=1730, y=64, label="Ломоносов рифмует как немец",rhyme_claus="masc")

# to change facet labels later
rhyme_names = c(
  `femin` = "женские",
  `masc` = "мужские"
)

# plot
rhyme_summarised %>% 
  # main aes for summarised
  ggplot(aes(decade, mean_decade, group=rhyme_form, color=rhyme_form)) + 
  # plot non-aggregatd points first
  geom_point(data=rhyme_inexact, 
             aes(period, n, group=rhyme_form, color=rhyme_form), 
             alpha=0.15, position = position_jitter(w=1), size=3) +
  # add lines for agg. data
  geom_line(size=2)  + 
  ## cosmetics
  theme_classic() + 
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=-45, hjust=-0.1),
        plot.title = element_text(size=20,face = "bold"),
        plot.subtitle = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x = element_text(size = 14,face="bold")) +
  ## titles etc.
  labs(x="Время", y="% неточных рифм относительно типа рифмы",
       title="История неточной рифмы в русской поэзии (1720-1970)",
       subtitle="Данные из: Гаспаров М.Л. Эволюция русской рифмы // Проблемы теории стиха. Л., 1984.\n\nЛинии показывают изменение среднего (по десятилетиям) состава рифменных форм.\nПрозрачные точки соответстуют реальным подсчетам Гаспарова по поэтам/текстам") +
  scale_x_continuous(breaks = seq(1720, 1970, by = 10)) +
  # color & legend
  scale_color_paletteer_d("ggsci::uniform_startrek", name="",labels=c("Ж приблизительная", "Ж йотированная", "Ж неточная", "М закрытая неточная", "М открытая неточная", "М открытая+закрытая")) +
  # draw crysis highlights
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
          fill=paletteer_d("wesanderson::Royal1")[1],
          color=NA,
          alpha=0.2,
          inherit.aes = F) +
  # add text
  geom_text(data=crysis, aes(x,y,label=label),inherit.aes = F, size=5) +
  geom_label_repel(data=lomonosov, aes(x,y, label=label), inherit.aes = F, nudge_x = 5,nudge_y = 5) +
  # wrap facets to "masculine" and "feminine"
  facet_wrap(~rhyme_claus,labeller = as_labeller(rhyme_names))


ggsave(filename = "rhyme_history_gasp.png", width = 16, height = 10)
