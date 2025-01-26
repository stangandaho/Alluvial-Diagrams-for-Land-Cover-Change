# Load setup file
source("setup.R")


## traj
traj <- read.csv("data_for_ad.csv", check.names = FALSE) %>%
  tidyr::separate_wider_delim(cols = classes, delim = "à", names = c("year1", "year2")) %>%
  mutate(year1 = trimws(year1), year2 = trimws(year2),
         year1 = case_when(year1 == "Foret" ~ "Forest",
                           year1 == "Savane" ~ "Savanna",
                           TRUE ~ "Other"),
         year2 = case_when(year2 == "Foret" ~ "Forest",
                           year2 == "Savane" ~ "Savanna",
                           TRUE ~ "Other")) %>%
  dplyr::filter(`2002-2013` != 0,
                `2013-2023` != 0)

# Plot

# 1
do_alluvial(data = traj, axis1 = year1, axis2 = year2, y = `2002-2013`,
            fill_color = color, axis_title = c("2002", "2013"))+
  geom_text(aes(label = paste0(`2013-2023`, "%")),
            stat = "flow", size = 18, nudge_x = 0.23, nudge_y = .6,
            color = "white", family = "mm")

ggsave(filename = "All1.jpeg", width = 35, height = 25,
       units = "cm", dpi = 300)


# 2
do_alluvial(data = traj, axis1 = year1, axis2 = year2, y = `2013-2023`,
            fill_color = color, axis_title = c("2013", "2023"))+
  geom_text(aes(label = paste0(`2013-2023`, "%")),
            stat = "flow", size = 18, nudge_x = 0.23, nudge_y = .6,
            color = "white", family = "mm")

ggsave(filename = "All2.jpeg", width = 35, height = 25,
       units = "cm", dpi = 300)

# Merge manually the outputs
