# Load package
pkg_to_use <- c("dplyr", "ggplot2", "ggalluvial",
                "showtext", "readxl", "tidyr")

for (pkg in pkg_to_use) {
  if(!pkg %in% rownames(installed.packages())){
    install.packages(pkg)
  }

}
rm(pkg_to_use, pkg)

library(dplyr)
library(ggplot2)
library(ggalluvial)
library(showtext)


## Set font
# Montserrat font download here: https://fonts.google.com/share?selection.family=Montserrat:ital,wght@0,100..900;1,100..900
font_add("mm", "Path\\to\\Montserrat-Medium.ttf")
font_add("mb", "Path\\to\\Montserrat-Bold.ttf")
showtext_auto()
color <- c("#027333", "#A62B1F", "#04D939")

do_alluvial <- function(data,
                        axis1,
                        axis2,
                        y,
                        fill_color = color,
                        axis_title = c("Axis 1", "Axis2"),
                        axis_font = c("mb", 60),
                        stratum_font = c("mm", 30)
) {

  plt <- ggplot(data = traj,
                aes(axis1 = !!dplyr::ensym(axis1),
                    axis2 = !!dplyr::ensym(axis2),
                    y = !!dplyr::ensym(y))) +
    geom_alluvium(aes(fill = !!dplyr::ensym(axis1)),  show.legend = F,
                  curve_type = "cubic", alpha = 1) +
    geom_stratum(fill = "white", color = "gray") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              family = stratum_font[1], size = as.numeric(stratum_font[2]), hjust = 0.5, vjust = 0.5)+
    scale_fill_manual(values = fill_color)+
    scale_x_discrete(limits = axis_title) +
    labs(x = "") +
    theme_minimal()+
    theme(
      plot.margin = margin(0,0,0,0),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = axis_font[1], size = as.numeric(axis_font[2])),
      plot.background = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank()
    )

  return(plt)

}
