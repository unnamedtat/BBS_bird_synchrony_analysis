##############画图信息################
library(ggthemes)
library(ggplot2)
library(rnaturalearth)
library(sf)
world_map <-  ne_countries(scale = "medium", returnclass = "sf")%>%
  st_transform(4326)
theme_custom <-theme_void()+
  theme(plot.title = element_text(family = "SimSun", size = 16, face = "bold",hjust = 0.5),
        axis.title = element_text(family = "SimSun", size = 14,hjust = 0.5),
        axis.text.y = element_text(color = "black"),
        axis.text = element_text(family = "Times New Roman", size = 12),
        panel.grid.major.y = element_line(colour = "gray",linetype = "dashed"),
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 1,linetype = "solid"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"))

theme_geo <-theme_void()+
  theme(plot.title = element_text(family = "SimSun", size = 16, face = "bold",hjust = 0.5),
        axis.title = element_text(family = "SimSun", size = 14,hjust = 0.5),
        axis.text = element_text(family = "Times New Roman", size = 12),
        axis.text.y = element_text(angle = 90),
        panel.background = element_rect(fill = "transparent", colour = "black", linewidth = 1, linetype = "solid"),
        plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), "in")
  )
theme_bar <-theme_void()+
  theme(plot.title = element_text(family = "SimSun", size = 16, face = "bold",hjust = 0.5),
        axis.title = element_text(family = "SimSun", size = 14,hjust = 0.5),
        axis.text.x = element_text(angle = 45),
        axis.text = element_text(family = "Times New Roman", size = 12),
        panel.grid.major.y = element_line(colour = "gray",linetype = "dashed"),
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 1,linetype = "solid"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"))+
        theme(legend.title = element_text(family = "SimSun"))

fancy_scientific <- function(l) {
     # turn in to character string in scientific notation
     l <- format(l, scientific = TRUE)
     # quote the part before the exponent to keep all the digits
     # l <- gsub("^(.*)e\\+", "'\\1'e", l)
      l <- gsub("^(.*)e\\+", "\\1%*%10\\^", l)
     parse(text=l)
}