library(ggplot2)
library(dplyr)
library(extrafont)
library(extrafontdb)
library(reshape2)
library(grid)
library(png)
# font_import()
  

pokemon <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv")

pokemon2<-melt(pokemon[,c("ID_poke", "nombre_ingles", "nombre_traducido", "tipo_1", "puntos_vida" ,"ataque", "defensa",
                          "fuerza_especial_ataque", "fuerza_especial_defensa", "velocidad")], 1:4)


pokemon2$variable<-toupper(gsub("_", " ",pokemon2$variable))

cols<-c("springgreen3", "orange", "dodgerblue3", 
        "khaki4","rosybrown","mediumorchid4", 
        "gold", "tan4", "lightsalmon", 
        "red2", "violetred1", "wheat3",
        "mediumpurple4", "skyblue1", "turquoise3",
        "gray30",  "gray70", "yellowgreen")
names(cols)<-unique(pokemon$tipo_1)


set.seed(123)

### BOXPLOT POR TIPO

lab <- function(x) {
  return(x == min(x) | x == max(x))
}

p<-pokemon %>%
  group_by(tipo_1) %>%
  mutate(outlier = ifelse(lab(total), gsub(" ", "\n",nombre_traducido), NA)) %>%
  ggplot(., aes(x = factor(tipo_1), y = total, fill=tipo_1)) +
  geom_boxplot(colour="gray50", alpha=0.5,outlier.size = 0, outlier.colour = "transparent")+
  geom_jitter(aes(colour=tipo_1), shape=1, width = 0.1) +
  ggrepel::geom_text_repel(aes(label = outlier, colour=tipo_1), alpha=1, 
                           size=3, na.rm = TRUE, vjust=0.15, 
                           position = position_jitter(seed = 123, width = 0.1)) +

  labs(y="Poder", x="Tipo", caption="@sporella")+
  ggtitle(toupper("\n¿Qué tipo de pokémon es más poderoso?\n"))+
  scale_fill_manual(values=cols)+
  scale_colour_manual(values=cols)
p <- p+
  theme_classic()+
  theme(legend.position="none", 
        plot.title = element_text(size = 40, family = "Pokemon Hollow", colour = "cyan4"), 
        axis.text.x = element_text(size = 12, family = "Pokemon Solid", colour = cols[order(names(cols))]), 
        axis.text.y = element_text(size = 12, family = "Pokemon Solid", colour = "cyan4"),
        axis.title = element_text(size = 15, family = "Pokemon Solid", colour = "grey"), 
        panel.background = element_rect(fill = 'gray10'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray10"),
        axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        plot.caption = element_text(color = 'white',size=12, family="Impact", hjust = 0)
        )
p<-p
ggsave(filename = "poder.png", plot = p, width=15, height = 8)

### GRAFICO RADIAL, PROMEDIO DE PODER POR TIPO

poder <- pokemon2 %>% group_by(tipo_1, variable) %>% summarise(media = round(mean(value),0))

p2<-ggplot(poder)+
  aes(x=tipo_1, y=media, fill=variable)+
  geom_col(colour="gray10", alpha=0.7)+
  geom_text(aes(label=media), position=position_stack(vjust = 0.5), size=3, colour="white")+
  scale_y_continuous(expand = c(0.2,0))+
  coord_polar()+ theme(legend.position = "bottom")+
  labs(y="Poder", x="", fill="", caption = "@sporella")+
  ggtitle("\nPoderes según tipos\n")+
  theme_classic()+
  theme(legend.position="right", 
        plot.title = element_text(size = 40, family = "Pokemon Hollow", colour = "cyan4"), 
        axis.text = element_text(size = 12, family = "Pokemon Solid", colour = "cyan4"), 
        axis.title = element_text(size = 20, family = "Pokemon Solid", colour = "gray"), 
        panel.background = element_rect(fill = 'gray10'),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        plot.background = element_rect(fill = "gray10"),
        axis.line = element_line(colour = "gray"),
        panel.border = element_blank(),
        legend.background = element_rect(fill="gray10"),
        legend.text = element_text(size=14, family="Segoe UI", colour="white"),
        plot.caption = element_text(color = 'white',size=12, family="Impact", hjust = 0)
  )+
  guides(fill=guide_legend(nrow=6, reverse = T, 
                            label.position = "top"))
ggsave(filename = "poder_radial.png", plot = p2, width=10, height = 8)

### TIPOS SEGUN PODER

top<- pokemon %>% filter(total>400) %>%  group_by(tipo_1) %>% top_n(n = 5,total)

p3<-ggplot(top)+aes(y=total, colour=tipo_1, x=tipo_1)+
ggrepel::geom_label_repel(aes(label=nombre_traducido), fill="gray10",
                          position=position_jitter(width=0.1), size=3, family="Impact" )+
labs(y="Poder total", x="", caption="@sporella", title = "\nLos más poderosos\n", colour="")+
  theme(legend.position = "none")+
scale_colour_manual(values=cols)+
  theme(legend.position="none", 
        plot.title = element_text(size = 40, family = "Pokemon Hollow", colour = "cyan4"), 
        axis.text.x = element_text(size = 12, family = "Pokemon Solid", colour = cols[order(names(cols))]), 
        axis.text.y = element_text(size = 12, family = "Pokemon Solid", colour = "cyan4"),
        axis.title = element_text(size = 20, family = "Pokemon Solid", colour = "gray"), 
        panel.background = element_rect(fill = 'gray10'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray10"),
        axis.line = element_line(colour = "gray"),
        panel.border = element_blank(),
        legend.background = element_rect(fill="gray10"),
        legend.text = element_text(size=14, family="Segoe UI", colour="white"),
        plot.caption = element_text(color = 'white',size=12, family="Impact", hjust = 0)
  )

ggsave(filename = "poderosos.png", plot = p3, width=15, height = 8)

###### Mi favorito

ids<-c(4,5,6)

char<-pokemon2 %>% filter(ID_poke %in% ids)

char$nombre_traducido<-factor(char$nombre_traducido, levels = c("Charmander","Charmeleon","Charizard","Charizard Mega X", "Charizard Mega Y"))

p4<-ggplot(char)+aes(x=nombre_traducido, y=value, fill=variable)+
  geom_col(position="stack", colour="gray24")+
  geom_text(aes(label=value), position=position_stack(vjust=0.5), colour="white")+
  labs(y="Poder total", x="", caption="@sporella", title = "\nEvoluciones de Charmander\n", fill="")+
  theme(legend.position = "none")+
  scale_colour_manual(values=cols)+
  scale_y_continuous(limits=c(0, 900))


e1<- rasterGrob(readPNG("charmander_t2.png"),width = unit(1,"npc"), height = unit(1,"npc"))
e2<-rasterGrob(readPNG("charmeleon_t.png"),width = unit(1,"npc"), height = unit(1,"npc"))
e3<-rasterGrob(readPNG("charizard_t.png"),width = unit(1,"npc"), height = unit(1,"npc"))
e4<-rasterGrob(readPNG("charizard_mega_x_t.png"),width = unit(1,"npc"), height = unit(1,"npc"))
e5<-rasterGrob(readPNG("charizard_mega_y_t.png"),width = unit(1,"npc"), height = unit(1,"npc"))

p4<-p4 + annotation_custom(grob = e1, xmin = 0.5, xmax = 1.4, ymin = 350, ymax=520)+
  annotation_custom(grob = e2, xmin = 1.5, xmax = 2.4, ymin = 450, ymax=600)+
  annotation_custom(grob = e3, xmin = 2.5, xmax = 3.4, ymin = 560, ymax=750)+
  annotation_custom(grob = e4, xmin = 3.5, xmax = 4.4, ymin = 680, ymax=900)+
  annotation_custom(grob = e5, xmin = 4.5, xmax = 5.4, ymin = 680, ymax=900)
p4<-p4+
  theme(legend.position="bottom", 
        plot.title = element_text(size = 40, family = "Pokemon Hollow", colour = "cyan4"), 
        axis.text.x = element_text(size = 12, family = "Pokemon Solid", colour = "cyan4"), 
        axis.text.y = element_text(size = 12, family = "Pokemon Solid", colour = "cyan4"),
        axis.title = element_text(size = 20, family = "Pokemon Solid", colour = "gray"), 
        panel.background = element_rect(fill = 'gray10'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray10"),
        axis.line = element_line(colour = "gray"),
        panel.border = element_blank(),
        legend.background = element_rect(fill="gray10"),
        legend.text = element_text(size=14, family="Segoe UI", colour="white"),
        plot.caption = element_text(color = 'white',size=12, family="Impact", hjust = 0)
  )

ggsave(filename = "evolucion.png", plot = p4, width=10, height = 8)
