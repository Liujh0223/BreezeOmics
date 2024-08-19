## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 7
)

## ----load_libs,message=FALSE,warning=FALSE------------------------------------
library(igraph)   
library(ggraph)   
library(graphlayouts)

## ----example------------------------------------------------------------------
set.seed(666)
pa <- sample_pa(1000,1,1,directed = F)

ggraph(pa,layout = "stress")+
  geom_edge_link0(width = 0.2,colour = "grey")+
  geom_node_point(color = "black",size = 0.3)+
  theme_graph()

## ----example_un---------------------------------------------------------------
set.seed(666)
g <- disjoint_union(
  sample_pa(10,directed = F),
  sample_pa(20,directed = F),
  sample_pa(30,directed = F),
  sample_pa(40,directed = F),
  sample_pa(50,directed = F),
  sample_pa(60,directed = F),
  sample_pa(80,directed = F)
)

ggraph(g, layout = "stress",bbox = 40) +
  geom_edge_link0() +
  geom_node_point() +
  theme_graph()

## ----hairball,eval = TRUE-----------------------------------------------------
set.seed(665)
#create network with a group structure
g <- sample_islands(9,40,0.4,15)
g <- simplify(g)
V(g)$grp <- as.character(rep(1:9,each=40))

ggraph(g,layout = "stress")+
  geom_edge_link0(colour = rgb(0,0,0,0.5),width = 0.1)+
  geom_node_point(aes(col = grp))+
  scale_color_brewer(palette = "Set1")+
  theme_graph()+
  theme(legend.position = "none")


## ----install_oaqc, eval=FALSE-------------------------------------------------
#  install.packages("oaqc")

## ----backbone,eval=TRUE-------------------------------------------------------
bb <- layout_as_backbone(g,keep=0.4)
E(g)$col <- F
E(g)$col[bb$backbone] <- T

ggraph(g,layout = "manual",x = bb$xy[,1],y = bb$xy[,2])+
  geom_edge_link0(aes(color = col),width = 0.1)+
  geom_node_point(aes(color = grp))+
  scale_color_brewer(palette = "Set1")+
  scale_edge_color_manual(values = c(rgb(0,0,0,0.3),rgb(0,0,0,1)))+
  theme_graph()+
  theme(legend.position = "none")

## ----flex_focus,eval=TRUE-----------------------------------------------------
karate <- make_graph("Zachary")

p1 <- ggraph(karate,layout = "focus",focus = 1) +
  draw_circle(use = "focus",max.circle = 3)+
  geom_edge_link0(edge_color="black",edge_width=0.3)+
  geom_node_point(fill="grey25", size=2,shape=21)+
  scale_fill_manual(values=c("#8B2323", "#EEAD0E"))+
  theme_graph()+
  theme(legend.position = "none")+
  coord_fixed()+
  labs(title= "Focus on Mr. Hi")

p2 <- ggraph(karate,layout = "focus",focus = 34) +
  draw_circle(use = "focus",max.circle = 4)+
  geom_edge_link0(edge_color="black",edge_width=0.3)+
  geom_node_point(fill="grey25",size=2,shape=21)+
  scale_fill_manual(values=c("#8B2323", "#EEAD0E"))+
  theme_graph()+
  theme(legend.position = "none")+
  coord_fixed()+
  labs(title= "Focus on John A.")

p1
p2

## ----flex_cent----------------------------------------------------------------

bc <- betweenness(karate)
p1 <- ggraph(karate,layout = "centrality", centrality = bc, tseq = seq(0,1,0.15)) +
  draw_circle(use = "cent") +
  annotate_circle(bc,format="",pos="bottom") +
  geom_edge_link0(edge_color="black",edge_width=0.3)+
  geom_node_point(fill="grey25", size=2,shape=21)+
  scale_fill_manual(values=c("#8B2323", "#EEAD0E"))+
  theme_graph()+
  theme(legend.position = "none")+
  coord_fixed()+
  labs(title="betweenness centrality")


cc <- closeness(karate)
p2 <- ggraph(karate,layout = "centrality", centrality = cc, tseq = seq(0,1,0.2)) +
  draw_circle(use = "cent") +
  annotate_circle(cc,format="scientific",pos="bottom") +
  geom_edge_link0(edge_color="black",edge_width=0.3)+
  geom_node_point(fill="grey25",size=2,shape=21)+
  scale_fill_manual(values=c("#8B2323", "#EEAD0E"))+
  theme_graph()+
  theme(legend.position = "none")+
  coord_fixed()+
  labs(title="closeness centrality")

p1
p2

