
install.packages("igraph")
install.packages("randomcoloR")
install.packages("dplyr")
install.packages("data.table")
install.packages("tidyverse")
install.packages("forcats")
install.packages("igraph")
install.packages("ggspatial")
install.packages("networkD3")
library(dplyr)
library(data.table)
library(sf)
library(tidyverse)
library(forcats)    # Reorder factor levels
library(igraph)     # network object
library(randomcoloR)# random color generator
library(ggspatial)
library(networkD3)

# US Census Cartographic Boundaries: US States (simple features)
sf_us_state <- st_read("Data/cb_2023_us_state/cb_2023_us_state_5m.shp")

### flows: separations from COLORADO 
j2j_from_co <- read.csv("Data/cln_j2j_outflow_from_colorado.csv")
### flows: hires to COLORADO
j2j_to_co <- read.csv("Data/cln_j2j_inflow_to_colorado.csv")
j2j_inflow_industry <- read.csv("Data/j2j_inflow_CO_industry_table.csv")

### j2j_from_co: What states do workers move to? search '.flag'
View(j2j_from_co)
View(j2j_to_co)
### data frame shape: 20 obs. 101 variables
str(j2j_from_co)

##na values
j2j_from_co[2, "avg_n_job"]

#remove rows w na
j2j_from_co_cln <- j2j_from_co %>% na.omit()
j2j_to_co_cln <- j2j_to_co %>% na.omit()

##data prep
data <- j2j_to_co_cln

## box plot record by median
ggplot(data = data, aes(x = reorder(naics_sector, avg_n_job), 
                        y=avg_n_job, fill =naics_sector )) + 
  geom_boxplot() + 
  xlab("NAICS Sector") +
  ylab("Job Counts") + 
  ggtitle("2023 Q1-Q3 Quaterly Average Job Inflow Inflow Colorado") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) 

##outflow

data <- j2j_from_co_cln
ggplot(data = data, aes(x = reorder(naics_sector, avg_n_job), 
                        y=avg_n_job, fill =naics_sector )) + 
  geom_boxplot() + 
  xlab("NAICS Sector") +
  ylab("Job Counts") + 
  ggtitle("2023 Q1-Q3 Quaterly Average Job Outflow from Colorado") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) 
##combine

### combine inflow & outflow dataframe together
j2j_co_bysector <- rbind(j2j_from_co_cln, j2j_to_co_cln)
### combine all sectors together 
j2j_co_aggsector <- aggregate(avg_n_job ~ from_state + to_state + direction, 
                              data = j2j_co_bysector %>% select(-naics_sector), 
                              FUN = sum)
j2j_co_aggsector$naics_sector <- "all_sectors"

head(j2j_co_aggsector)
### Final data
j2j_co <- rbind(j2j_co_aggsector, j2j_co_bysector)


##look
View(j2j_co)
### list of unique states in j2j_co
lt_state <- unique(c(j2j_co$from_state, j2j_co$to_state))
print(lt_state)
### list of unique naics sectors in j2j_co
lt_sector <- unique(j2j_co$naics_sector)
print(lt_sector)
View(data)
### visualize job inflow network
data <- j2j_co %>% filter(naics_sector == "all_sectors", 
                          direction == "inflow_to_co")

# Create a network object 
network <- graph_from_data_frame(d=data, directed=T) 


##network summary
summary(network)



#netwrok plot
plot(network, edge.arrow.size=0.2)

plot(network, vertex.size = 12, 
     edge.arrow.size = 00.2,
     edge.width=E(network)$avg_n_job * 0.0015, 
     edge.curved = TRUE, main = "Job Inflow to Colorado")

#### project to sf_us_state to CRS NAD83 / Colorado North (ftUS)
sf_us_state_j2j <- sf_us_state %>%
  filter(NAME %in% lt_state) %>% st_transform(., 2231)


#### get state centroid  
df_node <- sf_us_state_j2j %>% select(NAME) %>% st_centroid()
#### split point data into lat & long 
df_node <- df_node %>% mutate( long = unlist(map(df_node$geometry, 1)),
                               lat = unlist(map(df_node$geometry, 2)))
st_geometry(df_node) <- NULL

head(df_node)
#### join coordinates to edge 
j2j_co_xy <- j2j_co %>% 
  left_join(., df_node, by=c("from_state"="NAME")) %>% 
  setnames(old=c("long","lat"), new=c("long_start","lat_start")) %>% 
  left_join(., df_node, by=c("to_state"="NAME")) %>% 
  setnames(old=c("long","lat"), new=c("long_end","lat_end"))

#### filter data 
data <- j2j_co_xy %>% filter(direction == "inflow_to_co", 
                             naics_sector == "all_sectors")

#### plot 
p0_inflow <- ggplot(sf_us_state_j2j) + 
  geom_sf(fill= "White")+
  geom_segment(data = data,
               aes(x = long_start, y = lat_start, 
                   xend = long_end, yend = lat_end, linewidth= avg_n_job, colour= avg_n_job),
               alpha= 0.8)+
  scale_color_gradient(low="#FFFFC5", high = "blue")+
  scale_size_continuous(range = c(0.001, 5))+
  geom_point(data =df_node , aes(x = long, y = lat), 
             shape = 18, fill = "white",
             color = 'black', stroke = 0.5) + 
  geom_text(data =df_node, 
            aes(x = long, y = lat, label = NAME), 
            size=2) + 
  ggtitle("Job Inflow to Colorado") + 
  theme_bw()+
  ylab("Latitude") + 
  xlab("Longitude")
p0_inflow

p0_1 <-ggplot(sf_us_state_j2j) + 
  geom_sf() + 
  geom_segment(data = data,
               aes(x = long_start, y = lat_start, 
                   xend = long_end, yend = lat_end, 
                   size = avg_n_job, alpha = 0.5)) + 
  scale_size_continuous(range = c(0.1, 3)) + 
  geom_point(data =df_node , aes(x = long, y = lat), 
             shape = 18, fill = "white",
             color = 'black', stroke = 0.5) + 
  geom_text(data =df_node, 
            aes(x = long, y = lat, label = NAME), 
            size=2) + 
  ggtitle("Job Inflow to Colorado") + 
  ylab("Latitude") + 
  xlab("Longitude")
p0_1

### create a function to plot job outflow by sectors  
map_j2j <- function(idx, full_data, state_boundary, state_point, c_direction, c_sector, edge_color){
  # extract data 
  data <- full_data %>% filter(direction == as.character(c_direction),
                               naics_sector == as.character(c_sector))
  # create title label 
  title <- paste("Job-to-job", as.character(c_direction), as.character(c_sector), sep=" ")
  # create plot 
  p <- ggplot(state_boundary) + 
    geom_sf(fill = "white") + 
    geom_segment(data = data,
                 aes(x = long_start, y = lat_start, 
                     xend = long_end, yend = lat_end, linewidth = avg_n_job, alpha = 0.9, colour = avg_n_job)) + 
    scale_colour_gradient(low="#FFFFC5", high = edge_color) +  
    scale_size_continuous(range = c(0.01, 4)) + 
    geom_point(data =state_point , aes(x = long, y = lat),           # draw nodes
               shape = 18, fill = "white", size=1,
               color = 'grey', stroke = 0.5) + 
    geom_text(data =state_point, aes(x = long, y = lat, label = STUSPS), size=2) + 
    ggtitle(title) + 
    theme_bw() + 
    ylab("Latitude") + 
    xlab("Longitude")
  
  p <- p + annotation_scale(location = "bl", width_hint = 0.1) + 
    annotation_north_arrow(location = "bl", 
                           pad_x = unit(0.1, "in"), 
                           pad_y = unit(0.3, "in"),
                           height = unit(0.3, "in"),
                           width = unit(0.3, "in")) 
  
  pdf(sprintf("plot/p%s_%s.pdf", idx, title),  width = 10, height = 5)
  print(p)
  dev.off()
}
map_j2j
### random color set 
n <- length(lt_sector)
col_random <- randomColor(n)


### map 
for (i in 1:length(lt_sector)) {
  edge_color = col_random[[i]]
  sector_toviz <- lt_sector[[i]]
  
  map_j2j(i, j2j_co_xy, sf_us_state_j2j, df_node, "outflow_from_co", 
          sector_toviz, edge_color)
}

ggplot(sf_us_state_j2j) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering)


# tidy and clean the data
j2j_inflow_ind_cln <- j2j_inflow_industry %>%
  pivot_longer(cols = 2:ncol(.), names_to = "to_co_sector", values_to = "avg_n_job") %>%
  filter(!to_co_sector %like% ".flag") %>%
  na.omit() %>%
  setnames(old = "X", new = "from_sector") %>%
  select(from_sector, to_co_sector, avg_n_job)

# clean sector names to make them consistent
j2j_inflow_ind_cln$to_co_sector <- j2j_inflow_ind_cln$to_co_sector %>%
  gsub("\\.\\.", ", ", .) %>%
  gsub("\\.", " ", .) %>%
  gsub("Other Services, except Public Administration",
       "Other Services (except Public Administration)", .) %>%
  gsub("^\\s+|\\s+$", "", .)

# Prepare network data: edge & node

# edges
df_edge <- j2j_inflow_ind_cln

# top 10 industry with highest inflow to colorado
df_ten_industry <- df_edge %>%
  select(to_co_sector, avg_n_job) %>%
  group_by(to_co_sector) %>%
  summarise(n_ttl = sum(avg_n_job)) %>%
  slice_max(n_ttl, n = 10)

# extract edges from and to the top ten industries
df_edge <- df_edge %>%
  filter(from_sector %in% df_ten_industry$to_co_sector) %>%
  filter(to_co_sector %in% df_ten_industry$to_co_sector)

# add space to "to_co_sector"
df_edge$to_co_sector <- paste(df_edge$to_co_sector, " ", sep = "")

#nodes
df_node <- data.frame(name = unique(c(df_edge$from_sector,
                                      df_edge$to_co_sector)))

#edge add id for nodes
df_edge$id_from <- match(df_edge$from_sector, df_node$name) - 1
df_edge$id_to <- match(df_edge$to_co_sector, df_node$name) - 1


#interactive sankey chart - network
sankeyNetwork(Links = df_edge, Nodes = df_node,
              Source = "id_from", Target = "id_to",
              Value = "avg_n_job", NodeID = "name",
              sinksRight=FALSE, nodeWidth=15, fontSize=10, nodePadding=10)
