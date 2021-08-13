#### Set-Up ####
# Required packages 
library(tidyverse)
library(tidytext)
library(textreadr)
library(igraph)
library(brainGraph)
library(VertexSort)
library(data.table)
library(parallel)
library(kableExtra)
library(patchwork)
library(beepr)
theme_set(theme_classic())

# Build function
get_interactions <- function(begin, run, turns, N) {
  
  path <- paste0(begin, run, '.txt')
  # Data for just one run
  tm_data <- read.delim(path,
                        header = F, sep = ',')
  # Let's get board coordinates for the each time step 
  # Create function to extract a matrix of coordinates for each time-step
  get_coordinates <- function(x, num_agents) {
    tm <- tm_data[x,-ncol(tm_data)] %>% as_vector() %>% matrix(nrow = num_agents, ncol = 4, byrow = T)
    colnames(tm) <- c('monkey', 'x_coord', 'y_coord', 'fruit')
    return(tm)
  }
  # Iterate over all time-steps
  full_coord_list <- map(c(1:turns), get_coordinates, num_agents=N)
  # Now I am going to create a function to extract network matrices 
  get_network_matrix <- function(x) {
    # extract dataset for the coordinates of one time-step 
    coord <- full_coord_list[[x]] %>% as.data.frame(.)
    # Empty placeholder matrix
    net_mat <- matrix(0, N, N)  
    for (j in 1:N) {
      # Coordinates for where agent j is 
      mcx <- coord$x_coord[j] 
      mcy <- coord$y_coord[j]
      # Are there any monkeys there 
      coincide <- which(coord$x_coord==mcx & coord$y_coord==mcy)
      # If there are record on the network matrix
      for (i in 1:length(coincide)) {
        net_mat[j,coincide[[i]]] <- 1
      }
    }
    return(net_mat)
  }
  
  # Let's get all networks 
  full_matrix_list <- map(c(1:turns), get_network_matrix)
  
  interactions_per_turn <- function(x) {
    this_turn <- full_matrix_list[[x]]
    diag(this_turn) <- 0
    interactions <- sum(this_turn)/2
    return(interactions)
  }
  
  ints <- map_dbl(c(1:turns), 
                  interactions_per_turn)
  return(ints)
}

#### Beta 1.5 - Monkey 100 ####
# Radius 1
int_n100_b15_r1 <- map(c(0:19), 
                       begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius1/", 
                       get_interactions, 
                       turns = 1000, 
                       N = 100)
int_n100_b15_r1_df <- tibble(
  run = rep(c(0:19), 
            each = 1000),
  turn = rep(1:1000, 20),
  num_ints = unlist(int_n100_b15_r1),
  radius = 1, 
  beta = 1.5, 
  population = 100
)

# Radius 0.1
int_n100_b15_r01 <- map(c(0:19), 
                        begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius0.1/", 
                        get_interactions, 
                        turns = 1000, 
                        N = 100)
int_n100_b15_r01_df <- tibble(
  run = rep(c(0:19), 
  each = 1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b15_r01),
  radius = 0.1, 
  beta = 1.5, 
  population = 100
)

# Radius 0.01 
int_n100_b15_r001 <- map(c(0:19), 
                         begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius0.01/", 
                         get_interactions, 
                         turns = 1000, 
                         N = 100)
int_n100_b15_r001_df <- tibble(
  run = rep(c(0:19),
              each = 1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b15_r001),
  radius = 0.01, 
  beta = 1.5, 
  population = 100
)

# Radius 0.001 
int_n100_b15_r0001 <- map(c(0:19), 
                          begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius0.001/", 
                          get_interactions, 
                          turns = 1000, 
                          N = 100)
int_n100_b15_r0001_df <- tibble(
  run = rep(c(0:19), each =1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b15_r0001),
  radius = 0.001, 
  beta = 1.5, 
  population = 100
)

#### Beta 2.5 - Monkey 100 ####
# Radius 1
int_n100_b25_r1 <- map(c(0:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius1/", 
    get_interactions, 
    turns = 1000, 
    N = 100)
int_n100_b25_r1_df <- tibble(
  run = rep(c(0:19), each =1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b25_r1),
  radius = 1, 
  beta = 2.5, 
  population = 100
)

# Radius 0.1
int_n100_b25_r01 <- map(c(0:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius0.1/", 
    get_interactions, 
    turns = 1000, 
    N = 100)
int_n100_b25_r01_df <- tibble(
  run = rep(c(0:19), each =1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b25_r01),
  radius = 0.1, 
  beta = 2.5, 
  population = 100
)

# Radius 0.01 
int_n100_b25_r001 <- map(c(0:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius0.01/", 
    get_interactions, 
    turns = 1000, 
    N = 100)
int_n100_b25_r001_df <- tibble(
  run = rep(c(0:19), each = 1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b25_r001),
  radius = 0.01, 
  beta = 2.5, 
  population = 100
)

# Radius 0.001 
int_n100_b25_r0001 <- map(c(0:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius0.001/", 
    get_interactions, 
    turns = 1000, 
    N = 100)
int_n100_b25_r0001_df <- tibble(
  run = rep(c(0:19), each =1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b25_r0001),
  radius = 0.001, 
  beta = 2.5, 
  population = 100
)

#### Beta 3.5 - Monkey 100 ####
# Radius 1
int_n100_b35_r1 <- map(c(0:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius1/", 
    get_interactions, 
    turns = 1000, 
    N = 100)
int_n100_b35_r1_df <- tibble(
  run = rep(c(0:19), each =1000),
  turn = rep(1:1000, 20), 
  num_ints = unlist(int_n100_b35_r1),
  radius = 1, 
  beta = 3.5, 
  population = 100
)

# Radius 0.1
int_n100_b35_r01 <- map(c(0:12, 14:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius0.1/", 
    get_interactions, 
    turns = 1000, 
    N = 100)

int_n100_b35_r01_df <- tibble(
  run = rep(c(0:18),each = 1000),
  turn = rep(1:1000,19), 
  num_ints = unlist(int_n100_b35_r01),
  radius = 0.1, 
  beta = 3.5, 
  population = 100
)

# Radius 0.01 
int_n100_b35_r001 <- map(c(0:10,12:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius0.01/", 
    get_interactions, 
    turns = 1000, 
    N = 100)
int_n100_b35_r001_df <- tibble(
  run = rep(c(0:18), each =1000),
  turn = rep(1:1000,19), 
  num_ints = unlist(int_n100_b35_r001),
  radius = 0.01, 
  beta = 3.5, 
  population = 100
)

# Radius 0.001 
int_n100_b35_r0001 <- map(c(0:19), 
    begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius0.001/", 
    get_interactions, 
    turns = 1000, 
    N = 100)
int_n100_b35_r0001_df <- tibble(
  run = rep(c(0:19), each =1000),
  turn = rep(1:1000, 20),
  num_ints = unlist(int_n100_b35_r0001),
  radius = 0.001, 
  beta = 3.5, 
  population = 100
)


#### Beta 4.5 - Monkey 100 ####
# Radius 1
int_n100_b45_r1 <- map(c(0:7), 
                       begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius1/", 
                       get_interactions, 
                       turns = 1000, 
                       N = 100)
int_n100_b45_r1_df <- tibble(
  run = rep(c(0:7), each = 1000),
  turn = rep(1:1000, 8),
  num_ints = unlist(int_n100_b45_r1),
  radius = 1, 
  beta = 4.5, 
  population = 100
)

# Radius 0.1
int_n100_b45_r01 <- map(c(0:7), 
                        begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius0.1/", 
                        get_interactions, 
                        turns = 1000, 
                        N = 100)
int_n100_b45_r01_df <- tibble(
  run = rep(c(0:7), each = 1000),
  turn = rep(1:1000, 8), 
  num_ints = unlist(int_n100_b45_r01),
  radius = 0.1, 
  beta = 4.5, 
  population = 100
)

# Radius 0.01 
int_n100_b45_r001 <- map(c(0:7), 
                         begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius0.01/", 
                         get_interactions, 
                         turns = 1000, 
                         N = 100)
int_n100_b45_r001_df <- tibble(
  run = rep(c(0:7),each = 1000),
  turn = rep(1:1000, 8), 
  num_ints = unlist(int_n100_b45_r001),
  radius = 0.01, 
  beta = 4.5, 
  population = 100
)

# Radius 0.001 
int_n100_b45_r0001 <- map(c(0:7), 
                          begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius0.001/", 
                          get_interactions, 
                          turns = 1000, 
                          N = 100)
int_n100_b45_r0001_df <- tibble(
  run = rep(c(0:7), each =1000),
  turn = rep(1:1000, 8), 
  num_ints = unlist(int_n100_b45_r0001),
  radius = 0.001, 
  beta = 4.5, 
  population = 100
)

#### Interactions Plot ####
complete_df <- rbind(int_n100_b15_r1_df, 
                     int_n100_b15_r01_df, 
                     int_n100_b15_r001_df, 
                     int_n100_b15_r0001_df, 
                     int_n100_b25_r1_df,
                     int_n100_b25_r01_df, 
                     int_n100_b25_r001_df, 
                     int_n100_b25_r0001_df,
                     int_n100_b35_r1_df,
                     int_n100_b35_r01_df,
                     int_n100_b35_r001_df, 
                     int_n100_b35_r0001_df, 
                     int_n100_b45_r1_df, 
                     int_n100_b45_r01_df, 
                     int_n100_b45_r001_df, 
                     int_n100_b45_r0001_df)

summed <- complete_df %>% 
  group_by(radius, beta, population, turn) %>% 
  summarise(avg = mean(num_ints), 
            sd = sd(num_ints), 
            lower = quantile(num_ints, 0.25), 
            upper = quantile(num_ints, 0.75)) %>% 
  mutate(population = as.factor(population))

summed <- summed %>% 
  mutate(radius_rl = recode(radius, 
                            `1` = "1", 
                            `0.1` = "0.1", 
                            `0.01` = "0.01", 
                            `0.001` = "0.001"))

#summed$radius_rl[which(is.na(summed$radius_rl))] <- "Point-to-point"

p_ints <- summed %>% 
  ggplot(aes(x = turn, y = avg, fill = population)) + 
  geom_line(aes(color = population)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  facet_grid(beta ~ radius_rl)  + 
  labs(title = "Interactions per turn", 
       y = "Number of interactions", 
       x = "Turn", 
       caption = "Population 100") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.text  = element_text(size = 10))   +
  theme(legend.position = "none")  

ggsave("~/Documents/InformationTransmission/networks/interactions_1000.svg",
       p_ints, 
       height = 8, 
       width = 10)
#### Function for local efficiency ####
local_eff <- function(g) {
  if (is_empty(E(g))) {
    eff <- 0
  } else {
    # Turn it into an adjacency matrix 
    net_mat <- as_adj(g,
                      attr = 'weight', 
                      sparse = F)
    # Get the inverse matrix 
    mat_inv <- net_mat
    edges <- which(mat_inv > 0)
    mat_inv[edges] <- (1 / mat_inv[edges])+1
    # Populate the diagonal with 0s
    diag(mat_inv) <- 0
    
    # Create the new graph 
    net_inv <- graph_from_adjacency_matrix(mat_inv, 
                                           weighted = T, 
                                           mode = 'undirected')
    
    if ('degree' %in% vertex_attr_names(net_inv)) {
      degs <- V(net_inv)$degree
    } else {
      degs <- degree(net_inv)
    } 
    
    eff <- numeric(length(degs))
    nodes <- which(degs > 1)
    if (is_empty(eff[nodes])==TRUE) {
      eff <- rep(0, 100)
    } else {
      eff[nodes] <- simplify2array(mclapply(nodes, function(x) {
        neighbs <- neighbors(net_inv, v=x)
        g.sub <- induced.subgraph(net_inv, neighbs)
        Nv <- vcount(g.sub)
        
        paths <- shortest.paths(g.sub)
        paths <- paths[upper.tri(paths)]
        2 / Nv / (Nv - 1) * sum(1 / paths[paths != 0])
      }, mc.cores=detectCores())
      )
    }
  }
  return(eff)
}

#### Function Global Efficiency ####
inverse_efficiency <- function(g) {
  
  if (is_empty(E(g))) {
    geff <- 0
  } else {
    
    # Turn it into an adjacency matrix 
    net_mat <- as_adj(g,
                      attr = 'weight', 
                      sparse = F)
    
    # Get the inverse matrix 
    mat_inv <- net_mat
    edges <- which(mat_inv > 0)
    mat_inv[edges] <- (1 / mat_inv[edges])+1
    # Populate the diagonal with 0s
    diag(mat_inv) <- 0
    
    # Create the new graph 
    net_inv <- graph_from_adjacency_matrix(mat_inv, 
                                           weighted = T, 
                                           mode = 'undirected')
    D <- distances(net_inv, 
                   weights = E(net_inv)$weight)
    Nv <- nrow(D)
    Dinv <- 1/D
    eff <- colSums(Dinv * is.finite(Dinv), na.rm = T)/(Nv - 1)
    geff <- sum(eff)/length(eff)
  }
  return(geff)
}

#### Function Efficiencies per slide ####
efficiencies_per_slice <- function(slice, 
                                   begin, 
                                   run, 
                                   turns, 
                                   N) {
  
  path <- paste0(begin, run, '.txt')
# Data for just one run
tm_data <- read.delim(path,
                      header = F, sep = ',')
# Let's get board coordinates for the each time step 
# Create function to extract a matrix of coordinates for each time-step
get_coordinates <- function(x, num_agents) {
  tm <- tm_data[x,-ncol(tm_data)] %>% as_vector() %>% matrix(nrow = num_agents, ncol = 4, byrow = T)
  colnames(tm) <- c('monkey', 'x_coord', 'y_coord', 'fruit')
  return(tm)
}
# Iterate over all time-steps
full_coord_list <- map(c(1:turns), get_coordinates, num_agents=N)
# Now I am going to create a function to extract network matrices 
get_network_matrix <- function(x) {
  # extract dataset for the coordinates of one time-step 
  coord <- full_coord_list[[x]] %>% as.data.frame(.)
  # Empty placeholder matrix
  net_mat <- matrix(0, N, N)  
  for (j in 1:N) {
    # Coordinates for where agent j is 
    mcx <- coord$x_coord[j] 
    mcy <- coord$y_coord[j]
    # Are there any monkeys there 
    coincide <- which(coord$x_coord==mcx & coord$y_coord==mcy)
    # If there are record on the network matrix
    for (i in 1:length(coincide)) {
      net_mat[j,coincide[[i]]] <- 1
    }
  }
  return(net_mat)
}

# Let's get all networks 
full_matrix_list <- map(c(1:turns), get_network_matrix)

# How many slices? 

sections <- seq(0, 1000, slice)
sections <- sections[-1]

get_efficiencies_per_section <- function(x) {
  filtered_mats <- full_matrix_list[1:x]
  full_matrix <- Reduce("+", filtered_mats)
  # Get rid of the diagonal because it's meaningless 
  diag(full_matrix) <- 0
  # Create network
  network <- graph_from_adjacency_matrix(full_matrix, mode = "undirected", weighted = T)
  leff <- sum(local_eff(network))/100
  geff <- inverse_efficiency(network)
  return(c(global = geff, 
           local = leff, 
           md = mean_distance(network), 
           cs = components(network)$no))
}
effs_df <- map_df(sections, get_efficiencies_per_section)

return(effs_df)
}

#### Beta 1.5 - 100 Monkeys ####
# Radius 1
effs_n100_b15_r1 <- map_df(c(0:19), 
                           begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius1/", 
                           efficiencies_per_slice, 
                           turns = 1000, 
                           N = 100, 
                           slice = 100)
# Turn it into a tibble
effs_n100_b15_r1_df <- tibble(
  run = rep(c(0:19), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), times = 20),
  global = effs_n100_b15_r1$global, 
  local = effs_n100_b15_r1$local, 
  mean_dist = effs_n100_b15_r1$md,
  cs = effs_n100_b15_r1$cs,
  beta = 1.5, 
  radius = 1, 
  population = 100, 
)

# Radius 0.1
effs_n100_b15_r01 <- map_df(c(0:19), 
                            begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius0.1/", 
                            efficiencies_per_slice, 
                            turns = 1000, 
                            N = 100, 
                            slice = 100)
# Turn it into a tibble
effs_n100_b15_r01_df <- tibble(
  run = rep(c(0:19), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b15_r01$global, 
  local = effs_n100_b15_r01$local, 
  mean_dist = effs_n100_b15_r01$md,
  cs = effs_n100_b15_r01$cs,
  beta = 1.5, 
  radius = 0.1, 
  population = 100, 
)

# Radius 0.01
effs_n100_b15_r001 <- map_df(c(0:19), 
                             begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius0.1/", 
                             efficiencies_per_slice, 
                             turns = 1000, 
                             N = 100, 
                             slice = 100)
# Turn it into a tibble
effs_n100_b15_r001_df <- tibble(
  run = rep(c(0:19), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b15_r001$global, 
  local = effs_n100_b15_r001$local, 
  mean_dist = effs_n100_b15_r001$md,
  cs = effs_n100_b15_r001$cs,
  beta = 1.5, 
  radius = 0.01, 
  population = 100, 
)

# Radius 0.001
effs_n100_b15_r0001 <- map_df(c(0:19), 
                              begin ="/Users/nrestrepo/Downloads/beta_alll/beta1_final/beta1.5/monkeys100/radius0.001/", 
                              efficiencies_per_slice, 
                              turns = 1000, 
                              N = 100, 
                              slice = 100)
# Turn it into a tibble
effs_n100_b15_r0001_df <- tibble(
  run = rep(c(0:19), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b15_r0001$global, 
  local = effs_n100_b15_r0001$local, 
  mean_dist = effs_n100_b15_r0001$md,
  cs = effs_n100_b15_r0001$cs,
  beta = 1.5, 
  radius = 0.001, 
  population = 100, 
)

#### Beta 2.5 - 100 Monkeys ####
# Radius 1
effs_n100_b25_r1 <- map_df(c(0:19), 
                           begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius1/", 
                           efficiencies_per_slice, 
                           turns = 1000, 
                           N = 100, 
                           slice = 100)
# Turn it into a tibble
effs_n100_b25_r1_df <- tibble(
  run = rep(c(0:19), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b25_r1$global, 
  local = effs_n100_b25_r1$local, 
  mean_dist = effs_n100_b25_r1$md,
  cs = effs_n100_b25_r1$cs,
  beta = 2.5, 
  radius = 1, 
  population = 100, 
)

# Radius 0.1
effs_n100_b25_r01 <- map_df(c(0:19), 
                            begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius0.1/", 
                            efficiencies_per_slice, 
                            turns = 1000, 
                            N = 100, 
                            slice = 100)
# Turn it into a tibble
effs_n100_b25_r01_df <- tibble(
  run = rep(c(0:19), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b25_r01$global, 
  local = effs_n100_b25_r01$local, 
  mean_dist = effs_n100_b25_r01$md,
  cs = effs_n100_b25_r01$cs,
  beta = 2.5, 
  radius = 0.1, 
  population = 100, 
)

# Radius 0.01
effs_n100_b25_r001 <- map_df(c(0:19), 
                             begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius0.01/", 
                             efficiencies_per_slice, 
                             turns = 1000, 
                             N = 100, 
                             slice = 100)
# Turn it into a tibble
effs_n100_b25_r001_df <- tibble(
  run = rep(c(0:19), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b25_r001$global, 
  local = effs_n100_b25_r001$local, 
  mean_dist = effs_n100_b25_r001$md,
  cs = effs_n100_b25_r001$cs,
  beta = 2.5, 
  radius = 0.01, 
  population = 100, 
)

# Radius 0.001
effs_n100_b25_r0001 <- map_df(c(0:19), 
                              begin ="/Users/nrestrepo/Downloads/beta_alll/beta2_final/beta2.5/monkeys100/radius0.001/", 
                              efficiencies_per_slice, 
                              turns = 1000, 
                              N = 100, 
                              slice = 100)
# Turn it into a tibble
effs_n100_b25_r0001_df <- tibble(
  run = rep(c(0:19),each =10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b25_r0001$global, 
  local = effs_n100_b25_r0001$local, 
  mean_dist = effs_n100_b25_r0001$md,
  cs = effs_n100_b25_r0001$cs,
  beta = 2.5, 
  radius = 0.001, 
  population = 100, 
)

#### Beta 3.5 - 100 Monkeys ####
# Radius 1
effs_n100_b35_r1 <- map_df(c(0:19), 
                           begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius1/", 
                           efficiencies_per_slice, 
                           turns = 1000, 
                           N = 100, 
                           slice = 100)
# Turn it into a tibble
effs_n100_b35_r1_df <- tibble(
  run = rep(c(0:19), each =10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b35_r1$global, 
  local = effs_n100_b35_r1$local, 
  mean_dist = effs_n100_b35_r1$md,
  cs = effs_n100_b35_r1$cs,
  beta = 3.5, 
  radius = 1, 
  population = 100, 
)

# Radius 0.1
effs_n100_b35_r01 <- map_df(c(0:12, 14:19), 
                            begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius0.1/", 
                            efficiencies_per_slice, 
                            turns = 1000, 
                            N = 100, 
                            slice = 100)
# Turn it into a tibble
effs_n100_b35_r01_df <- tibble(
  run = rep(c(0:18),each=10), 
  turn = rep(seq(from = 100, to = 1000, 100), 19),
  global = effs_n100_b35_r01$global, 
  local = effs_n100_b35_r01$local, 
  mean_dist = effs_n100_b35_r01$md,
  cs = effs_n100_b35_r01$cs,
  beta = 3.5, 
  radius = 0.1, 
  population = 100, 
)

# Radius 0.01
effs_n100_b35_r001 <- map_df(c(0:10,12:19), 
                             begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius0.01/", 
                             efficiencies_per_slice, 
                             turns = 1000, 
                             N = 100, 
                             slice = 100)
# Turn it into a tibble
effs_n100_b35_r001_df <- tibble(
  run = rep(c(0:18),each=10), 
  turn = rep(seq(from = 100, to = 1000, 100), 19),
  global = effs_n100_b35_r001$global, 
  local = effs_n100_b35_r001$local, 
  mean_dist = effs_n100_b35_r001$md,
  cs = effs_n100_b35_r001$cs,
  beta = 3.5, 
  radius = 0.01, 
  population = 100, 
)

# Radius 0.001
effs_n100_b35_r0001 <- map_df(c(0:19), 
                              begin ="/Users/nrestrepo/Downloads/beta_alll/beta3_final/beta3.5/monkeys100/radius0.001/", 
                              efficiencies_per_slice, 
                              turns = 1000, 
                              N = 100, 
                              slice = 100)
# Turn it into a tibble
effs_n100_b35_r0001_df <- tibble(
  run = rep(c(0:19),each=10), 
  turn = rep(seq(from = 100, to = 1000, 100), 20),
  global = effs_n100_b35_r0001$global, 
  local = effs_n100_b35_r0001$local, 
  mean_dist = effs_n100_b35_r0001$md,
  cs = effs_n100_b35_r0001$cs,
  beta = 3.5, 
  radius = 0.001, 
  population = 100, 
)

#### Beta 4.5 - 100 Monkeys ####
# Radius 1
effs_n100_b45_r1 <- map_df(c(0:7), 
                           begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius1/", 
                           efficiencies_per_slice, 
                           turns = 1000, 
                           N = 100, 
                           slice = 100)
# Turn it into a tibble
effs_n100_b45_r1_df <- tibble(
  run = rep(c(0:7),each=10), 
  turn = rep(seq(from = 100, to = 1000, 100), 8),
  global = effs_n100_b45_r1$global, 
  local = effs_n100_b45_r1$local, 
  mean_dist = effs_n100_b45_r1$md,
  cs = effs_n100_b45_r1$cs,
  beta = 4.5, 
  radius = 1, 
  population = 100, 
)

# Radius 0.1
effs_n100_b45_r01 <- map_df(c(0:7), 
                            begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius0.1/", 
                            efficiencies_per_slice, 
                            turns = 1000, 
                            N = 100, 
                            slice = 100)
# Turn it into a tibble
effs_n100_b45_r01_df <- tibble(
  run = rep(c(0:7), each=10), 
  turn = rep(seq(from = 100, to = 1000, 100), 8),
  global = effs_n100_b45_r01$global, 
  local = effs_n100_b45_r01$local, 
  mean_dist = effs_n100_b45_r01$md,
  cs = effs_n100_b45_r01$cs,
  beta = 4.5, 
  radius = 0.1, 
  population = 100, 
)

# Radius 0.01
effs_n100_b45_r001 <- map_df(c(0:7), 
                             begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius0.01/", 
                             efficiencies_per_slice, 
                             turns = 1000, 
                             N = 100, 
                             slice = 100)
# Turn it into a tibble
effs_n100_b45_r001_df <- tibble(
  run = rep(c(0:7), each = 10), 
  turn = rep(seq(from = 100, to = 1000, 100), 8),
  global = effs_n100_b45_r001$global, 
  local = effs_n100_b45_r001$local, 
  mean_dist = effs_n100_b45_r001$md,
  cs = effs_n100_b45_r001$cs,
  beta = 4.5, 
  radius = 0.01, 
  population = 100, 
)

# Radius 0.001
effs_n100_b45_r0001 <- map_df(c(0:7), 
                              begin ="/Users/nrestrepo/Downloads/beta_alll/beta4_final/beta4.5/monkeys100/radius0.001/", 
                              efficiencies_per_slice, 
                              turns = 1000, 
                              N = 100, 
                              slice = 100)
# Turn it into a tibble
effs_n100_b45_r0001_df <- tibble(
  run = rep(c(0:7),each =10), 
  turn = rep(seq(from = 100, to = 1000, 100),8), 
  global = effs_n100_b45_r0001$global, 
  local = effs_n100_b45_r0001$local, 
  mean_dist = effs_n100_b45_r0001$md,
  cs = effs_n100_b45_r0001$cs,
  beta = 4.5, 
  radius = 0.001, 
  population = 100, 
)

#### Plot ####

complete_effs <- rbind(effs_n100_b15_r1_df,
                       effs_n100_b15_r01_df, 
                       effs_n100_b15_r001_df, 
                       effs_n100_b15_r0001_df,
  effs_n100_b25_r1_df, 
                       effs_n100_b25_r01_df,
                       effs_n100_b25_r001_df, 
                       effs_n100_b25_r0001_df, 
                       effs_n100_b35_r1_df, 
                       effs_n100_b35_r01_df, 
                       effs_n100_b35_r001_df, 
                       effs_n100_b35_r0001_df, 
  effs_n100_b45_r1_df, 
  effs_n100_b45_r01_df, 
  effs_n100_b45_r001_df, 
  effs_n100_b45_r0001_df)

summed_effs <- complete_effs %>% 
  group_by(population, 
           beta, 
           radius, 
           turn) %>% 
  summarise(avg_global = mean(global), 
            sd = sd(global), 
            lower = avg_global - 1.96*sd, 
            upper = avg_global + 1.96*sd) %>% 
  mutate(population = as.factor(population))

summed_effs <- summed_effs %>% 
  mutate(radius_rl = recode(radius, 
                            `1` = "1", 
                            `0.1` = "0.1", 
                            `0.01` = "0.01", 
                            `0.001` = "0.001"))

#summed_effs$radius_rl[which(is.na(summed_effs$radius_rl))] <- "Point-to-point"

cum_glob <- summed_effs %>% 
  ggplot(aes(x = turn, 
             y = avg_global)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() + 
  facet_grid(beta ~ radius) + 
  labs(title = "Cumulative Global Efficiency", 
       y = "Global Efficiency", 
       x = "Turn", 
       caption = "Population 100") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.text  = element_text(size = 10))

ggsave("~/Documents/InformationTransmission/networks/global_1000.svg",
       cum_glob, 
       height = 8, 
       width = 10)

summed_effs_loc <- complete_effs %>% 
  group_by(population, 
           beta, 
           radius, 
           turn) %>% 
  summarise(avg_local = mean(local), 
            sd = sd(local), 
            lower = avg_local - 1.96*sd, 
            upper = avg_local + 1.96*sd) %>% 
  mutate(population = as.factor(population))

summed_effs_loc <- summed_effs_loc %>% 
  mutate(radius_rl = recode(radius, 
                            `1` = "1", 
                            `0.1` = "0.1", 
                            `0.01` = "0.01", 
                            `0.001` = "0.001"))

summed_effs_loc$radius_rl[which(is.na(summed_effs_loc$radius_rl))] <- "Point-to-point"



cum_loc <- summed_effs_loc %>% 
  ggplot(aes(x = turn, 
             y = avg_local)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(beta ~ radius) + 
  labs(title = "Cumulative Local Efficiency", 
       y = "Local Efficiency", 
       x = "Turn", 
       caption = "Population 100") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.text  = element_text(size = 10))  

ggsave("~/Documents/InformationTransmission/networks/local_1000.svg",
       cum_loc, 
       height = 8, 
       width = 10)

summed_cs <- complete_effs %>% 
  group_by(population, 
           beta, 
           radius, 
           turn) %>% 
  summarise(avg_cs = mean(cs), 
            sd = sd(cs), 
            lower = avg_cs - 1.96*sd, 
            upper = avg_cs + 1.96*sd) %>% 
  mutate(population = as.factor(population))

summed_cs <- summed_cs %>% 
  mutate(radius_rl = recode(radius, 
                            `1` = "1", 
                            `0.1` = "0.1", 
                            `0.01` = "0.01", 
                            `0.001` = "0.001"))

cum_comp <- summed_cs %>% 
  ggplot(aes(x = turn, 
             y = avg_cs)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(beta ~ radius) + 
  labs(title = "Components across simulation", 
       y = "Number of Components", 
       x = "Turn", 
       caption = "Population 100") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.text  = element_text(size = 10))  

ggsave("~/Documents/InformationTransmission/networks/comps_1000.svg",
       cum_comp, 
       height = 8, 
       width = 10)
#### Matrices per slide ####
matrix_per_slice <- function(slice, 
                             begin, 
                             run, 
                             turns, 
                             N) {
  
  path <- paste0(begin, run, '.txt')
  # Data for just one run
  tm_data <- read.delim(path,
                        header = F, sep = ',')
  # Let's get board coordinates for the each time step 
  # Create function to extract a matrix of coordinates for each time-step
  get_coordinates <- function(x, num_agents) {
    tm <- tm_data[x,-ncol(tm_data)] %>% as_vector() %>% matrix(nrow = num_agents, ncol = 4, byrow = T)
    colnames(tm) <- c('monkey', 'x_coord', 'y_coord', 'fruit')
    return(tm)
  }
  # Iterate over all time-steps
  full_coord_list <- map(c(1:turns), get_coordinates, num_agents=N)
  # Now I am going to create a function to extract network matrices 
  get_network_matrix <- function(x) {
    # extract dataset for the coordinates of one time-step 
    coord <- full_coord_list[[x]] %>% as.data.frame(.)
    # Empty placeholder matrix
    net_mat <- matrix(0, N, N)  
    for (j in 1:N) {
      # Coordinates for where agent j is 
      mcx <- coord$x_coord[j] 
      mcy <- coord$y_coord[j]
      # Are there any monkeys there 
      coincide <- which(coord$x_coord==mcx & coord$y_coord==mcy)
      # If there are record on the network matrix
      for (i in 1:length(coincide)) {
        net_mat[j,coincide[[i]]] <- 1
      }
    }
    return(net_mat)
  }
  
  # Let's get all networks 
  full_matrix_list <- map(c(1:turns), get_network_matrix)
  
  # How many slices? 
  
  sections <- seq(0, 1000, slice)
  sections <- sections[-1]
  
  get_efficiencies_per_section <- function(x) {
    filtered_mats <- full_matrix_list[1:x]
    full_matrix <- Reduce("+", filtered_mats)
    # Get rid of the diagonal because it's meaningless 
    diag(full_matrix) <- 0
    return(full_matrix)
  }
  matrix_list <- map(sections, get_efficiencies_per_section)
  
  for (i in 1:10) {
    mat <- matrix_list[[i]]
    write.table(mat, 
                paste0(begin, "matrix", "_", run, "_", "slice", i, ".txt"), 
                sep = ",")
    
  }
}

#### Beta 1.5 - Monkey 100 ####
# Radius 1
int_n100_b15_r1 <- map(c(0), 
                       begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta1.5/monkeys100/radius1/", 
                       matrix_per_slice, 
                       turns = 1000, 
                       N = 100)
# Radius 0.1
int_n100_b15_r01 <- map(c(0), 
                        begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta1.5/monkeys100/radius0.1/", 
                        matrix_per_slice, 
                        turns = 1000, 
                        N = 100)

# Radius 0.01 
int_n100_b15_r001 <- map(c(0), 
                         begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta1.5/monkeys100/radius0.01/", 
                         matrix_per_slice, 
                         turns = 1000, 
                         N = 100)

# Radius 0.001 
int_n100_b15_r0001 <- map(c(0), 
                          begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta1.5/monkeys100/radius0.001/", 
                          matrix_per_slice, 
                          turns = 1000, 
                          N = 100)

#### Beta 2.5 - Monkey 100 ####
# Radius 1
int_n100_b25_r1 <- map(c(0), 
                       begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta2.5/monkeys100/radius1/", 
                       matrix_per_slice, 
                       turns = 1000, 
                       N = 100)

# Radius 0.1
int_n100_b25_r01 <- map(c(0), 
                        begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta2.5/monkeys100/radius0.1/", 
                        matrix_per_slice, 
                        turns = 1000, 
                        N = 100)

# Radius 0.01 
int_n100_b25_r001 <- map(c(0), 
                         begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta2.5/monkeys100/radius0.01/", 
                         matrix_per_slice, 
                         turns = 1000, 
                         N = 100)

# Radius 0.001 
int_n100_b25_r0001 <- map(c(0), 
                          begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta2.5/monkeys100/radius0.001/", 
                          matrix_per_slice, 
                          turns = 1000, 
                          N = 100)

#### Beta 3.5 - Monkey 100 ####
# Radius 1
int_n100_b35_r1 <- map(c(0), 
                       begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta3.5/monkeys100/radius1/", 
                       matrix_per_slice, 
                       turns = 1000, 
                       N = 100)

# Radius 0.1
int_n100_b35_r01 <- map(c(0), 
                        begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta3.5/monkeys100/radius0.1/", 
                        matrix_per_slice, 
                        turns = 1000, 
                        N = 100)


# Radius 0.01 
int_n100_b35_r001 <- map(c(0), 
                         begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta3.5/monkeys100/radius0.01/", 
                         matrix_per_slice, 
                         turns = 1000, 
                         N = 100)

# Radius 0.001 
int_n100_b35_r0001 <- map(c(0), 
                          begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta3.5/monkeys100/radius0.001/", 
                          matrix_per_slice, 
                          turns = 1000, 
                          N = 100)


#### Beta 4.5 - Monkey 100 ####
# Radius 1
int_n100_b45_r1 <- map(c(0), 
                       begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta4.5/monkeys100/radius1/", 
                       matrix_per_slice, 
                       turns = 1000, 
                       N = 100)

# Radius 0.1
int_n100_b45_r01 <- map(c(0), 
                        begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta4.5/monkeys100/radius0.1/", 
                        matrix_per_slice, 
                        turns = 1000, 
                        N = 100)
# Radius 0.01 
int_n100_b45_r001 <- map(c(0), 
                         begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta4.5/monkeys100/radius0.01/", 
                         matrix_per_slice, 
                         turns = 1000, 
                         N = 100)

# Radius 0.001 
int_n100_b45_r0001 <- map(c(0), 
                          begin ="/Users/nrestrepo/Downloads/revision_test_1000/beta4.5/monkeys100/radius0.001/", 
                          matrix_per_slice, 
                          turns = 1000, 
                          N = 100)
