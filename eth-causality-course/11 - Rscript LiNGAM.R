require(pcalg)
require(igraph)
set.seed(145)

# helper function to plot graph from adjacency matrix
plot_adj_mat <- function(Adj, labels = 1:dim(Adj)[1]){
  Adj[Adj != 0] <- 1 
  gr <- graph.adjacency(Adj, mode = "directed", weighted = NULL, diag = FALSE)
  V(gr)$label <- labels 
  plot(gr)
}

### Linear SEM with uniform errors
n <- 1000

eps1 <- runif(n,-.5,.5)
eps2 <- runif(n,-.3,3)
eps3 <- runif(n,-.7,.7)
eps4 <- runif(n,-.5,.5)
eps5 <- runif(n,-1,1)

x1 <- eps1
x2 <- eps2
x3 <- x1 + x2 + eps3
x4 <- 2*x1 + eps4
x5 <- 3*x3 + eps5

data <- cbind(x1,x2,x3,x4,x5)
dataframe <- data.frame(data)

# apply LiNGAM
lingam.fit <- lingam(data)
round(lingam.fit$Bpruned,1)
plot_adj_mat(t(lingam.fit$Bpruned))
# LiNGAM found the exact causal structure, 
# also orienting the edge from X1 to X4

# Check stability under subsampling
ind.sub <- sample(1:n,n/2,replace=F)
data.sub <- data[ind.sub,]
lingam.fit2 <- lingam(data.sub)
round(lingam.fit2$Bpruned,1)
plot_adj_mat(t(lingam.fit2$Bpruned))
# result is stable

### What if the errors happen to be Gaussian?
set.seed(145)
n <- 1000

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,2)
eps3 <- rnorm(n,0,.7)
eps4 <- rnorm(n,0,1.5)
eps5 <- rnorm(n,0,1)

x1 <- eps1
x2 <- eps2
x3 <- x1 + x2 + eps3
x4 <- 2*x1 + eps4
x5 <- 3*x3 + eps5

data <- cbind(x1,x2,x3,x4,x5)
dataframe <- data.frame(data)

# apply LiNGAM (although errors are Gaussian)
lingam.fit <- lingam(data)
round(lingam.fit$Bpruned,1)
plot_adj_mat(t(lingam.fit$Bpruned))
# LiNGAM does not give the correct DAG

# check stability under subsampling:
ind.sub <- sample(1:n,n/2,replace=F)
data.sub <- data[ind.sub,]
lingam.fit2 <- lingam(data.sub)
round(lingam.fit2$Bpruned,1)
plot_adj_mat(t(lingam.fit2$Bpruned))
# not stable: warning sign!
