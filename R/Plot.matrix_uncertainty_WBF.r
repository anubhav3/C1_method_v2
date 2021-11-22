#plots the predation matrix 


## takes a food web in matrix format and converts it to list format
Matrix.to.list_uncertainty <- function(web.matrix, predator.first=TRUE){
  if(length(dimnames(web.matrix)[[1]])==length(web.matrix[1,]))
    species.names <- dimnames(web.matrix)[[1]]
  else
    species.names <- 1:length(web.matrix[,1])
  summ <- dim(web.matrix)[1]^2
  web.list <- matrix(0, summ, 3)
  counter <- 1
  for(i in 1:length(web.matrix[,1]))
    for(j in 1:length(web.matrix[,1]))
      if(web.matrix[i,j]>=-1){
        web.list[counter,] <- c(species.names[i], species.names[j], web.matrix[i,j])
        counter <- counter + 1
      }
  if(!predator.first)
    web.list <- cbind(web.list[,2], web.list[,1])
  #print(web.list)
  web.list <- web.list[sort.list(as.numeric(web.list[,3]),),]
  web.list
}

## takes a food web in matrix format and coverts it to list format
Matrix.to.list <- function(web.matrix, predator.first=TRUE){
  if(length(dimnames(web.matrix)[[1]])==length(web.matrix[1,]))
    species.names <- dimnames(web.matrix)[[1]]
  else
    species.names <- 1:length(web.matrix[,1])
  web.list <- matrix(0, sum(web.matrix), 2)
  counter <- 1
  for(i in 1:length(web.matrix[,1]))
    for(j in 1:length(web.matrix[,1]))
      if(web.matrix[i,j]==1){
        web.list[counter,] <- c(species.names[i],species.names[j])
        counter <- counter + 1
      }
  if(!predator.first)
    web.list <- cbind(web.list[,2], web.list[,1])
  web.list
}

## For real food web matrix
Plot.matrix <- function(web, title=" ", point.cex=0.5, trait.cex=1,
                        diag.line=T, traits=F, by.consumer=T, axes.labels=F, sp.pt.ch=NA){
  
  
  S <- length(web[,1])
  
  
  dimnames(web) <- list(1:S, 1:S)
  consumer <- rep(1:S, each=S)
  resource <- rep(1:S, S)
  web.list <- Matrix.to.list(web)
  par(xpd=T)
  par(mar = c(8, 8, 10, 8))
  plot(consumer, resource, pch=19, type="n", cex=2,
       ann=F, axes=F,
       xlim=c(1, S), ylim=c(1, S),
       col = "black", bg = "red", lwd = 2)
  if(diag.line==T)
    lines(1:S, S:1, lty="dashed", col = "black", lwd = 4)
  
  if(length(traits)==1)
    points(web.list[,2], S+1-as.numeric(web.list[,1]),
           type="p", pch=21, cex=point.cex,
           col = "black", bg = "red", lwd = 4)

  mtext(side=3, text=title, line=6.2, cex=4.0, family="Times New Roman")
  if(axes.labels){
    mtext(side=2, "Resource", line=3.0, cex=3, family="Times New Roman")
    mtext(side=2, "<--- Increasing bodysize ---<", line=1.0, cex=1.5, family="Times New Roman")
    mtext(side=3, "Consumer", line=3.0, cex=3, family="Times New Roman")
    mtext(side=3, ">--- Increasing bodysize --->", line=1.0, cex=1.5, family="Times New Roman")
  }
  
}

Plot.matrix_uncertainty_real <- function(web, real_web, title = "", diag.line = T, axes.labels = T, nsim){

  h_col <- c("#f7f4f9", "#e7e1ef", "#d4b9da", "#c994c7", "#df65b0")
  n_col <- length(h_col)
  
  S <- length(web[,1])
  
  dimnames(web) <- list(1:S, 1:S)
  dimnames(real_web) <- list(1:S, 1:S)
  consumer <- rep(1:S, each=S)
  resource <- rep(1:S, S)
  web.list <- Matrix.to.list_uncertainty(web)
  
  web.list.real <- Matrix.to.list(real_web)
  
  # par(xpd=T)
  # par(mar = c(8, 8, 10, 8))
  # par(fig=c(0,3,0,4)/4)
  
  index <- as.numeric(web.list[,3])
  index <- ceiling(index/(nsim/(n_col))) + 1
  index[index == n_col + 1] = n_col
  labels_prop <- c("0.0 - 0.2" , "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1.0")
  
  dd_pred <- data.frame(consumer = as.numeric(web.list[,2]), resource = S+1-as.numeric(web.list[,1]), 
                        prop = as.factor(index))
  dd_pred_real <- data.frame(consumer = as.numeric(web.list.real[,2]), resource = S+1-as.numeric(web.list.real[,1]), Legend = "Links in observed foodweb")
  # dd_image <- data.frame(consumer = c(-1, 0, S), resource = c(1, S+2, S+2),
  #                       image = c("https://media.istockphoto.com/vectors/the-bluefin-tuna-isolated-on-the-white-background-vector-id940716938",
  #                                        "https://previews.123rf.com/images/destinacigdem/destinacigdem1512/destinacigdem151200065/50568632-green-bacteria-isolated-on-white-background.jpg",
  #                                 "https://media.istockphoto.com/vectors/the-bluefin-tuna-isolated-on-the-white-background-vector-id940716938"))
  g1 <- ggplot(dd_pred) +
    geom_segment(aes(x = 1, y = S, xend = S, yend = 1), lty = "dashed") +
    geom_point(aes(x = consumer, y = resource, col = prop), size = 10) +
    # geom_image(data = dd_image, aes(x = consumer, y = resource, image = image), size = 0.1) +
    scale_color_manual(values = h_col, labels = labels_prop, name = "Occurrence proportion") +
    theme_classic() +
    theme(axis.line=element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
          plot.margin = margin(1,1,1,1, "cm"), text = element_text(family="Times New Roman", size = 30)) +
    scale_x_discrete(position = "top") +
    xlab(expression(paste("Consumer"))) +
    ylab(expression(paste("Resource"))) +
    geom_point(data = dd_pred_real, aes(x = consumer, y = resource, shape = Legend), size = 5) +
    coord_cartesian(clip = "off") +
    labs(tag = "(a)") +
    theme(plot.tag.position = c(0, 1))
  
  return(g1)
}






Plot.matrix_AplusB <- function(webA = webA, webB = webB, diag.line=T, traits=F){
  
  S <- length(webA[,1])
  webAplusB <- webA + webB
  
  dimnames(webA) <- list(1:S, 1:S)
  dimnames(webB) <- list(1:S, 1:S)
  consumer <- rep(1:S, each=S)
  resource <- rep(1:S, S)
  
  webA.list <- Matrix.to.list_uncertainty(webA)
  webB.list <- Matrix.to.list_uncertainty(webB)
  webAplusB.list <- Matrix.to.list_uncertainty(webAplusB)
  
  pch_size <- 19
  cex_size <- 0.3
  
  h_col <- c("#edf8fb", "#2ca25f", "#006d2c")
  n_col <- length(h_col)
  nsim <- 2

  #Plotting matrix A
  index_A <- as.numeric(webA.list[,3]) + 1
  dd_A <- data.frame(consumer = as.numeric(webA.list[,2]), resource = S+1-as.numeric(webA.list[,1]), occ = as.factor(index_A))
  
  #Plotting matrix B
  index_B <- as.numeric(webB.list[,3]) + 1
  dd_B <- data.frame(consumer = as.numeric(webB.list[,2]) + 2*S, resource = S+1-as.numeric(webB.list[,1]), occ = as.factor(index_B))
  
  
  #Plotting matrix AplusB

  
  index_AplusB <- as.numeric(webAplusB.list[,3]) + 1
  dd_AplusB <- data.frame(consumer = as.numeric(webAplusB.list[,2]) + 4*S, resource = S+1-as.numeric(webAplusB.list[,1]), occ = as.factor(index_AplusB))
  
  labels_prop <- c("1" = "0" , "2" = "1", "3" = "2")
  psize <- 2
  plot_AplusB <-  ggplot() +
    geom_segment(aes(x = 1, y = S, xend = S, yend = 1), lty = "dashed") +
    geom_segment(aes(x = S+S/4, y = S/2, xend = 2*S-S/4, yend = S/2)) +
    geom_segment(aes(x = S+S/2, y = S/4, xend = S+S/2, yend = 3*S/4)) +
    geom_segment(aes(x = 2*S+1, y = S, xend = 3*S, yend = 1), lty = "dashed") +
    geom_segment(aes(x = 3*S+S/4, y = 2*S/6 , xend = 4*S-S/4, yend = 2*S/6)) +
    geom_segment(aes(x = 3*S+S/4, y = 4*S/6, xend = 4*S-S/4, yend = 4*S/6)) +
    geom_segment(aes(x = 4*S+1, y = S, xend = 5*S, yend = 1), lty = "dashed") +
    geom_point(data = dd_A, aes(x = consumer, y = resource, col = occ), size = psize) +
    geom_point(data = dd_B, aes(x = consumer, y = resource, col = occ), size = psize) +
    geom_point(data = dd_AplusB, aes(x = consumer, y = resource, col = occ), size = psize) +
    scale_color_manual(values = h_col, labels = labels_prop, name = expression(paste("Number of occurrences \n (out of 2)"))) +
    theme_classic() +
    theme(axis.line=element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(3,.8,3,1.8, "cm"), text = element_text(size = 30, family="Times New Roman")) +
    labs(tag = "(c)") +
    theme(plot.tag.position = c(-0.025, 1))
  
  return(plot_AplusB)
}


Plot_dist_occ <- function(web){
  
  bins_seq <- seq(1,1000, 37)
  occurence <- data.frame(occ = as.vector(web[web>0]))
  zeros <- sum(web == 0)
  zeros <- data.frame(x0 = 0, xend = 0, y0 = 0, yend = zeros, legend = "Number of absences of links in the summed predation matrix")
  n_zeros_1 <- zeros$yend*1.10
  plot_dist <- ggplot(occurence) +
    geom_histogram(aes(x = occ), breaks  = bins_seq) +
    geom_segment(data = zeros, aes(x = x0, xend = xend, y = y0, yend = yend, color = legend), size = 5) +
    theme_classic() +
    xlab("Number of occurences") +
    ylab("Frequency") +
    theme(text = element_text(size=30, family="Times New Roman")) +
    # labs(title = "Occurence frequency") +
    theme(plot.title = element_text(size = 30, hjust = 0.5),
          plot.margin = margin(1, 3, 0, 3, "cm"),
          legend.position= c(0.8, 0.9), legend.title = element_blank()) +
    coord_cartesian(clip = "off") +
    labs(tag = "(b)") +
    theme(plot.tag.position = c(-0.05, 1)) +
    geom_text(label = sum(web == 0), x = 0, y = zeros$yend*1.05, size = 7)
  
  return(plot_dist)
}


  