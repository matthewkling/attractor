



###########

library(doParallel)
library(tidyverse)


for(i in 3:25){
      
      message(i)
      
      attractors <- c(1, 3, 5)
      powers <- c(-10^c(10, -1, -1.5, -2, -2.5, -10),  
                  10^c(-2.5, -2.15, -2, -1.9, -1.83, -1.78, -1.7, -1.6, -1.5, -1.38, -.85, 10))
      
      p <- data.frame(x=runif(sum(attractors),0,100), 
                      y=runif(sum(attractors),0,100),
                      a=lapply(attractors, function(x) rep(x, x)) %>% unlist())
      
      xs <- seq(0,100,.01)
      ys <- seq(0, 100, 1)
      
      nc <- detectCores() - 1
      clust <- makeCluster(nc)
      registerDoParallel(clust)
      
      export <- c("attractors", "powers", "p", "xs")
      
      d <- foreach(y = ys, .packages="dplyr", .combine="rbind",
                   .noexport=setdiff(ls(), export)) %dopar% {
      
            dy <- expand.grid(x=xs,
                             y=y,
                             e=powers,
                             a=attractors) %>%
                  as.matrix()
            
            dy <- apply(dy, 1, function(z){
                  
                  x <- z[1]
                  y <- z[2]
                  e <- z[3]
                  a <- z[4]
                  
                  # distance from point to each attractor
                  dx <- x - p$x[p$a==a]
                  dy <- y - p$y[p$a==a]
                  d <- sqrt(dx^2 + dy^2)
                  
                  x2 <- x - (dx / (e*d))
                  y2 <- y - (dy / (e*d))
                  
                  x2 <- mean(x2)
                  y2 <- mean(y2)
                  
                  c(x2=x2, y2=y2)
            }) %>% 
                  t() %>%
                  cbind(dy, .) %>% 
                  as.data.frame()
            
            return(dy)
      }
      
      stopCluster(clust)
      
      saveRDS(list(p, d), paste0("e:/attractor/data_", i, ".rds"))
      
      eb <- element_blank()
      p <- ggplot(d %>% mutate(y2 = ifelse(e < 0, -y2, y2),
                               x2 = ifelse(e < 0, -x2, x2))) +
            geom_path(aes(x2, y2, group=y, color=y), 
                      size=.375,
                      linejoin="round", lineend="round") +
            scale_color_gradientn(colors=c("red", "darkgoldenrod1", "white")) +
            coord_fixed() +
            facet_wrap(~a+e, scales="free", nrow=3) +
            theme(axis.title=eb, 
                  axis.text=eb, 
                  axis.ticks=eb,
                  panel.grid=eb,
                  strip.text=eb, 
                  panel.background=element_rect(fill="black"),
                  plot.background=element_rect(fill="black", color="black"),
                  legend.position="none")
      
      ggsave(paste0("e:/attractor/x_",i,".png"), p, 
             height=5*length(unique(d$a)), width=5*length(unique(d$e)), 
             limitsize=F)
      
      p <- ggplot(d %>% mutate(y2 = ifelse(e < 0, -y2, y2),
                               x2 = ifelse(e < 0, -x2, x2))) +
            geom_path(aes(x2, y2, group=y, color=y), 
                      size=.375,
                      linejoin="round", lineend="round") +
            scale_color_gradientn(colors=c("red", "darkgoldenrod1", "white")) +
            coord_fixed() +
            facet_wrap(~e+a, scales="free", ncol=3) +
            theme(axis.title=eb, 
                  axis.text=eb, 
                  axis.ticks=eb,
                  panel.grid=eb,
                  strip.text=eb, 
                  panel.background=element_rect(fill="black"),
                  plot.background=element_rect(fill="black", color="black"),
                  legend.position="none")
      
      ggsave(paste0("e:/attractor/y_",i,".png"), p, 
             width=5*length(unique(d$a)), height=5*length(unique(d$e)), 
             limitsize=F)
}



