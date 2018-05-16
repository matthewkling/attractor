
library(tidyverse)


for(i in 1:30){
      message(paste("iteration", i))
      
      attractors <- ceiling(i/10)
      #powers <- 10^seq(-4, 0, length.out=16)
      powers <- 10^c(-4, seq(-2.5, -1, length.out=17), -.5, 0)
      
      p <- data.frame(x=runif(attractors,0,100), 
                      y=runif(attractors,0,100))
      
      d <- expand.grid(x=seq(0,100,.01),
                       y=seq(0,100,1),
                       e=powers) %>%
            as.matrix()
      
      d <- apply(d, 1, function(z){
            #z=d[123,]
            x <- z[1]
            y <- z[2]
            e <- z[3]
            
            # distance from point to each attractor
            dx <- x - p$x
            dy <- y - p$y
            d <- sqrt(dx^2 + dy^2)
            
            x2 <- x - (dx / (e*d))
            y2 <- y - (dy / (e*d))
            
            x2 <- mean(x2)
            y2 <- mean(y2)
            
            c(x2=x2, y2=y2)
      }) %>% t() %>%
            cbind(d, .) %>% as.data.frame()
      
      
      eb <- element_blank()
      p <- ggplot(d) +
            geom_path(aes(x2, y2, group=y, color=y), 
                      #color="white", 
                      size=.375,
                      linejoin="round", lineend="round") +
            scale_color_gradientn(colors=c(sample(c("cyan", "darkgoldenrod1", "white", "white"), 1), "white")) +
            coord_fixed() +
            facet_wrap(~e, scales="free", nrow=4) +
            theme(axis.title=eb, 
                  axis.text=eb, 
                  axis.ticks=eb,
                  panel.grid=eb,
                  strip.text=eb, 
                  panel.background=element_rect(fill="black"),
                  plot.background=element_rect(fill="black", color="black"),
                  legend.position="none")
      ggsave(paste0("~/desktop/generation/p", i, ".png"), p, width=25, height=20)
}

p <- ggplot(d) +
      geom_path(aes(x2, y2, group=y, color=y), 
                #color="white", 
                size=.375,
                linejoin="round", lineend="round") +
      scale_color_gradientn(colors=c("red", "darkgoldenrod1", "white")) +
      coord_fixed() +
      facet_wrap(~e, scales="free", ncol=1) +
      theme(axis.title=eb, 
            axis.text=eb, 
            axis.ticks=eb,
            panel.grid=eb,
            strip.text=eb, 
            panel.background=element_rect(fill="black"),
            plot.background=element_rect(fill="black", color="black"),
            legend.position="none")
ggsave(paste0("~/desktop/generation/p", i, ".png"), p, width=5, height=5*20, limitsize=F)





###########

for(i in sample(1:1000, 30)){
      
      message(i)
      
      attractors <- c(1, 3, 5)
      #attractors <- 3
      powers <- 10^c(-6, -4, -3, -2.5, -2.3, -2.1, -2.05, -1.95, -1.85, -1.75, -1.65, -1.55, -1.35, -1.15, -1, 0)
      #powers <- powers[c(1, 4, 8, 12, 16)]
      powers <- c(-powers, powers)
      
      p <- data.frame(x=runif(sum(attractors),0,100), 
                      y=runif(sum(attractors),0,100),
                      a=lapply(attractors, function(x) rep(x, x)) %>% unlist())
      
      d <- expand.grid(x=seq(0,100,.1),
                       y=seq(0,100,1),
                       e=powers,
                       a=attractors) %>%
            as.matrix()
      
      d <- pbapply::pbapply(d, 1, function(z){
            
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
            cbind(d, .) %>% 
            as.data.frame()
      
      saveRDS(d, paste0("~/desktop/generation/data_", i, ".rds"))
      
      p <- ggplot(d %>% mutate(e = ifelse(e<0, e+3, e))) +
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
      ggsave(paste0("~/desktop/generation/x_",i,".png"), p, width=5*length(attractors), height=5*length(powers), limitsize=F)
      
}




for(file in list.files("~/desktop/generation", pattern="data_", full.names=T)){
      if(grepl("data_x", file)) next()
      
      d <- readRDS(file)
      i <- gsub("data_|\\.rds", "", basename(file))
      
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
      
      ggsave(paste0("~/desktop/generation/ey_",i,".png"), p, height=3*length(unique(d$a)), width=3*length(unique(d$e)), limitsize=F)
}




