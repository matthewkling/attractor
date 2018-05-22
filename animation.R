

library(doParallel)
library(tidyverse)


# define a smoothly interpolated sequence of frames
powers <- c(-10^c(5, 3, 1, 0, -.5, -.9, -1.2, -1.5, -1.75, -2, -2.3, -3),  
            10^c(-4, -2.5, -2.27, -2.18, -2.1, -2.05, -2, -1.96, -1.92, -1.885, -1.855, -1.825, -1.8, 
                 -1.775, -1.75, -1.725, -1.695, -1.66, -1.62, -1.57, -1.52, -1.43, -1.25, -1, -.5, 1, 3, 5))

pd <- data.frame(x=seq(0,1,length.out=length(powers)),
                 y=abs(powers)^.25 * sign(powers))
fit <- splinefun(x=pd$x, y=pd$y, method="hyman")
frames <- 300
pd <- data.frame(x=seq(0,1,length.out=frames))
pd$power <- fit(pd$x)
pd$power <- pd$power^4 * sign(pd$power)
pd$i <- 1:nrow(pd)

# view interpolation
ggplot() +
      geom_line(data=pd, 
                aes(x, abs(power)^.2*sign(power))) +
      geom_point(data=data.frame(x=seq(0,1,length.out=length(powers)), power=powers),
                 aes(x, abs(power)^.2*sign(power)))



attractors <- 3

# locations of attractor points -- either random or hard-coded
#p <- data.frame(x=runif(sum(attractors),0,100), 
#                y=runif(sum(attractors),0,100),
#               a=lapply(attractors, function(x) rep(x, x)) %>% unlist())
p <- data.frame(x=c(10.71981, 76.40360, 49.27411),
                y=100 - c(7.670871, 96.192075, 15.267391),
                a=3)

# grid
xs <- seq(0, 100, .01)
ys <- seq(0, 100, 1)

# cluster setup
nc <- detectCores() - 1
clust <- makeCluster(nc)
registerDoParallel(clust)
export <- c("attractors", "powers", "p", "xs", "ys", "pd")

d <- foreach(i=pd$i, .packages="tidyverse", .combine="rbind",
             .noexport=setdiff(ls(), export)) %dopar% {
                   
                   power <- pd$power[i]
                   
                   dy <- expand.grid(x=xs,
                                     y=ys,
                                     e=power,
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
                   
                   # plot
                   eb <- element_blank()
                   plt <- ggplot(dy %>% mutate(y2 = ifelse(e < 0, -y2, y2),
                                             x2 = ifelse(e < 0, -x2, x2))) +
                         geom_path(aes(x2, y2, group=y, color=y), 
                                   size=.375,
                                   linejoin="round", lineend="round") +
                         scale_color_gradientn(colors=c("red", "darkgoldenrod1", "white")) +
                         coord_fixed() +
                         theme(axis.title=eb, 
                               axis.text=eb, 
                               axis.ticks=eb,
                               panel.grid=eb,
                               strip.text=eb, 
                               panel.background=element_rect(fill="black"),
                               plot.background=element_rect(fill="black", color="black"),
                               legend.position="none")
                   
                   ggsave(paste0("e:/orb_weaver/frames/f", i,".png"), plt, height=6, width=6, bg="black")
}

stopCluster(clust)

# shell command to generate gif
# "magick -delay 6 $(for i in $(seq 1 1 150); do echo f${i}.png; done; for i in $(seq 150 -1 1); do echo f${i}.png; done) -loop 0 animated.gif"

