library(magrittr)
library(dplyr)
library(data.table)
library(R2OpenBUGS)

setwd("~/pipeline1222")
start = Sys.time()

fac2cha = function(x){
  if(!is.factor(x)) {
    warning("object is not factor")
    return(x)
  } else {
    return(levels(x)[x])
  }
}

andrew = function(data, name, player = T){
  if(player){
    out.p.combination <-
      strsplit(data$p.combination, ", ") %>%
      sapply(., function(x) {
        name %in% x
      }) %>%
      t %>%
      set_colnames(paste0("O_", name)) %>%
      plyr::mapvalues(., c(T, F), c(1L, 0L))
    out.p.com.allowed <-
      strsplit(data$p.com.allowed, ", ") %>%
      sapply(., function(x) {
        name %in% x
      }) %>%
      t %>%
      set_colnames(paste0("D_", name)) %>%
      plyr::mapvalues(., c(T, F), c(-1L, 0L))
    out.home <- fac2cha(data$home) %>% 
      plyr::mapvalues(., c("H", "A"), c(1L, 0L))
    out <- cbind(out.p.combination, 
                 out.p.com.allowed,
                 out.home)
  } else {
    out.off.team <- 
      sapply(data$team, function(x){
        name %in% x
      }) %>% t %>% 
      set_colnames(paste0("O_", name)) %>% 
      plyr::mapvalues(., c(T,F), c(1L, 0L))
    out.def.team <- 
      sapply(data$def.team, function(x){
        name %in% x
      }) %>% t %>% 
      set_colnames(paste0("D_", name)) %>% 
      plyr::mapvalues(., c(T, F), c(1L, 0L))
    out <- cbind(out.off.team, out.def.team)
    rownames(out) <- NULL
  }
  return(out)
}



all.play = read.table("find player new 2.0/all_play_corr.txt", header = T)
all.play$diff = all.play$home.score - all.play$away.score
season.index = read.table("find player new 2.0/season.index.txt",
                          header = F) %>% "["(,1)
train.index = read.table("~/pipeline1222/train_index.txt") %>% "["(,1)


p.var = read.table("p_var.txt")
p.var = data.frame(game_elapsed = rownames(p.var),
                    as.data.frame(p.var))

colnames(p.var) = gsub("X\\.","-",colnames(p.var)) %>% 
  gsub("X", "", .)

v.long0 = melt(p.var, id.vars = "game_elapsed")
colnames(v.long0)[2] = "diff"
v.long0$diff = fac2cha(v.long0$diff) %>%
  as.numeric()
v.long0$game_elapsed = fac2cha(v.long0$game_elapsed) %>% 
  as.numeric()

v.long0$value = plyr::mapvalues(v.long0$value,
                                NA, 0) %>% 
  plyr::mapvalues(.,0, max(.))
# make factor to character

all.play$p.com.allowed = fac2cha(all.play$p.com.allowed)
all.play$p.combination = fac2cha(all.play$p.combination)
tmp = cbind(fac2cha(all.play$team),
            fac2cha(all.play$home.team),
            fac2cha(all.play$away.team))
all.play$def.team = apply(tmp, 1, function(x){
  x[2:3][!(x[2:3] %in% x[1])]
})
#

#
all.play = dplyr::left_join(all.play, v.long0,
                       by = c("game.elapsed" = "game_elapsed",
                              "diff"))
colnames(all.play) = gsub("^value$", "variance", 
                          colnames(all.play))
#


# set range of data
player.threshold = 1000             
combi.threshold = 18
s.id = (season.index[2]+1):season.index[3]
train.id = (season.index[2]+1):(train.index[3]-1)
test.id = train.index[3]:season.index[3]
#

# find "player.enough"
season.play = all.play[s.id,]

season.off.player = season.play$p.combination %>% 
  as.character() %>% 
  strsplit(., ", ") %>% do.call(c, .)
season.off.player = season.off.player[season.off.player!=""] 

season.def.player = season.play$p.com.allowed  %>% 
  as.character() %>% 
  strsplit(., ", ") %>% do.call(c, .)
season.def.player = season.def.player[season.def.player!=""]

player.off.poss = table(season.off.player)
player.def.poss = table(season.def.player)

which(player.off.poss>=player.threshold) %>% length()
which(player.def.poss>=player.threshold) %>% length()


player.enough = 
  intersect(names(player.off.poss[which(player.off.poss >= player.threshold)]),
            names(player.def.poss[which(player.def.poss >= player.threshold)]))  

team = levels(all.play$team)
#


# model
X = andrew(all.play[train.id,], player.enough, player = T)
Y = andrew(all.play[train.id,], team, player = F)
P = 2*length(player.enough)
M = 2*length(all.play[train.id,]$team %>% levels())
tau = 1/all.play$variance[train.id]
N = length(train.id)
points = all.play$points[train.id]

mydata = list("X", "Y", "P", "M",
              "tau", "N", "points")

model = function(){
  for(i in 1:N){
    points[i] ~ dnorm(mu[i], tau[i])
    mu[i] <- b0 + inprod(X[i,], beta[]) +
      inprod(Y[i,], omega[])
  }
  
  for(p in 1:(P+1)){
    beta[p] ~ dnorm(0,.001)
  }
  
  for(m in 1:M){
    omega[m] ~ dnorm(0,.001)
  }
  
  b0 ~ dnorm(0, .001)

}
write.model(model, "BWLR.odc")
my.model.file = "BWLR.odc"

inits = function(){
  list(beta = rep(0, P+1),
       omega = rep(0, M),
       b0 = 0)
}

params = list("beta", "omega")
sampler = bugs(data = mydata, inits = inits, 
               parameters.to.save = params,
               model.file = my.model.file , codaPkg = TRUE,
               n.iter = 500, n.burnin = 100, n.thin = 10,
               n.chains = 2, debug =T)

BWLR.coda = read.bugs(sampler)
save(BWLR.coda, file = "coda_BWLR.rda")
#


Sys.time() - start

