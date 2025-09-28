library(grid)
set.seed(11111)


size <- 100


mat <- matrix(runif(size*size,0,1), nrow = size, ncol = size)
colmat <- matrix(gray(mat), nrow = nrow(mat), ncol = ncol(mat))
r <- as.raster(colmat)



grid.newpage()
grid.raster(r, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)

const <- 1
start <- 1
end <- size - const


for(k in 1:1000){

for(i in start:end){
  for(j in start:end){
    point_1 <- i
    point_2 <- i + 1
    point_3 <- j
    point_4 <- j + 1
    
    avg <- mean(mat[point_1:point_2,point_3:point_4])
    
    
  
    
    if(avg < 0.4){
      mat[point_1:point_2,point_3:point_4] <- mean(mat[point_1:point_2,point_3:point_4])*0.1 + 
                                              mat[point_1:point_2,point_3:point_4]*0.9 -
                                              0.01
    }
    
    else if(avg < 0.45){
      mat[point_1:point_2,point_3:point_4] <- mean(mat[point_1:point_2,point_3:point_4])*0.1 + 
        mat[point_1:point_2,point_3:point_4]*0.9 -
        0.007
    }
    
    else if(avg < 0.5){
      mat[point_1:point_2,point_3:point_4] <- mean(mat[point_1:point_2,point_3:point_4])*0.1 + 
        mat[point_1:point_2,point_3:point_4]*0.9 -
        0.005
    }
    
    else if(avg > 0.6){
      mat[point_1:point_2,point_3:point_4] <- mean(mat[point_1:point_2,point_3:point_4])*0.1 + 
        mat[point_1:point_2,point_3:point_4]*0.9 +
        0.01
    }
    
    else if(avg > 0.55){
      mat[point_1:point_2,point_3:point_4] <- mean(mat[point_1:point_2,point_3:point_4])*0.1 + 
        mat[point_1:point_2,point_3:point_4]*0.9 +
        0.007
    }
    
    else{
      mat[point_1:point_2,point_3:point_4] <- mean(mat[point_1:point_2,point_3:point_4])*0.1 + 
        mat[point_1:point_2,point_3:point_4]*0.9 +
        0.005
    } 
    
    
  }
}

mat <- mat + matrix(rnorm(size*size,0,0.04), nrow = 100, ncol = 100)
mat <- pmin(mat,1)
mat <- pmax(mat,0)

colmat <- matrix(gray(mat), nrow = nrow(mat), ncol = ncol(mat))
r <- as.raster(colmat)


grid.newpage()
grid.raster(r, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)

}




png("grayscale_100x100.png", width = 1000, height = 1000, res = 100)
grid.raster(r, interpolate = FALSE)
dev.off()



















##################################


size <- 70


R <- matrix(runif(size*size,0,1), nrow = size, ncol = size)
G <- matrix(runif(size*size,0,1), nrow = size, ncol = size)
B <- matrix(runif(size*size,0,1), nrow = size, ncol = size)


colmat <- rgb(R, G, B)
colmat <- matrix(colmat, nrow = size, ncol = size)

r <- as.raster(colmat)

grid.newpage()
grid.raster(r, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)





const <- 1
start <- 1
end <- size - const


for(k in 1:200){
  #RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
  for(i in start:end){
    for(j in start:end){
      point_1 <- i
      point_2 <- i + 1
      point_3 <- j
      point_4 <- j + 1
      
      avg <- mean(R[point_1:point_2,point_3:point_4])
      
      
      
      
      if(avg < 0.4){
        R[point_1:point_2,point_3:point_4] <- mean(R[point_1:point_2,point_3:point_4])*0.1 + 
          R[point_1:point_2,point_3:point_4]*0.9 -
          0.01
      }
      
      else if(avg < 0.45){
        R[point_1:point_2,point_3:point_4] <- mean(R[point_1:point_2,point_3:point_4])*0.1 + 
          R[point_1:point_2,point_3:point_4]*0.9 -
          0.007
      }
      
      else if(avg < 0.5){
        R[point_1:point_2,point_3:point_4] <- mean(R[point_1:point_2,point_3:point_4])*0.1 + 
          R[point_1:point_2,point_3:point_4]*0.9 -
          0.005
      }
      
      else if(avg > 0.6){
        R[point_1:point_2,point_3:point_4] <- mean(R[point_1:point_2,point_3:point_4])*0.1 + 
          R[point_1:point_2,point_3:point_4]*0.9 +
          0.01
      }
      
      else if(avg > 0.55){
        R[point_1:point_2,point_3:point_4] <- mean(R[point_1:point_2,point_3:point_4])*0.1 + 
          R[point_1:point_2,point_3:point_4]*0.9 +
          0.007
      }
      
      else{
        R[point_1:point_2,point_3:point_4] <- mean(R[point_1:point_2,point_3:point_4])*0.1 + 
          R[point_1:point_2,point_3:point_4]*0.9 +
          0.005
      } 
    }
  }
  

  
  for(i in start:end){
    for(j in start:end){
      point_1 <- i
      point_2 <- i + 1
      point_3 <- j
      point_4 <- j + 1
      
      avg <- mean(G[point_1:point_2,point_3:point_4])
      
      
      
      
      if(avg < 0.4){
        G[point_1:point_2,point_3:point_4] <- mean(G[point_1:point_2,point_3:point_4])*0.1 + 
          G[point_1:point_2,point_3:point_4]*0.9 -
          0.01
      }
      
      else if(avg < 0.45){
        G[point_1:point_2,point_3:point_4] <- mean(G[point_1:point_2,point_3:point_4])*0.1 + 
          G[point_1:point_2,point_3:point_4]*0.9 -
          0.007
      }
      
      else if(avg < 0.5){
        G[point_1:point_2,point_3:point_4] <- mean(G[point_1:point_2,point_3:point_4])*0.1 + 
          G[point_1:point_2,point_3:point_4]*0.9 -
          0.005
      }
      
      else if(avg > 0.6){
        G[point_1:point_2,point_3:point_4] <- mean(G[point_1:point_2,point_3:point_4])*0.1 + 
          G[point_1:point_2,point_3:point_4]*0.9 +
          0.01
      }
      
      else if(avg > 0.55){
        G[point_1:point_2,point_3:point_4] <- mean(G[point_1:point_2,point_3:point_4])*0.1 + 
          G[point_1:point_2,point_3:point_4]*0.9 +
          0.007
      }
      
      else{
        G[point_1:point_2,point_3:point_4] <- mean(G[point_1:point_2,point_3:point_4])*0.1 + 
         G[point_1:point_2,point_3:point_4]*0.9 +
          0.005
      } 
    }
  }

  
  for(i in start:end){
    for(j in start:end){
      point_1 <- i
      point_2 <- i + 1
      point_3 <- j
      point_4 <- j + 1
      
      avg <- mean(B[point_1:point_2,point_3:point_4])
      
      
      
      
      if(avg < 0.4){
        B[point_1:point_2,point_3:point_4] <- mean(B[point_1:point_2,point_3:point_4])*0.1 + 
          B[point_1:point_2,point_3:point_4]*0.9 -
          0.01
      }
      
      else if(avg < 0.45){
        B[point_1:point_2,point_3:point_4] <- mean(B[point_1:point_2,point_3:point_4])*0.1 + 
          B[point_1:point_2,point_3:point_4]*0.9 -
          0.007
      }
      
      else if(avg < 0.5){
        B[point_1:point_2,point_3:point_4] <- mean(B[point_1:point_2,point_3:point_4])*0.1 + 
          B[point_1:point_2,point_3:point_4]*0.9 -
          0.005
      }
      
      else if(avg > 0.6){
        B[point_1:point_2,point_3:point_4] <- mean(B[point_1:point_2,point_3:point_4])*0.1 + 
          B[point_1:point_2,point_3:point_4]*0.9 +
          0.01
      }
      
      else if(avg > 0.55){
        B[point_1:point_2,point_3:point_4] <- mean(B[point_1:point_2,point_3:point_4])*0.1 + 
          B[point_1:point_2,point_3:point_4]*0.9 +
          0.007
      }
      
      else{
        B[point_1:point_2,point_3:point_4] <- mean(B[point_1:point_2,point_3:point_4])*0.1 + 
          B[point_1:point_2,point_3:point_4]*0.9 +
          0.005
      } 
    }
  }
  
  
  
R <- R + matrix(rnorm(size*size,0,0.04), nrow = size, ncol = size)
R <- pmin(R,1)
R <- pmax(R,0)

  
G <- G + matrix(rnorm(size*size,0,0.04), nrow = size, ncol = size)
G <- pmin(G,1)
G <- pmax(G,0)  

  
B <- B + matrix(rnorm(size*size,0,0.04), nrow = size, ncol = size)
B <- pmin(B,1)
B <- pmax(B,0)



colmat <- rgb(R, G, B)
colmat <- matrix(colmat, nrow = size, ncol = size)
  
r <- as.raster(colmat)
  
grid.newpage()
grid.raster(r, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)
    
    
  
  
  
  
  
}










