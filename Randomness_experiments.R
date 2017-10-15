


# random R probability tests


# This creates a loop to see how long on average it takes to create w many unique numbers out of a pool of l numbers
# it uses 100 experiments and then averages them 
# l cannot be less than w!!!!!
uniquenumbers = function(w,l){
  y = 0
  tries = c()
  while(y< 101){
    
    randonum = 0
    z = 0
    
    
    while(length(unique(randonum)) <10){
      randonum = sample(1:l,w,replace = TRUE )
      
      z= z+1
      
    }
    
    tries[y] = z
    y = y+1
    
  }
  return(mean(tries))
}



# This for loop runs the experiemt using the miniumin number pool size to a pool size that is 10 numbers larger
randomdata = c()
for (i in 1:10){
  # print(uniquenumbers(i+9,10))
  randomdata[i] = uniquenumbers(i+9,10)
}

randomdatagraph = plot(randomdata, type = "l")

#does not do much other than perform a bunch of mathematical calculations
(df[,3]**runif(1))*(df[,2] +4)



# testing to see if the random number generation of R is accurate
signficant = 0
insignificant = 0


for (t in 1:10000){
  randomatrix = matrix(runif(50), nrow = 5, ncol = 10)
  randomatrix2 = matrix(runif(50), nrow = 5, ncol = 10)
  
  ttest = t.test(randomatrix,randomatrix2)
  if (ttest$p.value<.05){
    signficant = signficant+1
  }
  else{
    
    insignificant = insignificant +1
  }
  
}

# gives percentage of signigicant similarites
signficant/10000
