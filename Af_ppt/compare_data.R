
rm(list = ls())

m_data <- as.matrix(read.csv("driving_score_180ea.csv"))
s_data <- as.matrix(read.csv("virtual_answer.csv"))

loss <- c()

for( i in 1:nrow(m_data) ) {
  compl <- m_data[i, 1]
  accel <- m_data[i, 2]
  decel <- m_data[i, 3]
  result <- m_data[i, 4]
  
  c <- 1
  a <- 1
  d <- 1
  
  find_d <- FALSE
  
  ## Find compliance
  for( j in seq(1, 101, 25) ) {
    
    if( s_data[j, 1] == compl ) {
      c = j
      
      ## Find acceleration
      for( k in seq(c, c+20, 5)) {
        
        if( s_data[k, 2] == accel ) {
          a = k
          
          ## Find deceleration
          for( l in seq(a, a+4, 1)) {
            
            if( s_data[l, 3] == decel ) {
              d = l
              find_d <- TRUE
              break
            }
            if( l+1 >= nrow(s_data) ){
              d = 125
            }
          } ## End of d_for, var=l
        }
        
        if( find_d ){ break }
      } ## End of a_for, var = k
      
    }
    
    if( find_d ){ break }
  } ## End of c_for, var = j
  
  loss[i] = abs(m_data[i, 4] - s_data[d, 4])
} ## End of main for

comp_result <- cbind(as.data.frame(m_data), loss)
comp_result

write.csv(comp_result, "filter_data.csv")




