winner_matrix=matrix(ncol=50,nrow=50)
att_troops_vector=c(1:50)
def_troops_vector=c(1:50)

for (mc1 in 1:50){
  for (mc2 in 1:50){
    
    winner=c()
    sim=100
    
    for (h in 1:sim){
      
      
      att_troops=att_troops_vector[mc1]
      def_troops=def_troops_vector[mc2]
      j=0
      while((att_troops>1)&(def_troops>0)){ 
        j=j+1
        att_one=FALSE
        def_one=FALSE
        if (att_troops>3){
          dice_att=sort(sample(size=3,c(1:6),replace=TRUE),decreasing = TRUE)
        } else if (att_troops==3){
          dice_att=sort(sample(size=2,c(1:6),replace=TRUE),decreasing = TRUE)
        } else if (att_troops==2){
          dice_att=sort(sample(size=1,c(1:6),replace=TRUE),decreasing = TRUE)
          att_one=TRUE
        }
        
        if (def_troops>1){
          dice_def=sort(sample(size=2,c(1:6),replace=TRUE),decreasing = TRUE)
        } else if (def_troops==1){
          dice_def=sort(sample(size=1,c(1:6),replace=TRUE),decreasing = TRUE)
          def_one=TRUE
        }
        
        
        if (dice_att[1]>dice_def[1]){
          def_troops=def_troops-1
        } else if (dice_att[1]<=dice_def[1]){
          att_troops=att_troops-1
        }
        
        if ((att_one==FALSE)&(def_one==FALSE)){
          
          if (dice_att[2]>dice_def[2]){
            def_troops=def_troops-1 
          } else if (dice_att[2]<=dice_def[2]){
            att_troops=att_troops-1
          }
          
        }
        
        
        
        
        
      }
      
      
      
      
      
      
      if (att_troops<=def_troops){
        winner[h]=0
      } else if (att_troops>def_troops){
        winner[h]=1
      } 
      
      troops_left_att[h]=att_troops
      troops_left_def[h]=def_troops
      
    }
    
    print(mc1)
    print(mc2)
    
    winner_matrix[mc1,mc2]=mean(winner)
    
    
    
  }
  
  
}


image(winner_matrix)