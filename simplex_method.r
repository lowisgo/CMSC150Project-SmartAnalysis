SimplexMethod <- function(matx){
  #gets basic solution
  #1-7
  
  #continue until no negative on last row
  #flag = FALSE
  basic_solution = matrix()
  y = as.integer()
  while(TRUE){
    print(matx)
    
    #obtain the solution column
    solution_col = matrix(matx[,ncol(matx)])
    print(solution_col)
    
    #PRINT BASIC SOLUTION
    for(i in 1:7){
      cnt = 0
      #1-5
      for(j in 1:5){
        #print(matx[j,i])
        #check if active/inactive
        if(matx[j,i] != 0) {
          cnt = cnt + 1; 
          y = j
        }
      }
      
      #print(cnt)
      if(cnt != 1) {
        basic_solution[i] = 0
      }
      else {
        basic_solution[i] = solution_col[y]
      }
    }
    
    print(basic_solution)
    
    #access the last row
    colnames(matx) <- NULL
    lastrow = matrix (matx[nrow(matx),]);
    size = nrow(lastrow)-1
    
    #iterate on last row to find the highest magnitude negative number
    temp = as.double(999) 
    flag = FALSE #has no negative
    temp_idx = as.integer(1)  #holds the column number (pivot column)
    for(i in 1:size){
      #check if negative
      #print(lastrow)
      if(lastrow[i] < 0){
        flag = TRUE #has negative
        #if temp > 0, meaning no value yet
        if(temp > 0) {
          temp = lastrow[i]
          temp_idx = i
        }
        #else get the highest magnitude
        else {
          if(abs(temp) < abs(lastrow[i])){
            temp = lastrow[i]
            temp_idx = i
          }
        }
      }
    }
    
    if(flag == FALSE) break;
    
    
    #obtain the pivot column
    pivot_col = matrix(matx[,temp_idx])
    
    #select pivot element in pivot column
    nsize = nrow(pivot_col)-1
    min = as.integer(999)
    test_idx = as.integer()
    #get test ratio a/b
    for(i in 1:nsize){
      test_ratio = solution_col[i] / pivot_col[i]
      if(test_ratio > 0){
        if(i == 1){
          #check first if integer answer
          if(!is.infinite(test_ratio)){
            min = test_ratio
            test_idx = i
          }
        }else{
          #if integer, proceed to finding minimum
          if(!is.infinite(test_ratio)){
            if(test_ratio < min){
              min = test_ratio
              test_idx = i
            }
          }
        }
      }
    }
    
    #get the pivot row
    pivot_row = matrix (matx[test_idx,]);
    #get the pivot element
    
    pivot_element = matx[test_idx,temp_idx]
    
    #normalize the pivot row
    for(i in 1:nrow(pivot_row)){
      matx[test_idx,i] = pivot_row[i] / pivot_element;
      pivot_row[i] = pivot_row[i] / pivot_element;
    }
    
    
   #Do gauss jordan
   #element in pivot column * PR
    for(i in 1:(nsize+1)){
      #forelem = matrix(matx[i,])
      for(j in 1:nrow(pivot_row)){
        #if pivot element, continue
        if(i != test_idx){
          answer = pivot_col[i] * pivot_row[j]
          result = matx[i,j] - answer
          matx[i,j] = result
         }
        }
    }
  }   
}


#Test cases
#accepts matrix as input
matx = matrix( 
  c(7, 10, 1, 0, -150, 11, 8, 0, 1, -175, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 77, 80, 9, 6, 0), # the data elements 
  nrow=5,              # number of rows 
  ncol=8,              # number of columns 
  byrow = FALSE)    
colnames(matx) <- c("x1", "x2", "s1","s2", "s3", "s4", "z", "sol")

#function call
answer = SimplexMethod(matx)
answer