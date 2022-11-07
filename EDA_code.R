# A function to read all csv files in the folder 
stock_dat<-function(file_path) # Function that takes the file path as input
{
  file_paths=fs::dir_ls(file_path) # Gets the file path of all files
  ccsv=list()
  for (i in seq_along(file_paths)) # iterating through the each file path 
  {
    ccsv[[i]]<-data.frame(read.csv(file=file_paths[i])) # putting the file contents in a list
  }
  return(ccsv)
  
}

# The below function takes in the stock data and expected change as inputs and gives out the,
# success, failure and no opportunity counts 

stock_stats=function(list_data_frame,u_change)
{
  First_Day_down=numeric(100)   # Initializing the arrays to store the number of success and failures
  Second_Day_down=numeric(100)
  Third_Day_down=numeric(100)
  First_Day_up=numeric(100)
  Second_Day_up=numeric(100)
  Third_Day_up=numeric(100)
  failure=numeric(100)
  no_opportunity=numeric(100)
  k=numeric(1)                
  
  for (i in 1:length(list_data_frame)){        # Iterating through each dataframe for individual stock
    Datee=as.Date(list_data_frame[[i]]$Date,format="%Y-%m-%d") # Getting the dates in each month
    Day=format(Datee,"%d")                     # Extracting only days from the date
    Day=as.integer(Day)-13                     # Converting the day type to integer and subtracting 13 
    ind=numeric(1)
    for (j in 1:(length(Day)-2)){              # Looping through the extracted (days -13)
      if ((Day[j]*Day[j+1])<=0){               # If this criteria is satisfied we get 13th 
        ind[1]<-j                              # Getting the index of 13th
      } else{                                  # If the condition is not satisfied the loop proceeds to next iteration
        next
      }
    }
    
    
    t1=((list_data_frame[[i]]$High[ind+1])-(list_data_frame[[i]]$High[ind]))/((list_data_frame[[i]]$Low[ind+1])-(list_data_frame[[i]]$Low[ind]));     # ((H2-H1)/(L2-L1)) criteria
    t2=((list_data_frame[[i]]$High[ind+3])-(list_data_frame[[i]]$High[ind+2]))/((list_data_frame[[i]]$Low[ind+3])-(list_data_frame[[i]]$Low[ind+2])); #  ((H4-H3)/(L4-L3)) criteria
    
    if ((t1>0 & t2>0 & is.nan(t1)==FALSE & is.nan(t2)==FALSE)) # Condition to check Gann's triangles formation
    {
      if (((list_data_frame[[i]]$Low[ind+4])<(list_data_frame[[i]]$Low[ind+3]))&((list_data_frame[[i]]$High[ind+4])<(list_data_frame[[i]]$High[ind+3]))) # Condition to check if trade is favourable in downward direction
      {                                                                                                                                     
        if (((-1*((list_data_frame[[i]]$Low[ind+5])-(list_data_frame[[i]]$Low[ind+3])))>=((u_change/100)*list_data_frame[[i]]$Low[ind+3])) || # Checking if the stock crossed u_change from L4 on 1st day
            ((-1*((list_data_frame[[i]]$Low[ind+6])-(list_data_frame[[i]]$Low[ind+3])))>=((u_change/100)*list_data_frame[[i]]$Low[ind+3])) || # Checking if the stock crossed u_change from L4 on 2nd day
            ((-1*((list_data_frame[[i]]$Low[ind+7])-(list_data_frame[[i]]$Low[ind+3])))>=(u_change/100)*list_data_frame[[i]]$Low[ind+3])) {   # Checking if the stock crossed u_change from L4 on 3rd day
          
          if (((-1*((list_data_frame[[i]]$Low[ind+5])-(list_data_frame[[i]]$Low[ind+3])))>=((u_change/100)*list_data_frame[[i]]$Low[ind+3]))==TRUE){
            First_Day_down=append(First_Day_down,1)                                                                                                 # Appending the array if the 1st day condition is satisfied
          }
          else if (((-1*((list_data_frame[[i]]$Low[ind+6])-(list_data_frame[[i]]$Low[ind+3])))>=((u_change/100)*list_data_frame[[i]]$Low[ind+3]))==TRUE){
            Second_Day_down=append(Second_Day_down,1)                                                                                                # Appending the array if the 2nd day condition is satisfied
            k=1
          } 
          else if (((-1*((list_data_frame[[i]]$Low[ind+7])-(list_data_frame[[i]]$Low[ind+3])))>=((u_change/100)*list_data_frame[[i]]$Low[ind+3]))==TRUE){
            Third_Day_down=append(Third_Day_down,1)                                                                                                  # Appending the array if the 3rd day condition is satisfied
          }
        } else{
          failure=append(failure,1)   # If none of the condition is satisfied then counting it as a failure
        }
      }
      else if (((list_data_frame[[i]]$Low[ind+4])>(list_data_frame[[i]]$Low[ind+3]))&((list_data_frame[[i]]$High[ind+4])>(list_data_frame[[i]]$High[ind+3]))) # Condition to check if trade is favourable in upward direction
      {
        if (((((list_data_frame[[i]]$High[ind+5])-(list_data_frame[[i]]$High[ind+3])))>=((u_change/100)*list_data_frame[[i]]$High[ind+3])) ||           # Checking if the stock crossed u_change from L4 on 1st day
            ((((list_data_frame[[i]]$High[ind+6])-(list_data_frame[[i]]$High[ind+3])))>=((u_change/100)*list_data_frame[[i]]$High[ind+3])) ||           # Checking if the stock crossed u_change from L4 on 2nd day
            ((((list_data_frame[[i]]$High[ind+7])-(list_data_frame[[i]]$High[ind+3])))>=(u_change/100)*list_data_frame[[i]]$High[ind+3])) {             # Checking if the stock crossed u_change from L4 on 3rd day
          if (((((list_data_frame[[i]]$High[ind+5])-(list_data_frame[[i]]$High[ind+3])))>=((u_change/100)*list_data_frame[[i]]$High[ind+3]))==TRUE){
            First_Day_up=append(First_Day_up,1)                                                                                                         # Appending the array if the 1st day condition is satisfied   
          }
          else if (((((list_data_frame[[i]]$High[ind+6])-(list_data_frame[[i]]$High[ind+3])))>=((u_change/100)*list_data_frame[[i]]$High[ind+3]))==TRUE){
            Second_Day_up=append(Second_Day_up,1)                                                                                                       # Appending the array if the 2nd day condition is satisfied
          } else if (((((list_data_frame[[i]]$High[ind+7])-(list_data_frame[[i]]$High[ind+3])))>=((u_change/100)*list_data_frame[[i]]$High[ind+3]))==TRUE){
            Third_Day_up=append(Third_Day_up,1)                                                                                                         # Appending the array if the 3rd day condition is satisfied        
          }
        } else{
          failure=append(failure,1)      # If none of the condition is satisfied then counting it as a failure
        }
      } else{
        no_opportunity=append(no_opportunity,1)  # If the criteria to get the direction is not satisfied then it is counted as no opportunity
      }
    } else {
      no_opportunity=append(no_opportunity,1)  # If the criteria to get the direction is not satisfied then it is counted as no opportunity
    }
  }
  sum_succ=(sum(First_Day_down)+sum(Second_Day_down)+sum(Third_Day_down)+   # Summing all the successes
              sum(First_Day_up)+sum(Second_Day_up)+sum(Third_Day_up))
  sum_failure=(sum(failure))                                            # Summing the failures
  sum_no=(sum(no_opportunity))                                          # Summing the no opportunity 
  
  f_success<<-sum(First_Day_up)+sum(First_Day_down)                     # Summing the first day success 
  s_success<<-sum(Second_Day_up)+sum(Second_Day_down)                   # Summing the second day success
  T_success<<-sum(Third_Day_up)+sum(Third_Day_down)                     # Summing the third day success
  return(c(sum_succ,sum_failure,sum_no))                                # Returning a vector of total success, failure and no opportunity for a stock
}