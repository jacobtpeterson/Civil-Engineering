GPMS <- function(county){
  
  #this is all for rain intensity info
  int=c(5,10,15,30,60)
  #this can be changed based on project location
  ##need to set up a veriable list
  rain=c(0.54,0.85,1.07,1.45,1.84)
  #hour equivilent value
  hreq=60/int*rain
  
  
  # myFile <-  "C:\\Users\\jpeterson\\Documents\\test_orig.csv" #file.choose()    #use this when troubleshooting
  myFile <-  file.choose()    #slect file dialog box
  myData  <- read.csv(myFile, header = TRUE,stringsAsFactors=FALSE)   #reads input data from selected file
  
  pave_c <- 0.9   #set C value for pavement
  roof_c <- 0.9   #set C value for rooftops
  grass_c <- 0.2  #set C value for grassland
  
  #adds up the areas given in the file
  myData$area <- myData$roof + myData$pave + myData$grass
  #calculates the weighted average of c for each area
  myData$c <- (myData$roof * roof_c)/myData$area + (myData$pave * pave_c)/myData$area + (myData$grass * grass_c)/myData$area
  #area *c
  myData$area_c <- myData$area * myData$c / 43560
  #mannings equation for lengths of pipe
  myData$flow_full <- (1.49/myData$mannings) * (myData$diameter/24) ^(2/3) * (myData$slope)^0.5
  #80% of full flow for safty
  myData$flow_design <- myData$flow_full * 0.8
  #total flow time within the pipe
  myData$flowtime_sect <- myData$length/myData$flow_design/60
  #if there is an inlet that isn't an outlet for another pipe, then the flow time into the pipe is set to 5
  myData$flowtime_in <- 5 * (!myData$inlet %in% myData$outlet)
  #calculates the total flow time at the pipe outlet (flow time into the pipe plus the flow time through the pipe)
  myData$flowtime_out <- myData$flowtime_in + myData$flowtime_sect
  
  #when a pipe has now attached area, the weighted average of c is INF, this set it to 0
  for (i in 1:length(myData$inlet)){
    
    if(myData$area[i]==0){
      myData$c[[i]]=0
      myData$area_c[[i]]=0
    }
    
  }
 
  
  
  # 1,000 iterations to account for pipes out of order.  This should catch any resonable istuation.  
  ## This should be changed out for an delta(X_1) check
 for (n in 1:1000){
 
  #for each pipe in a file...
  for (i in 1:length(myData$inlet)){
    #if a pipes inlet has the same name as the outlet of a nother pipe...
    if(myData$inlet[i] %in% myData$outlet){
      #the flow time into inlet "X" is equal to the flow time at the pipe with outlet "X" 
      #if there are more then one outlet with the name "X' then it chooses the maximum value
      #this is why we need to itterate.
      myData$flowtime_in[i] <- max(myData$flowtime_out[which(myData$inlet[i]==myData$outlet)])
      #this calculates the intensity (i) based on the flow time.  
      myData$int[[i]] <- approx(int, hreq, xout=myData$flowtime_out[[i]])[[2]]
      # myData$Qtot[[i]]=myData$Q[[i]]+sum(myData$Q[which(myData$inlet[i]==myData$outlet)])
      # print(i)
    } 
    #if a pipe inlet doesn't have the same name as the outlet of a nother pipe...
    else{
      #this calculates the intensity (i) based on the flow time. 
      myData$int[[i]] <- approx(int, hreq, xout=myData$flowtime_out[[i]])[[2]]
    }
    #calculates the total flow time at the pipe outlet (flow time into the pipe plus the flow time through the pipe)
    myData$flowtime_out <- myData$flowtime_in + myData$flowtime_sect
    #outputs data
    myData
    }
  }
  
  #calculates the surface water flowing into the pipe 
  myData$Q <- myData$int * myData$area_c
  
  i=1
  #for each pipe in the file
  for (i in 1:length(myData$inlet)){
    
    #the total flow into pipe inlet "X" is equal to the surface flow + the outflow of any pipe with outlet "X" 
    myData$Qtot[i]=myData$Q[i]+sum(myData$Qtot[which(myData$inlet[i]==myData$outlet)])
  }

  #date output
  myData

  }