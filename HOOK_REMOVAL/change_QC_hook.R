################################################################################
## This program is set to save time when performing visual control for CTS4 
## to remove DOXY hook in the profile  
##
## Catherine Schmechtig 20250729
##
## Input : Bfile from EDAC
## Output : Bfile with DOXY_QC=4 on the hook 
##
## 1- Estimate the hook for the last 70 dbars  
## 2- Set QC=4 when the value is not within +- 0.2 compare to the previous value 
## 
## This needs to be checked with a stringent visual control !!!!!
#################################################################################
library(ncdf4)
library(stringr)


#### Creating the list of files for which we need to recompute  
liste_to_do=read.table("liste_all_B",header=FALSE, as.is=TRUE)

# List of the file to process
LIST_nc=liste_to_do$V1

for (IDnc in LIST_nc) {
#########################
# Open the nc File
#########################

# Open the B file
        filenc_B=nc_open(IDnc,readunlim=FALSE,write=TRUE)

############################################################
## work on index 
#############################################################
###  Getting DOXY

        DOXY=ncvar_get(filenc_B,"DOXY")
        
        DOXY_QC=ncvar_get(filenc_B,"DOXY_QC")
        
        PRES=ncvar_get(filenc_B,"PRES")


        if (length(DOXY[!is.na(DOXY)])==0) {

                next # jump to the next profile if no chla in the profile

        }

#### Get the list of parameters in the profile

        STATION_PARAMETERS=ncvar_get(filenc_B,"STATION_PARAMETERS")
# Stations parameters has a fixed length 64 characters 

        DOXY_STRING=str_pad("DOXY",64,"right")

# Find the profile containing DOXY  

        index_doxy=which(STATION_PARAMETERS == DOXY_STRING, arr.ind=TRUE)
        
        if (length(index_doxy)==0) {

                next # jump to the next profile if no doxy in the profile

        }
 
 	print(nrow(index_doxy))

 
         for ( irow in seq(1,nrow(index_doxy))) {

		print("nprof")
		
                iprof_doxy = index_doxy[irow,2]

                print(iprof_doxy)
                
                if ( (nrow(index_doxy)==1) &  (iprof_doxy == 1)) {
                
                	PRES_DOXY=PRES
                	
                	DOXY_DOXY=DOXY
                
                } else {
                
			PRES_DOXY=PRES[,iprof_doxy]
	
			DOXY_DOXY=DOXY[,iprof_doxy]
			
		}
	
		TEST_DOXY=DOXY_DOXY
		
		STATUS=rep(1,length(PRES_DOXY))
		
		for (i in seq(2,length(PRES_DOXY))) {
	
			TEST_DOXY[i]=DOXY_DOXY[i]-DOXY_DOXY[i-1]
			
			if (!is.na(TEST_DOXY[i])) {
			
				if ( TEST_DOXY[i] < -0.2 ) STATUS[i] = 0
			 
				if ( TEST_DOXY[i] >  0.2 ) STATUS[i] = 2
			}

		}

        	PRES_LIM=max(PRES_DOXY,na.rm=TRUE)-70 
        
######################################################################################
####    changing DOXY_QC (to confirm) 
####
####    We change the QC of the hook 
####      -with a loop from the bottom of the profile 
####      -considering a difference of absolute magnitude of 0.2 in the concentration 
####      -if the trend stops then we are no longer in the hook (value and sign)
####      -Till 70 dbars above the max pressure 
#### 
####    => Still needed to check and adapt the QC with SCOOP
#######################################################################################

        	N_QC=nchar(DOXY_QC[iprof_doxy])

        	QC_test=unlist(strsplit(DOXY_QC[iprof_doxy],split="")) # to keep 4 set by GL

        	index_qc=which(!is.na(DOXY_DOXY))

  		i_qc<-length(index_qc)
  		
  		j=index_qc[i_qc]
  		 
  		STATUS_STOCK = STATUS[j] 
  		
  		print(PRES_DOXY[j])
  		print(PRES_LIM)
  		print(STATUS_STOCK)
  		print(STATUS[j])

		while ( ( PRES_DOXY[j] > PRES_LIM ) & (STATUS_STOCK == STATUS[j]) &  (PRES_LIM > 0) ){

			if ( ( STATUS[j] == 0 ) | ( STATUS[j] == 2 ) ) {
				
				substr(DOXY_QC[iprof_doxy],j,j)<-as.character(4)
				
				STATUS_STOCK = STATUS[j] 
			
			}
			
			i_qc <- i_qc-1
			
			j = index_qc[i_qc]
			 
		} 

	}    
###########################################
# Write in the working files 
###########################################

        ncvar_put(filenc_B,"DOXY_QC",DOXY_QC)

        nc_close(filenc_B)

}# end loop on B files  

