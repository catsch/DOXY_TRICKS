################################################################################
## This program is set to gain time when performing visual control for CTS4 
## to remove DOXY in air measurements in the profile  
##
## Catherine Schmechtig 20250625
##
## Input : Bfile from EDAC
## Output : Bfile with DOXY_QC=4 at surface 
##
## 1- Estimate the median in the first 2 dbars 
## 2- Set QC=4 when the value is not within +- 0.2 dbars around the median in the first 0.5dbars
## 
## This needs to be checked !!!!!
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
        
        iprof_doxy=index_doxy[,2]

        if (length(index_doxy)==0) {

                next # jump to the next profile if no chla in the profile

        }
                
	PRES_DOXY=PRES[,iprof_doxy]
	
	DOXY_DOXY=DOXY[,iprof_doxy]
        
        med_doxy=median(DOXY_DOXY[(PRES_DOXY<2.5)],na.rm=TRUE)
        
        med_doxy_min=med_doxy-0.2 
        
        med_doxy_max=med_doxy+0.2

###############################################################
####    changing the NPQ_QC in CHLA_ADJUSTED_QC (to confirm) 
###############################################################

        N_QC=nchar(DOXY_QC[iprof_doxy])

        QC_test=unlist(strsplit(DOXY_QC[iprof_doxy],split="")) # to keep 4 set by GL

        index_qc=which(!is.na(DOXY_DOXY))

        for (i_qc in seq(1,length(index_qc))) {

                j=index_qc[i_qc] # to avoid _ filled_values 

                if ( (DOXY_DOXY[j] < med_doxy_min | DOXY_DOXY[j] > med_doxy_max) & (PRES_DOXY[j] < 0.5 ) ) {

                        substr(DOXY_QC[iprof_doxy],j,j)<-as.character(4)

                }

        }
###########################################
# Write in the working files 
###########################################

        ncvar_put(filenc_B,"DOXY_QC",DOXY_QC)

        nc_close(filenc_B)

}# end loop on B files  

