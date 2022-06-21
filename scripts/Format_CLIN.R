args <- commandArgs(trailingOnly = TRUE)
input_dir <- args[1]
output_dir <- args[2]

source("https://raw.githubusercontent.com/BHKLAB-Pachyderm/ICB_Common/main/code/Get_Response.R")

clin = read.csv( file.path(input_dir, "CLIN.txt"), stringsAsFactors=FALSE , sep="\t" )
clin = cbind( clin[ , c( "PATIENT_ID","SEX","OS_MONTHS","OS_STATUS","DRUG_TYPE" ) ] , NA , NA , NA , NA )
colnames(clin) = c( "patient" , "sex" , "t.os" , "os" , "drug_type" , "stage" , "histo" , "dna" , "rna" )
rownames(clin) = clin$patient 

clin$os = ifelse(clin$os %in% "DECEASED" , 1 , 0)
clin$sex = ifelse(clin$sex %in% "Female" , "F" , "M")

response = read.csv( file.path(input_dir, "CLIN_response.txt"), stringsAsFactors=FALSE , sep="\t" , dec=',' )
rownames(response) = response$X

sample_info = read.csv( file.path(input_dir, "CLIN_sample.txt"), stringsAsFactors=FALSE , sep="\t" )
rownames(sample_info) = sample_info$SAMPLE_ID

info = cbind( sample_info[ , c("PATIENT_ID","SAMPLE_ID","PRIMARY_SITE","AGE_AT_SEQ_REPORT")] , NA , NA , NA , NA , NA ) 
colnames(info) = c( "patient","sample","primary" , "age" , "t.pfs" , "pfs" , "recist" , "response.other.info" , "response" )

response = response[response$X %in% rownames(info) , ]
info[rownames(response),]$t.pfs = response$PFS_months
info[rownames(response),]$pfs = response$PFS_events
info[rownames(response),]$recist = response[ , "Response.to.IO" ]

rownames(info) = info$patient
clinical = cbind( clin , info[ rownames(clin) , c("primary" , "age" , "t.pfs" , "pfs" , "recist" , "response.other.info" , "response" )] )
colnames(clinical) = c( colnames(clin) , c("primary" , "age" , "t.pfs" , "pfs" , "recist" , "response.other.info" , "response" ))

clinical$recist[ clinical$recist %in% "" ] = NA
clinical$response = Get_Response( data=clinical )

clinical$patient = sapply( clinical$patient , function(x){ paste( unlist( strsplit( as.character( x ) , "-" , fixed=TRUE )) , collapse=".") })

case = read.csv( file.path(output_dir, "cased_sequenced.csv"), stringsAsFactors=FALSE , sep=";" )
clinical$dna[ clinical$patient %in% case[ case$snv %in% 1 , ]$patient ] = "tgs"

clinical$primary[ clinical$primary %in% "Cancer of Unknown Primary" ] = "Unknown"
clinical$primary[ clinical$primary %in% "Cancer of Unknown primary" ] = "Unknown"
clinical$primary[ clinical$primary %in% "Cancer of unknown primary" ] = "Unknown"
clinical$primary[ clinical$primary %in% "Skin" ] = "Melanoma"
clinical$primary[ clinical$primary %in% "Bladder, Upper Tract" ] = "Bladder"
clinical$primary[ clinical$primary %in% "Ascending Colon" ] = "Colon"
clinical$primary[ clinical$primary %in% "base of tongue" ] = "HNC"
clinical$primary[ clinical$primary %in% "cecum" ] = "Colon"
clinical$primary[ clinical$primary %in% "Cecum" ] = "Colon"
clinical$primary[ clinical$primary %in% "Colorectal NOS" ] = "Colon"
clinical$primary[ clinical$primary %in% "Conjunctiva" ] = "Eye"
clinical$primary[ clinical$primary %in% "Descending Colon" ] = "Colon"
clinical$primary[ clinical$primary %in% "Esophagogastric Junction" ] = "Esophagus"
clinical$primary[ clinical$primary %in% "Ethmoid Sinus" ] = "HNC"
clinical$primary[ clinical$primary %in% "Foot" ] = "Melanoma"
clinical$primary[ clinical$primary %in% "Gastroesophageal" ] = "Esophagus"
clinical$primary[ clinical$primary %in% "Gastroesophageal Junction" ] = "Esophagus"
clinical$primary[ clinical$primary %in% "GE Junction" ] = "Esophagus"
clinical$primary[ clinical$primary %in% "Head and Neck" ] = "HNC"
clinical$primary[ clinical$primary %in% "HNSCC" ] = "HNC"
clinical$primary[ clinical$primary %in% "Hypopharynx" ] = "HNC"
clinical$primary[ clinical$primary %in% "Hypopharynx vs Oropharynx" ] = "HNC"
clinical$primary[ clinical$primary %in% "Kidney/Upper Tract" ] = "Kidney"
clinical$primary[ clinical$primary %in% "Larynx" ] = "HNC"
clinical$primary[ clinical$primary %in% "Leg" ] = "Melanoma"
clinical$primary[ clinical$primary %in% "Mandible" ] = "HNC"
clinical$primary[ clinical$primary %in% "Maxilla" ] = "HNC"
clinical$primary[ clinical$primary %in% "Maxillary Sinus" ] = "HNC"
clinical$primary[ clinical$primary %in% "Mouth" ] = "HNC"
clinical$primary[ clinical$primary %in% "Nailbed" ] = "Melanoma"
clinical$primary[ clinical$primary %in% "Nasal Cavity" ] = "HNC"
clinical$primary[ clinical$primary %in% "Nasopharynx" ] = "HNC"
clinical$primary[ clinical$primary %in% "Neck" ] = "HNC"
clinical$primary[ clinical$primary %in% "Not available" ] = "Unknown"
clinical$primary[ clinical$primary %in% "Oral Cavity" ] = "HNC"
clinical$primary[ clinical$primary %in% "Oropharynx" ] = "HNC"
clinical$primary[ clinical$primary %in% "Palate" ] = "HNC"
clinical$primary[ clinical$primary %in% "Parotid Gland" ] = "HNC"
clinical$primary[ clinical$primary %in% "Pharynx" ] = "HNC"
clinical$primary[ clinical$primary %in% "Rectosigmoid Colon" ] = "Colon"
clinical$primary[ clinical$primary %in% "Rectosimoid Colon" ] = "Colon"
clinical$primary[ clinical$primary %in% "Rectum" ] = "Colon"
clinical$primary[ clinical$primary %in% "Anal" ] = "Colon"
clinical$primary[ clinical$primary %in% "Anus" ] = "Colon"
clinical$primary[ clinical$primary %in% "Cervix" ] = "Ovary"
clinical$primary[ clinical$primary %in% "Renal Pelvis" ] = "Ureteral"
clinical$primary[ clinical$primary %in% "Retroperitenium" ] = "Other"
clinical$primary[ clinical$primary %in% "Sarcoma" ] = "Other"
clinical$primary[ clinical$primary %in% "Scalp" ] = "Melanoma"
clinical$primary[ clinical$primary %in% "Sigmoid Colon" ] = "Colon"
clinical$primary[ clinical$primary %in% "Sinonasal" ] = "HNC"
clinical$primary[ clinical$primary %in% "Soft Tissue" ] = "Other"
clinical$primary[ clinical$primary %in% "Subungual Toe" ] = "Melanoma"
clinical$primary[ clinical$primary %in% "Tongue" ] = "HNC"
clinical$primary[ clinical$primary %in% "Tonsil" ] = "HNC"
clinical$primary[ clinical$primary %in% "Trachea" ] = "HNC"
clinical$primary[ clinical$primary %in% "Transverse Colon" ] = "Colon"
clinical$primary[ clinical$primary %in% "Upper Tract" ] = "Ureteral"
clinical$primary[ clinical$primary %in% "Ureter" ] = "Ureteral"
clinical$primary[ clinical$primary %in% "Urethra" ] = "Ureteral"
clinical$primary[ clinical$primary %in% "Vagina" ] = "Ovary"
clinical$primary[ clinical$primary %in% "Vulva" ] = "Other"


clinical = clinical[ , c("patient" , "sex" , "age" , "primary" , "histo" , "stage" , "response.other.info" , "recist" , "response" , "drug_type" , "dna" , "rna" , "t.pfs" , "pfs" , "t.os" , "os" ) ]
clinical$drug_type[ clinical$drug_type %in% "PD-1/PDL-1" ] = "PD-1/PD-L1"

write.table( clinical , file=file.path(output_dir, "CLIN.csv") , quote=FALSE , sep=";" , col.names=TRUE , row.names=FALSE )
