library(rsconnect)

cmdargs = commandArgs(trailingOnly=T)

effect = cmdargs[1];     						   
demographic = cmdargs[2];     						          
NonEuro = unlist(strsplit(cmdargs[3], ","));    
cohort_size = cmdargs[4];                      
cols = unlist(strsplit(cmdargs[5], ","));       
sample_size = cmdargs[6];                      
CI_LBB = cmdargs[7];                           
CI_UBB = cmdargs[8];                           
Area = cmdargs[9];                                 
Trait = gsub(","," ",cmdargs[10]);                           
snp = cmdargs[11]; 							   
min_val=as.numeric(cmdargs[12]);						    
max_val=as.numeric(cmdargs[13]);						   
demographic_annot=gsub(","," ",cmdargs[14]);
save.image("data.RData")

#rsconnect::setAccountInfo(name='bagari',token='11E2F5102E117E56080B9B76E526DCFF',secret='l2jkO5LHgn2MAaOd5CSaZkpoMYH0sDTfBx0GrUxQ')
rsconnect::setAccountInfo(name='brainescience', token='F5A48617BC4A4AD5A4441E5114E88671', secret='vtME8xSUimBI1a92uXI0t2M03Kb/ZejKTSEKQklh')
rsconnect::deployApp(appFiles=c('app.R', 'data.csv', 'data.RData')) 
rsconnect::deployApp()
