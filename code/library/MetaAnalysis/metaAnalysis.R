library(readxl)
library(metafor)
library(tools)
library(gridExtra)
library(grid)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

pred_length = 100
cex_lab_value = 0.8 # magnification of x and y labels relative to cex
cex_main_value = 0.8 # magnification of titles relative to cex
las_value = 1
pch_value = 19
size_min_value = 0.5
size_ratio_value = 0.5
var1 = 'EFFECT'
#var2 = 'Age' # variable to test for association'


# read.csv, csv file
# read.delim, txt file                             
# read_excel, xls/xlsx file
FileReadFunc = function(path) {
  if(file_ext(path) == 'txt')
    data = read.delim(path)
  if(file_ext(path) == 'csv')
    data = read.csv(path)
  if(file_ext(path) == 'xls' || file_ext(path) == 'xlsx')
    data = read_excel(path)
  return(data)
}


#Commenting out removemissingfunc and filtermerg func
RemoveMissingFunc = function(data, desiredCols) {
  #Filter out the rows which have NAs in desired Cols
  completeVec = complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


FilterMergFunc = function(data, desiredCols) {
  missingVec = is.na(data[, desiredCols])
  return(data[missingVec, ])
}


## Added 12/7/20
##Updated 5/18/21
RemoveNonEuroFunc = function(data){
  #Create a list
  NonEuro = c('GOBS', 'IMH', 'UNICAMP', 'Meth-CT', 'MIRECC', 'Meth-CT', 'MIRECC',
              'UKBB_NonEuropean', 'OSAKA', 'PING_NonEuropean', 'UKBB')
  #Keep the Study in data which are not in NonEuro
  return(data[!is.element(data$Study,NonEuro),])
}

      
DataStats = function(data) {
  # mean,median,25th and 75th quartiles,min,max
  print(summary(data))
  # Tukey min,lower-hinge, median,upper-hinge,max
  print(fivenum(data))
}

# define a function to adjust the digits of the statistics
digits = function(regression_object){
  if(abs(regression_object$beta[2])<0.001){
    round_decimal=5
  }else{round_decimal=2}
  return(round_decimal)
}

#Data cleaning
data = FileReadFunc(args[1])
#data = read.csv(args[1])
# Remove missing data in Effect and Age
#data = RemoveMissingFunc(data, c(var1, var2))
data = RemoveMissingFunc(data, c(var1))
# Remove mergeD cohorts based on column N
data = FilterMergFunc(data, 'N') 
# Remove Non European cohorts
data = RemoveNonEuroFunc(data)
# Sort data based on Age
#data = data[order(data$Age),]
data = data[order(data$Study),]


###convert data types and calculate N,SE,Z
cols=c("EFFECT","SAMPLE_SIZE","CI_LB","CI_UB","N","TOTAL_N","PCT")
data[cols]=lapply(data[cols],as.character)
data[cols]=lapply(data[cols],as.numeric)
# Extract features to run meta regression
# convert EFFECT SAMPLE_SIZE Age CI_LB CI_UB N TOTAL_N PCT to numeric
data$N = data$SAMPLE_SIZE
data$SE =(data$CI_UB - data$CI_LB)/(2*1.96)
data$Z = data$EFFECT/data$SE      


#### Generate meta regression plot (age_allele)
#res = rma.uni(yi=data$EFFECT, sei=data$SE, mods=data$Age, method="REML", control=list(stepadj=0.2, maxiter=1000), digits = 5)
res = rma.uni(yi=data$EFFECT, sei=data$SE, verbose = TRUE, method="FE", control=list(maxiter = 1000), digits = 5)
#preds = predict(res, newmods=seq(min(data$Age),max(data$Age),length=pred_length))
preds = predict(res)
size = size_min_value + size_ratio_value * (data$N - min(data$N))/(max(data$N) - min(data$N))

# convert the result into a dataframe
result=as.data.frame(preds)
#result$Age = seq(min(data$Age),max(data$Age),length=pred_length)      

# calculate the statistics
#round_decimal=digits(res)
round_decimal=2
                      
# Output p value
#print(c('p value: ', res$QMp))
print(c('p value: ', res$pval))
writeLines(as.character(res$pval), args[2])
#dev.off()
