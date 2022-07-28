import os
import pandas as pd
import argparse
import sys

print("On python, Arguments are: ",sys.argv)
args = sys.argv
inputDir = args[1]
trait = args[2]
area = args[3]
snp = args[4]
#Comment

#check if inputs are read correctly
print("input directory path:",inputDir)
print("trait: ",trait)
print("area: ",area)
print("snp: ",snp)

input_list = os.listdir(inputDir)
print("Input list length: ", len(input_list))

merged_data = pd.DataFrame()

index = 0
for file_name in input_list:
    csv = pd.read_csv(inputDir+'/'+file_name)
    df = pd.DataFrame(csv)
    df.columns = df.columns.str.upper()
    #print(file_name)
    f1 = df.loc[(df["TRAIT"]==trait)&(df['AREA']==area)&(df['SNP']==snp)]
    merged_data = pd.concat([merged_data, f1],sort=False)
    index += 1

    
merged_data.rename(columns={'STUDY':'Study'}, inplace=True)
merged_data.sort_values(by=['Study'])
print("Merged CSV",merged_data.head())

merged_data.to_csv(path_or_buf=args[-1],index=False)
