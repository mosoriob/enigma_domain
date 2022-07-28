import os
import pandas as pd
import argparse
import sys

TRANSLATOR = str.maketrans({" ": "", ",": "", "[": "", "]": ""})

print("On python, Arguments are: ",sys.argv)
args = sys.argv
inputDir = args[1]
trait = args[2]
area = args[3]
snp = args[4]
demographic = args[5].replace(" ","").replace("(E)","")
min = float(args[6])
max = float(args[7])
demographic_values = args[8:-1]

#check if inputs are read correctly
print("input directory path:",inputDir)
print("trait: ",trait)
print("area: ",area)
print("snp: ",snp)
print("demographic: ",demographic)
print("min: ",min)
print("max:",max)
print("demographic values:",demographic_values)

input_list = os.listdir(inputDir)
demographic_list = []
for value in demographic_values:
    demographic_list.append(value.translate(TRANSLATOR))

print("age list: ", demographic_list)
print("Input list length: ", len(input_list))
print("Demographic list length: ", len(demographic_list))
assert len(input_list) == len(demographic_list), "Error. Check the mean age string."

merged_data = pd.DataFrame()

index = 0
for file_name in input_list:
    csv = pd.read_csv(inputDir+'/'+file_name)
    df = pd.DataFrame(csv)
    df.columns = df.columns.str.upper()
    #print(file_name)
    f1 = df.loc[(df["TRAIT"]==trait)&(df['AREA']==area)&(df['SNP']==snp)]
    demographic_value = float(demographic_list[index])
    if ((min == 0 or demographic_value > min) and (max == 0 or demographic_value <= max)):
        f1[demographic] = demographic_value
        merged_data = pd.concat([merged_data, f1],sort=False)
    index += 1

merged_data.rename(columns={'STUDY':'Study'}, inplace=True)

merged_data.sort_values(by=['Study'])
print("Merged CSV",merged_data.head())

merged_data.to_csv(path_or_buf=args[-1],index=False)
