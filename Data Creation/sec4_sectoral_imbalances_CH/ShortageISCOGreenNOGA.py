############################################
# Purpose: Use this script to process the  #
#          raw SAKE data.                  #
# Date:    03.06.2020                      #
# Authors: Matthias Niggli, CIEB/Uni Basel #
############################################

#%% LOAD LIBRARIES & SET DIRECTORIES
import numpy as np
import pandas as pd
import os

# (1) state the path to the "green potential" repository
repo_path = "xxxxxxxxxxxxxxxxxxxx" 

# (2) SAKE data is confidential. If you have access to it, set "SAKE_path" to where the raw data is stored.
SAKE_path = "xxxxxxxxxxxxxxxxx"
os.chdir(repo_path)
print("Libraries are loaded and directories are set.")

#%% LOAD SAKE DATA SUBSET TO VARIABLES OF INTEREST

def data_loading(year):
    df = pd.read_csv(SAKE_path+"SAKE"+year+".csv",sep=";",na_values=-9, keep_default_na = True)
    df["year"] = year
    return df

##### available sample weights in SAKE:
# IXHHH         =  "(TOT) Gewichtung Hochrechnung Haushalt - Jahresdaten"                                                                               
# IXPXH         =  "(TOT) Gewichtung Hochrechnung Zielperson - Quartalsdaten"                                                                           
# IXPXHJ        =  "(TOT) Gewichtung Hochrechnung Zielperson - Jahresdaten"                                                                             
# IXPXM1        =  "(TOT) Gewichtung Zielperson - Europäisches Modul (1/3 der Welle 1)"                                                                 
# IXPXM2        =  "(TOT) Gewichtung Zielperson - Nationales Modul (2/3 der Welle 1)" 
# => choose "IXPXHJ"-Variable

##### available region variables:
# EM06             "UV (E) Geografische Lage des Betriebs: Kanton"
# B030          =  "(TOT) Raumplanungsregionen (RPR)"
# B029          =  "(TOT) MS-Region (räumliche Mobilität) 2000"
# B027          =  "(TOT) Arbeitsmarktregion 2000"
# B023          =  "(TOT) Grossregion der Wohngemeinde der Zielperson (NUTS II)"
# => choose "B023" for analysis

#### Subset to "Erwerbstätige" (= "B000") and retrieve the following information:
# BFU5I            "UV (E, L, EL, N) Ausgeübter Beruf: klassiert nach Berufsstruktur ISCO-08"       
# EM03          =  "(E) Wirtschaftliche Tätigkeit des Betriebs: gemäss BUR (NOGA 2008)"
# B023          =  "(TOT) Grossregion der Wohngemeinde der Zielperson (NUTS II)"
# IXPXHJ        =  "(TOT) Gewichtung Hochrechnung Zielperson - Jahresdaten"                                                                             

def subSAKE_fun(df):
    df = df.loc[df["B0000"]==1, ["BFU5I", "EM03", "B023", "IXPXHJ", "year"]]
    df.dropna(inplace = True)
    df.rename(columns = {"BFU5I":"isco","EM03":"NOGA","B023":"Region", "IXPXHJ": "Gewicht","year": "year"}, inplace=True)
    return df

SAKE_dat = data_loading("2015")
SAKE_dat=subSAKE_fun(SAKE_dat)

for year in range(2016, 2020):
    df = data_loading(year = str(year))
    df = subSAKE_fun(df = df)
    SAKE_dat = pd.concat([SAKE_dat, df], axis = 0)
    
# summarize the data
print(SAKE_dat.head())
print("number of observations: ",len(SAKE_dat))
print("number of variables: ", len(SAKE_dat.columns))
print("available years: ", SAKE_dat.year.unique())

#%% MERGE WITH GREEN POTENTIAL

def load_isco():
    df=pd.read_csv(repo_path+"Data Creation/sec1&3_green_potential_shortages_isco/isco_list.csv",sep=";")
    df.rename(columns = {df.columns[1]: df.columns[1].replace(".","_")}, inplace=True)
    df.rename(columns = {df.columns[0]: "isco"}, inplace=True)
    df.drop(["Title"], axis = 1, inplace = True)
    return df

green = load_isco()
shortage = green.drop("green", axis = 1)
green = green.drop("index1", axis = 1)
green["green"] = [x.replace(",",".") for x in green["green"]]
green["green"] = green["green"].astype("float")

print("number of 4-digit ISCO occupations :", len(green))
SAKE_dat=SAKE_dat.merge(green,on="isco", how = "left")

def data_proc(NOGAdigit, df):

    # create NOGA x-digit variable:
    digit_name="NOGA"+str(NOGAdigit)+"digit"
    
    # add a "0" to those NOGAs that are not already 6-digit (use 8 letters because of formating as a float)
    df["NOGA"] = df["NOGA"].astype(str)
    df["Digits"] = df["NOGA"].apply(lambda x: len(x))
    
    tmp = pd.Series(df["Digits"].value_counts().index)
    tmp = tmp[tmp > 4]
    tmp = tmp.sort_values(ascending = False)
        
    for d in tmp[1:]:
        fill_zeros = 8-d
        df.loc[df["Digits"] == d, "NOGA" ] = ("0"*fill_zeros)+df["NOGA"]
        
    # check if all NOGAS are 8 digit now:
    df["Digits"] = df["NOGA"].apply(lambda x: len(x))
    if len(list(df["Digits"].value_counts().index)) > 2:
        return print("There is an error with NOGAs")

    df = df.drop(["Digits"], axis = 1)
    df[digit_name]=pd.Series(df["NOGA"].astype(str)).apply(lambda x: x[0:(NOGAdigit)])

    # set correct names for the Grossregion from codes:
    df["Region"]=df["Region"].astype("category")
    df=df.loc[df["Region"].astype(int).isin(np.arange(1,8))==True,:]
    df["Region"].cat.remove_unused_categories(inplace=True)
    Reg=list(pd.read_csv(repo_path+"Data Creation/sec4_sectoral_imbalances_CH/SAKEGrossregion.txt",
                         encoding="latin",header=None)[0].astype(str))
    df["Region"].cat.rename_categories(Reg,inplace=True)

    return df

SAKE_dat=data_proc(NOGAdigit = 2, df = SAKE_dat)
print(SAKE_dat.head())

#%% MERGE WITH SHORTAGE INDICATORS

def shortage_fun(df_NOGA):
    
    # convert index to floats:
    tmp = shortage["index1"].astype("str")
    tmp = [x.replace("(","") for x in tmp]
    tmp = [x.replace(")","") for x in tmp]
    tmp = [x.replace(",",".") for x in tmp]
    tmp = pd.Series(tmp).astype("float")
    shortage["index1"] = tmp
    shortage.rename(columns={"index1": "shortage_index"}, inplace=True)
    
    # normalize and store the index:
    tmp=shortage["shortage_index"]
    shortage["shortage_index_norm"]=tmp.map(lambda x: (x-tmp.min())/(tmp.max()-tmp.min()))

    # merge with SAKE/NOGA and return df
    df_NOGA=pd.merge(df_NOGA, shortage, on = "isco", how="left")
    return df_NOGA
    
SAKE_dat=shortage_fun(SAKE_dat)

# summarize final df
SAKE_dat.head()
len(SAKE_dat)

#%% WRITE CSV
#SAKE_dat.to_csv("xxxxxxxxxxx", sep=';')


