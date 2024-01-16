#libraries and enviroment 
import os
import pandas as pd
from src.functions import *

# os.chdir("C:/Archivos/datos/bea/")
# os.chdir("C:/Users/mateo/Documents/data/bea")

#datos de contienntes
continents = pd.read_csv("data/bea/Countries-Continents.csv")


file_path_all_parent_2dig = "results/bea/all_parent/all_parent_2dig.csv"
file_path_all_parent_4dig = "results/bea/all_parent/all_parent_4dig.csv"
file_path_maj_own = "results/bea/majority_owned_nonbank/data_majority_owned_nonbank.csv"

if (not os.path.exists(file_path_all_parent_2dig)) or  (not os.path.exists(file_path_all_parent_4dig)) or  (not os.path.exists(file_path_maj_own))  :
    #all_parents
    path = "data/bea/all_parent"

    data_2dig =create_data(path, type= 'all_parent')
    data_4dig =create_data(path, type= 'all_parent', desagregation = "4dig")

    data_2dig = calculos(data_2dig, type= 'all_parent')
    data_4dig = calculos(data_4dig, type= 'all_parent')

    data_2dig.to_csv(file_path_all_parent_2dig)
    data_4dig.to_csv(file_path_all_parent_4dig)


    #mayority_owned
    path = "data/bea/majority_owned_nonbank"

    data=create_data(path, type= 'majority_owned')
    data = calculos(data,type= 'majority_owned')

    data = data.merge( continents, how= "left", left_on="country", right_on="Country"   ).drop("Country", axis=1)
    data.to_csv(file_path_maj_own, index=False)
else:
    print("################ Files already exists ################ ")
    data = pd.read_csv(file_path_maj_own)

# print(data.loc[ data['Continent'].isnull() , ['country', 'Continent']].drop_duplicates() )