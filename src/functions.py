# -*- coding: utf-8 -*-
"""
Created on Sun May  1 20:20:49 2022

@author: mateo
"""
import pandas as pd
import numpy as np
import os
import re 

# file = files[-1]
# df = pd.read_csv(path+"/"+file, skiprows = 5)


def preprocessing(x, var="unallocated", unit = "no_unit", desagregation = "2dig", type= 'all_parent'):
    if type== 'all_parent':
        x.rename(columns = {"Unnamed: 0": "sector"}, inplace = True)
        legend_index = x[x["sector" ] == "Legend / Footnotes:"].index.to_list()
        x = x.iloc[:legend_index[0]]
        x = x.melt(id_vars="sector", var_name="year")
        
        if desagregation == "2dig":
            x=x[~x["sector"].str.startswith("  ")]
        elif desagregation == "4dig":
            x=x[(x["sector"].str.startswith(" "))  & (~x["sector"].str.startswith("   "))]
            # x=x[~x["sector"].str.startswith("   ")]
            x=x[x["sector"]!= "  Other"]

        # variable adjustment
        x["value"] = x["value"].replace(to_replace =  '^[a-zA-Z0-9&_(*)\.-]+$', value= np.nan, regex= True).astype(float)
        x["year"] = x["year"].astype(int)
        x["variable"] =  var
        x["unit"] =  unit
        return x
    
    elif type== 'majority_owned':
        sectors = x.iloc[0,:].to_list()
        years = x.iloc[1,:].to_list()
        nombres = []
        for i,j in zip(sectors, years):
            nombres.append( i + j )
        x.columns = ["country"]+ nombres[1:]
        legend_index = x[x["country" ] == "Legend / Footnotes:"].index.to_list()
        x = x.iloc[2:legend_index[0]]
        x= x[x["country"]!= "Addenda:"]
        
        x = x.melt(id_vars="country", var_name="sector_year")
        
        # variable adjustment
        x["value"] = x["value"].replace(to_replace =  '^[a-zA-Z0-9&_(*)\.-]+$', value= np.nan, regex= True).astype(float)
        x["year"] = x["sector_year"].str.extract(r'(\d+[.\d]*)').astype(int)
        x["sector"] = x["sector_year"].str.replace(r'(\d+[.\d]*)','')
        x["variable"] =  var.lower()
        x["unit"] =  unit
        x["country"] = x["country"].str.strip()
        return x

def create_data( path, desagregation = "2dig",type= 'all_parent'):
    files = os.listdir(path)
    print(f"archivos concatenados de {type}:")
    dfs = []
    if type== 'all_parent':
        for file in files:
        
            if file == "employment.csv":
                unit = "thousands"
                skip = 6
            else:
                unit = "millions_usd"
                skip=5

        
            var_name = file.split(".")[0]
            
            df = pd.read_csv(path+"/"+file, skiprows = skip)
            df = preprocessing(df, var_name, unit, desagregation)
            
            dfs.append(df)
            print(file)
        print("fin de archivos")
    
        data = pd.concat(dfs, ignore_index=True)
        data = data.pivot(index = ["sector", "year"], columns= "variable", values = "value" )
        return data 
        
    elif type== 'majority_owned':
        for file in files:
            #revisamos que el dataset sea correcto
            source_check = pd.read_csv(path+"/"+file, nrows=1)
            if source_check.iloc[0].str.contains("All Majority-owned Foreign Affiliates").bool():
                print('concatenando {0}'.format(file), "(All Majority-owned Foreign Affiliates)")
            elif source_check.iloc[0].str.contains("Majority-owned Nonbank Foreign Affiliates").bool():
                print('concatenando {0}'.format(file), "(Majority-owned Nonbank Foreign Affiliates)") 
            else :
                print("Existe un dataframe que no corresponde: {0}".format(file))
                
                
            #revisamos la unidad de medida
            unit_check = pd.read_csv(path+"/"+file, nrows=3)
            if unit_check.iloc[2].str.contains("Millions of Dollars").bool():
                print(" En millones de USD" )
            elif unit_check.iloc[2].str.contains("Thousands of Employees").bool():
                print(" En miles de empleados")
            else:
                print(" Hay otra unidad de medida".upper())    

        
            var_name = re.search(r'download(.*?)[0-9]', file).group(1).strip().replace(" ", "_")
            
            df = pd.read_csv(path+"/"+file, skiprows = 5)
            df = preprocessing(x=df, var = var_name, type= 'majority_owned')
            print( " ultimo año", max(df.year))
            dfs.append(df)
        
        
        print("fin de archivos")
        
        data = pd.concat(dfs, ignore_index=True)
        data.drop(["sector_year", "unit"], axis=1, inplace=True)
        data = data[data["country"]!= "Other"] #buscar una mejor forma
        try:
            data = data.pivot(index = ["sector", "country", "year"], columns= "variable", values = "value" )
        except Exception as e:
            print(f"no se pudo reshapear, debido a {e}")
            
        return data.reset_index()
    


def calculos(x, type= 'all_parent'):
    if type== 'all_parent':
        x["employment"] = x.employment * 1000
        x["CI"] = x.total_sales - x.value_added
        # x["Kcca_1"] = x.total_assets - x.net_property_plant_and_equipment # Kcca_1 = inventarios
        # x["r"] = x.total_sales / x.Kcca_1 # x.net_income / x.Kcc # x.total_sales / x.Kcca 
        # x["Kcca_2"] = x.CI / x.r # Kcca_2 
        # x["Kva"] = x.compensation_employees / x.r 
        # x["TG"] = x.net_income / (x.net_property_plant_and_equipment + x.Kcca_1 + x.Kva )
        # x["TGsr"] = x.net_income / x.net_property_plant_and_equipment #TGstock
        x["TGasset"] = x.net_income / x.total_assets
        x["TGequity"] = x.net_income / x.owners_equity
        x["PT"] = x.value_added / x.employment
        x["Rem"] = (x.compensation_employees  / x.employment) / 12
        # x["COtec"] = x.net_property_plant_and_equipment /  x.employment
        # x["COv"] = x.net_property_plant_and_equipment /  x.compensation_employees
        x["TP"] =  x.net_income/ x.compensation_employees
        # x["TPr"] =  x.net_income/( x.compensation_employees/ x.r)
    
    elif type== 'majority_owned':
        x["employment"] = x.employment * 1000
        x["CI"] = x.total_sales - x.value_added
        x["Kcca_1"] = x.asset - x.net_property_plant_and_equipment # Kcca_1 = inventarios
        x["r"] = x.net_income / x.Kcca_1 # x.total_sales / x.Kcca 
        x["Kcca_2"] = x.CI / x.r # Kcca_2 
        x["Kva"] = x.compensation_employees / x.r 
        x["TG"] = x.net_income / (x.net_property_plant_and_equipment + x.Kcca_1 + x.Kva )
        x["TGsr"] = x.net_income / x.net_property_plant_and_equipment #TGstock
        x["TGasset"] = x.net_income / x.asset
        # x["TGequity"] = x.net_income / x.equity
        x["PT"] = x.value_added / x.employment
        x["Rem"] = (x.compensation_employees  / x.employment) / 12
        x["COtec"] = x.net_property_plant_and_equipment /  x.employment
        x["COv"] = x.net_property_plant_and_equipment /  x.compensation_employees
        x["TP"] =  x.net_income/ x.compensation_employees
        x["TPr"] =  x.net_income/( x.compensation_employees/ x.r)
    return x