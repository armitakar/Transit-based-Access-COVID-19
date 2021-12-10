
### This code is used to average multiple isochrones generated with different departure times (hence, different waiting times).
#code collected and modified from: https://gis.stackexchange.com/questions/68359/creating-average-polygon



import rasterio
import numpy as np
from rasterio import Affine, features
from shapely.geometry import mapping, shape
from shapely.ops import cascaded_union
from math import floor, ceil, sqrt
import os



import geopandas as gp
import pandas as pd
import json


## Functions to average polygons

def average_polygon(shapes, path):  
    max_shape = cascaded_union([shape(s) for s in shapes])
    minx, miny, maxx, maxy = max_shape.bounds
    dx = dy = 10.0  # grid resolution; this can be adjusted
    lenx = dx * (ceil(maxx / dx) - floor(minx / dx))
    leny = dy * (ceil(maxy / dy) - floor(miny / dy))
    assert lenx % dx == 0.0
    assert leny % dy == 0.0
    nx = int(lenx / dx)
    ny = int(leny / dy)
    gt = Affine(
        dx, 0.0, dx * floor(minx / dx),
        0.0, -dy, dy * ceil(maxy / dy))
    pa = np.zeros((ny, nx), 'd')
    for s in shapes:
        r = features.rasterize([s], (ny, nx), transform=gt)
        pa[r > 0] += 1
    pa /= len(shapes)  # normalise values
    thresh = 0.5  # median
    pm = np.zeros(pa.shape, 'B')
    pm[pa > thresh] = 1
    poly_shapes = []
    for sh, val in features.shapes(pm, transform=gt):
        if val == 1:
            poly_shapes.append(shape(sh))
    if not any(poly_shapes):
        raise ValueError("could not find any shapes")
    avg_poly = cascaded_union(poly_shapes)
    # Simplify the polygon
    simp_poly = avg_poly.simplify(sqrt(dx**2 + dy**2))
    simp_shape = mapping(simp_poly)
    with open(path, 'w') as outfile:
      json.dump(mapping(simp_poly), outfile)




def read_shapes(filename):
    shapes = []
    for i in filename:
        with open(i) as f:
            print(i)
            a = json.load(f)
            shapes.append(a['features'][0]['geometry'])
    return shapes
def write_shapes(infile, outfile):
    with open(infile) as f:
        a = json.load(f)
    with open(outfile, 'w') as outfile:
            json.dump(a, outfile)


## Functions to generate, categorize, and store shapefiles
def categorize_file(filename, date):
    filename_category = [[],[],[],[],[],[],[],[],[],[],[],[]]
    for i in filename:
        if i[8:13] == date[0] and i[19:21] =='am' and i[22:26] == '1800':
            filename_category[0].append(i)
        if i[8:13] == date[0] and i[19:21] =='pm' and i[22:26] == '1800':
            filename_category[1].append(i)
        if i[8:13] == date[0] and i[19:21] =='am' and i[22:26] == '2700':
            filename_category[2].append(i)
        if i[8:13] == date[0] and i[19:21] =='pm' and i[22:26] == '2700':
            filename_category[3].append(i)
        if i[8:13] == date[1] and i[19:21] =='am' and i[22:26] == '1800':
            filename_category[4].append(i)
        if i[8:13] == date[1] and i[19:21] =='pm' and i[22:26] == '1800':
            filename_category[5].append(i)
        if i[8:13] == date[1] and i[19:21] =='am' and i[22:26] == '2700':
            filename_category[6].append(i)
        if i[8:13] == date[1] and i[19:21] =='pm' and i[22:26] == '2700':
            filename_category[7].append(i)
        if i[8:13] == date[2] and i[19:21] =='am' and i[22:26] == '1800':
            filename_category[8].append(i)
        if i[8:13] == date[2] and i[19:21] =='pm' and i[22:26] == '1800':
            filename_category[9].append(i)
        if i[8:13] == date[2] and i[19:21] =='am' and i[22:26] == '2700':
            filename_category[10].append(i)
        if i[8:13] == date[2] and i[19:21] =='pm' and i[22:26] == '2700':
            filename_category[11].append(i)
    return filename_category



def categorize_file_health(filename, date):
    filename_category = [[],[],[],[],[],[],[],[],[],[],[],[]]
    for i in filename:
        if i[7:12] == date[0] and i[18:20] =='am' and i[21:25] == '1800':
            filename_category[0].append(i)
        if i[7:12] == date[0] and i[18:20] =='pm' and i[21:25] == '1800':
            filename_category[1].append(i)
        if i[7:12] == date[0] and i[18:20] =='am' and i[21:25] == '2700':
            filename_category[2].append(i)
        if i[7:12] == date[0] and i[18:20] =='pm' and i[21:25] == '2700':
            filename_category[3].append(i)
        if i[7:12] == date[1] and i[18:20] =='am' and i[21:25] == '1800':
            filename_category[4].append(i)
        if i[7:12] == date[1] and i[18:20] =='pm' and i[21:25] == '1800':
            filename_category[5].append(i)
        if i[7:12] == date[1] and i[18:20] =='am' and i[21:25] == '2700':
            filename_category[6].append(i)
        if i[7:12] == date[1] and i[18:20] =='pm' and i[21:25] == '2700':
            filename_category[7].append(i)
        if i[7:12] == date[2] and i[18:20] =='am' and i[21:25] == '1800':
            filename_category[8].append(i)
        if i[7:12] == date[2] and i[18:20] =='pm' and i[21:25] == '1800':
            filename_category[9].append(i)
        if i[7:12] == date[2] and i[18:20] =='am' and i[21:25] == '2700':
            filename_category[10].append(i)
        if i[7:12] == date[2] and i[18:20] =='pm' and i[21:25] == '2700':
            filename_category[11].append(i)
    return filename_category



def avg_polygon_to_shapefile_health_urgent(path_name, date_list):
    path = os.chdir(r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\health_urgent'%path_name)
    file = os.listdir(path)
    date = date_list
    names = ["%s_morning_1800"%date_list[0], "%s_midday_1800"%date_list[0], 
             "%s_morning_2700"%date_list[0], "%s_midday_2700"%date_list[0],
            "%s_morning_1800"%date_list[1], "%s_midday_1800"%date_list[1], 
             "%s_morning_2700"%date_list[1], "%s_midday_2700"%date_list[1],
            "%s_morning_1800"%date_list[2], "%s_midday_1800"%date_list[2], 
             "%s_morning_2700"%date_list[2], "%s_midday_2700"%date_list[2]]
    file_list = categorize_file_health(file,date)
    shapes = []
    for i in file_list:
        #print(i)
        a = read_shapes(i)
        shapes.append(a)
    for i in range(len(shapes)):
        print(i)
        average_polygon(shapes[i], r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\health_%s_urgent.geojson'%(path_name,names[i]))
        print(i, "done")



def avg_polygon_to_shapefile_health_nonurgent(path_name, date_list):
    path = os.chdir(r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\health_nonurgent'%path_name)
    file = os.listdir(path)
    date = date_list
    names = ["%s_morning_1800"%date_list[0], "%s_midday_1800"%date_list[0], 
             "%s_morning_2700"%date_list[0], "%s_midday_2700"%date_list[0],
            "%s_morning_1800"%date_list[1], "%s_midday_1800"%date_list[1], 
             "%s_morning_2700"%date_list[1], "%s_midday_2700"%date_list[1],
            "%s_morning_1800"%date_list[2], "%s_midday_1800"%date_list[2], 
             "%s_morning_2700"%date_list[2], "%s_midday_2700"%date_list[2]]
    file_list = categorize_file_health(file,date)
    shapes = []
    for i in file_list:
        #print(i)
        a = read_shapes(i)
        shapes.append(a)
    for i in range(len(shapes)):
        print(i)
        average_polygon(shapes[i], r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\health_%s_nonurgent.geojson'%(path_name,names[i]))
        print(i, "done")



def avg_polygon_to_shapefile_grocery(path_name, date_list):
    path = os.chdir(r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\grocery'%path_name)
    file = os.listdir(path)
    date = date_list
    names = ["%s_morning_1800"%date_list[0], "%s_midday_1800"%date_list[0], 
             "%s_morning_2700"%date_list[0], "%s_midday_2700"%date_list[0],
            "%s_morning_1800"%date_list[1], "%s_midday_1800"%date_list[1], 
             "%s_morning_2700"%date_list[1], "%s_midday_2700"%date_list[1],
            "%s_morning_1800"%date_list[2], "%s_midday_1800"%date_list[2], 
             "%s_morning_2700"%date_list[2], "%s_midday_2700"%date_list[2]]
    file_list = categorize_file(file,date)
    shapes = []
    for i in file_list:
        #print(i)
        a = read_shapes(i)
        shapes.append(a)
    for i in range(len(shapes)):
        print(i)
        average_polygon(shapes[i], r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\grocery_%s.geojson'%(path_name,names[i]))
        print(i, "done")



def store_file_health_nonurgent(path_name, date, city_name):
    path = os.chdir(r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s'%path_name)
    file = os.listdir(path)
    outpath = r'D:\Accessibility_study\Final isochrones1\health_nonurgent'
    outfile = os.listdir(outpath)
     

    covid = [[],[],[],[]]
    precovid = [[],[],[],[]]
    postcovid = [[],[],[],[]]
    for i in file:
        infile = r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\%s'%(path_name,i)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:20] =='morning' and i[21:25] == '1800' and i[26:35] == 'nonurgent':
            covid[0].append(i)   
            outfile1 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_covid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:20] =='morning' and i[21:25] == '2700' and i[26:35] == 'nonurgent':
            covid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_covid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:19] =='midday' and i[20:24] == '1800' and i[25:34] == 'nonurgent':
            covid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_covid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:19] =='midday' and i[20:24] == '2700' and i[25:34] == 'nonurgent':
            covid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_covid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:20] =='morning' and i[21:25] == '1800' and i[26:35] == 'nonurgent':
            precovid[0].append(i)
            outfile1 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_precovid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:20] =='morning' and i[21:25] == '2700' and i[26:35] == 'nonurgent':
            precovid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_precovid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:19] =='midday' and i[20:24] == '1800' and i[25:34] == 'nonurgent':
            precovid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_precovid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:19] =='midday' and i[20:24] == '2700' and i[25:34] == 'nonurgent':
            precovid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_precovid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:20] =='morning' and i[21:25] == '1800' and i[26:35] == 'nonurgent':
            postcovid[0].append(i)
            outfile1 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_postcovid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:20] =='morning' and i[21:25] == '2700' and i[26:35] == 'nonurgent':
            postcovid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_postcovid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:19] =='midday' and i[20:24] == '1800' and i[25:34] == 'nonurgent':
            postcovid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_postcovid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:19] =='midday' and i[20:24] == '2700' and i[25:34] == 'nonurgent':
            postcovid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\health_nonurgent\health_postcovid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
    print (precovid, covid, postcovid)



def store_file_health_urgent(path_name, date, city_name):
    path = os.chdir(r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s'%path_name)
    file = os.listdir(path)
    outpath = r'D:\Accessibility_study\Final isochrones1\health_urgent'
    outfile = os.listdir(outpath)
     

    covid = [[],[],[],[]]
    precovid = [[],[],[],[]]
    postcovid = [[],[],[],[]]
    for i in file:
        infile = r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\%s'%(path_name,i)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:20] =='morning' and i[21:25] == '1800' and i[26:32] == 'urgent':
            covid[0].append(i)   
            outfile1 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_covid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:20] =='morning' and i[21:25] == '2700' and i[26:32] == 'urgent':
            covid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_covid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:19] =='midday' and i[20:24] == '1800' and i[25:31] == 'urgent':
            covid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_covid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:6] == 'health' and i[7:12] == date[1] and i[13:19] =='midday' and i[20:24] == '2700' and i[25:31] == 'urgent':
            covid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_covid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:20] =='morning' and i[21:25] == '1800' and i[26:32] == 'urgent':
            precovid[0].append(i)
            outfile1 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_precovid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:20] =='morning' and i[21:25] == '2700' and i[26:32] == 'urgent':
            precovid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_precovid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:19] =='midday' and i[20:24] == '1800' and i[25:31] == 'urgent':
            precovid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_precovid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:6] == 'health' and i[7:12] == date[0] and i[13:19] =='midday' and i[20:24] == '2700' and i[25:31] == 'urgent':
            precovid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_precovid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:20] =='morning' and i[21:25] == '1800' and i[26:32] == 'urgent':
            postcovid[0].append(i)
            outfile1 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_postcovid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:20] =='morning' and i[21:25] == '2700' and i[26:32] == 'urgent':
            postcovid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_postcovid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:19] =='midday' and i[20:24] == '1800' and i[25:31] == 'urgent':
            postcovid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_postcovid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:6] == 'health' and i[7:12] == date[2] and i[13:19] =='midday' and i[20:24] == '2700' and i[25:31] == 'urgent':
            postcovid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\health_urgent\health_postcovid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
    print (precovid, covid, postcovid)



def store_file_grocery(path_name, date, city_name):
    path = os.chdir(r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s'%path_name)
    file = os.listdir(path)
    outpath = r'D:\Accessibility_study\Final isochrones1\grocery'
    outfile = os.listdir(outpath)
     

    covid = [[],[],[],[]]
    precovid = [[],[],[],[]]
    postcovid = [[],[],[],[]]
    for i in file:
        infile = r'D:\Accessibility_study\isochrones_by_cities\EPSG 5070\%s\%s'%(path_name,i)
        if i[:7] == 'grocery' and i[8:13] == date[1] and i[14:21] =='morning' and i[22:26] == '1800':
            covid[0].append(i)   
            outfile1 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_covid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:7] == 'grocery' and i[8:13] == date[1] and i[14:21] =='morning' and i[22:26] == '2700':
            covid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_covid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:7] == 'grocery' and i[8:13] == date[1] and i[14:20] =='midday' and i[21:25] == '1800':
            covid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_covid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:7] == 'grocery' and i[8:13] == date[1] and i[14:20] =='midday' and i[21:25] == '2700':
            covid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_covid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
        if i[:7] == 'grocery' and i[8:13] == date[0] and i[14:21] =='morning' and i[22:26] == '1800':
            precovid[0].append(i)
            outfile1 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_precovid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:7] == 'grocery' and i[8:13] == date[0] and i[14:21] =='morning' and i[22:26] == '2700':
            precovid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_precovid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:7] == 'grocery' and i[8:13] == date[0] and i[14:20] =='midday' and i[21:25] == '1800':
            precovid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_precovid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:7] == 'grocery' and i[8:13] == date[0] and i[14:20] =='midday' and i[21:25] == '2700':
            precovid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_precovid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
        if i[:7] == 'grocery' and i[8:13] == date[2] and i[14:21] =='morning' and i[22:26] == '1800':
            postcovid[0].append(i)
            outfile1 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_postcovid_1800_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile1)
        if i[:7] == 'grocery' and i[8:13] == date[2] and i[14:21] =='morning' and i[22:26] == '2700':
            postcovid[1].append(i)
            outfile2 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_postcovid_2700_morning\%s_%s'%(city_name, i)
            write_shapes(infile,outfile2)
        if i[:7] == 'grocery' and i[8:13] == date[2] and i[14:20] =='midday' and i[21:25] == '1800':
            postcovid[2].append(i)
            outfile3 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_postcovid_1800_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile3)
        if i[:7] == 'grocery' and i[8:13] == date[2] and i[14:20] =='midday' and i[21:25] == '2700':
            postcovid[3].append(i)
            outfile4 = r'D:\Accessibility_study\Final isochrones1\grocery\grocery_postcovid_2700_midday\%s_%s'%(city_name, i)
            write_shapes(infile,outfile4)
    print (precovid, covid, postcovid)


## Estimating average polygons for groceries

louisville_grocery = avg_polygon_to_shapefile_grocery("isochrones_louisville", ["Jan28","Mar31", "Nov17"])
nashville_grocery = avg_polygon_to_shapefile_grocery("isochrones_nashville", ["Jan28","Apr07", "Nov24"])
philadelphia_grocery = avg_polygon_to_shapefile_grocery("isochrones_philadelphia", ["Jan28","Apr14", "Nov17"])
columbus_grocery = avg_polygon_to_shapefile_grocery("isochrones_columbus", ["Jan07","May05", "Dec08"])
atlanta_grocery = avg_polygon_to_shapefile_grocery("isochrones_atlanta", ["Jan07","Apr28", "Dec08"])
miami_grocery = avg_polygon_to_shapefile_grocery("isochrones_miami", ["Feb25","Apr28", "Nov03"])
dallas_grocery = avg_polygon_to_shapefile_grocery("isochrones_dallas", ["Jan07","Apr07", "Dec08"])
denver_grocery = avg_polygon_to_shapefile_grocery("isochrones_denver", ["Jan07","May26", "Nov24"])
austin_grocery = avg_polygon_to_shapefile_grocery("isochrones_austin", ["Jan07","Mar31", "Dec01"])
chicago_grocery = avg_polygon_to_shapefile_grocery("isochrones_chicago", ["Jan07","Apr21", "Nov24"])
la_grocery = avg_polygon_to_shapefile_grocery("isochrones_LA", ["Jan14","Jun23", "Dec01"])
nyc_grocery = avg_polygon_to_shapefile_grocery("isochrones_nyc", ["Jan07","May05", "Dec01"])
phoenix_grocery = avg_polygon_to_shapefile_grocery("isochrones_phoenix", ["Jan21","Apr14", "Dec01"])
portland_grocery = avg_polygon_to_shapefile_grocery("isochrones_portland", ["Jan14","Apr21", "Dec15"])
sanfrancisco_grocery = avg_polygon_to_shapefile_grocery("isochrones_sanfrancisco", ["Jan28","Apr14", "Dec01"])
sanjose_grocery = avg_polygon_to_shapefile_grocery("isochrones_sanjose", ["Jan28","Mar31", "Dec01"])
seattle_grocery = avg_polygon_to_shapefile_grocery("isochrones_seattle", ["Feb04","Jun09","Dec08"])
madison_grocery = avg_polygon_to_shapefile_grocery("isochrones_madison", ["Feb04","Mar24", "Dec08"])
champaign_grocery = avg_polygon_to_shapefile_grocery("isochrones_champaign", ["Jan07","May12", "Nov24"])
annarbor_grocery = avg_polygon_to_shapefile_grocery("isochrones_annarbor", ["Jan28","Mar31", "Nov17"])
boston_grocery = avg_polygon_to_shapefile_grocery("isochrones_boston", ["Jan07","Mar10", "Dec15"])
slc_grocery = avg_polygon_to_shapefile_grocery("isochrones_slc", ["Jan14","Apr28", "Nov24"])


## Estimating average polygon for urgent health

columbus_health = avg_polygon_to_shapefile_health_urgent("isochrones_columbus", ["Jan07","May05", "Dec08"])
philadelphia_health = avg_polygon_to_shapefile_health_urgent("isochrones_philadelphia", ["Jan28","Apr14", "Nov17"])
miami_health = avg_polygon_to_shapefile_health_urgent("isochrones_miami", ["Feb25","Apr28", "Nov03"])
nashville_health = avg_polygon_to_shapefile_health_urgent("isochrones_nashville", ["Jan28","Apr07", "Nov24"])
denver_health = avg_polygon_to_shapefile_health_urgent("isochrones_denver", ["Jan07","May26", "Nov24"])
dallas_health = avg_polygon_to_shapefile_health_urgent("isochrones_dallas", ["Jan07","Apr07", "Dec08"])
boston_health = avg_polygon_to_shapefile_health_urgent("isochrones_boston", ["Jan07","Mar10", "Dec15"])
portland_health = avg_polygon_to_shapefile_health_urgent("isochrones_portland", ["Jan14","Apr21", "Dec15"])
seattle_health = avg_polygon_to_shapefile_health_urgent("isochrones_seattle", ["Feb04","Jun09","Dec08"])
sanfrancisco_health = avg_polygon_to_shapefile_health_urgent("isochrones_sanfrancisco", ["Jan28","Apr14", "Dec01"])
sanjose_health = avg_polygon_to_shapefile_health_urgent("isochrones_sanjose", ["Jan28","Mar31", "Dec01"])
chicago_health = avg_polygon_to_shapefile_health_urgent("isochrones_chicago", ["Jan07","Apr21", "Nov24"])
austin_health = avg_polygon_to_shapefile_health_urgent("isochrones_austin", ["Jan07","Mar31", "Dec01"])
la_health = avg_polygon_to_shapefile_health_urgent("isochrones_LA", ["Jan14","Jun23", "Dec01"])
louisville_health = avg_polygon_to_shapefile_health_urgent("isochrones_louisville", ["Jan28","Mar31", "Nov17"])
champaign_health = avg_polygon_to_shapefile_health_urgent("isochrones_champaign", ["Jan07","May12", "Nov24"])
phoenix_health = avg_polygon_to_shapefile_health_urgent("isochrones_phoenix", ["Jan21","Apr14", "Dec01"])
annarbor_health = avg_polygon_to_shapefile_health_urgent("isochrones_annarbor", ["Jan28","Mar31", "Nov17"])
atlanta_health = avg_polygon_to_shapefile_health_urgent("isochrones_atlanta", ["Jan07","Apr28", "Dec08"])
slc_health = avg_polygon_to_shapefile_health_urgent("isochrones_slc", ["Jan14","Apr28", "Nov24"])
madison_health = avg_polygon_to_shapefile_health_urgent("isochrones_madison", ["Feb04","Mar24", "Dec08"])
nyc_health = avg_polygon_to_shapefile_health_urgent("isochrones_nyc", ["Jan07","May05", "Dec01"])


## Estimating average polygon for non-urgent health

annarbor_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_annarbor", ["Jan28","Mar31", "Nov17"])
atlanta_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_atlanta", ["Jan07","Apr28", "Dec08"])
columbus_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_columbus", ["Jan07","May05", "Dec08"])
philadelphia_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_philadelphia", ["Jan28","Apr14", "Nov17"])
miami_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_miami", ["Feb25","Apr28", "Nov03"])
nashville_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_nashville", ["Jan28","Apr07", "Nov24"])
denver_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_denver", ["Jan07","May26", "Nov24"])
dallas_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_dallas", ["Jan07","Apr07", "Dec08"])
boston_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_boston", ["Jan07","Mar10", "Dec15"])
portland_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_portland", ["Jan14","Apr21", "Dec15"])
seattle_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_seattle", ["Feb04","Jun09","Dec08"])
sanfrancisco_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_sanfrancisco", ["Jan28","Apr14", "Dec01"])
sanjose_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_sanjose", ["Jan28","Mar31", "Dec01"])
chicago_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_chicago", ["Jan07","Apr21", "Nov24"])
austin_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_austin", ["Jan07","Mar31", "Dec01"])
la_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_LA", ["Jan14","Jun23", "Dec01"])
louisville_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_louisville", ["Jan28","Mar31", "Nov17"])
slc_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_slc", ["Jan14","Apr28", "Nov24"])
madison_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_madison", ["Feb04","Mar24", "Dec08"])
champaign_health = avg_polygon_to_shapefile_health_nonurgent("isochrones_champaign", ["Jan07","May12", "Nov24"])


## File categorizations

store_file_health_urgent('isochrones_austin', ["Jan07","Mar31", "Dec01"], 'Austin')
store_file_health_urgent("isochrones_chicago", ["Jan07","Apr21", "Nov24"], 'Chicago')
store_file_health_urgent("isochrones_LA", ["Jan14","Jun23", "Dec01"], 'LA')
store_file_health_urgent("isochrones_louisville", ["Jan28","Mar31", "Nov17"], 'Louisville')
store_file_health_urgent("isochrones_nashville", ["Jan28","Apr07", "Nov24"], 'Nashville')
store_file_health_urgent("isochrones_nyc", ["Jan07","May05", "Dec01"], 'NYC')
store_file_health_urgent("isochrones_philadelphia", ["Jan28","Apr14", "Nov17"], 'Philadelphia')
store_file_health_urgent("isochrones_phoenix", ["Jan21","Apr14", "Dec01"], 'Phoenix')
store_file_health_urgent("isochrones_portland", ["Jan14","Apr21", "Dec15"], 'Portland')
store_file_health_urgent("isochrones_sanfrancisco", ["Jan28","Apr14", "Dec01"], 'SF')
store_file_health_urgent("isochrones_sanjose", ["Jan28","Mar31", "Dec01"], 'Sanjose')
store_file_health_urgent("isochrones_columbus", ["Jan07","May05", "Dec08"], 'Columbus')
store_file_health_urgent("isochrones_seattle", ["Feb04","Jun09","Dec08"], 'Seattle')
store_file_health_urgent("isochrones_madison", ["Feb04","Mar24", "Dec08"], 'Madison')
store_file_health_urgent("isochrones_champaign", ["Jan07","May12", "Nov24"], 'Champaign')
store_file_health_urgent("isochrones_annarbor", ["Jan28","Mar31", "Nov17"], 'Annarbor')
store_file_health_urgent("isochrones_boston", ["Jan07","Mar10", "Dec15"], 'Boston')
store_file_health_urgent("isochrones_denver", ["Jan07","May26", "Nov24"], 'Denver')
store_file_health_urgent("isochrones_atlanta", ["Jan07","Apr28", "Dec08"], 'Atlanta')
store_file_health_urgent("isochrones_slc", ["Jan14","Apr28", "Nov24"], 'SLC')
store_file_health_urgent("isochrones_miami", ["Feb25","Apr28", "Nov03"], 'Miami')
store_file_health_urgent("isochrones_dallas", ["Jan07","Apr07", "Dec08"], 'Dallas')



store_file_health_nonurgent('isochrones_austin', ["Jan07","Mar31", "Dec01"], 'Austin')
store_file_health_nonurgent("isochrones_chicago", ["Jan07","Apr21", "Nov24"], 'Chicago')
store_file_health_nonurgent("isochrones_LA", ["Jan14","Jun23", "Dec01"], 'LA')
store_file_health_nonurgent("isochrones_louisville", ["Jan28","Mar31", "Nov17"], 'Louisville')
store_file_health_nonurgent("isochrones_nashville", ["Jan28","Apr07", "Nov24"], 'Nashville')
store_file_health_nonurgent("isochrones_nyc", ["Jan07","May05", "Dec01"], 'NYC')
store_file_health_nonurgent("isochrones_philadelphia", ["Jan28","Apr14", "Nov17"], 'Philadelphia')
store_file_health_nonurgent("isochrones_phoenix", ["Jan21","Apr14", "Dec01"], 'Phoenix')
store_file_health_nonurgent("isochrones_portland", ["Jan14","Apr21", "Dec15"], 'Portland')
store_file_health_nonurgent("isochrones_sanfrancisco", ["Jan28","Apr14", "Dec01"], 'SF')
store_file_health_nonurgent("isochrones_sanjose", ["Jan28","Mar31", "Dec01"], 'Sanjose')
store_file_health_nonurgent("isochrones_columbus", ["Jan07","May05", "Dec08"], 'Columbus')
store_file_health_nonurgent("isochrones_seattle", ["Feb04","Jun09","Dec08"], 'Seattle')
store_file_health_nonurgent("isochrones_madison", ["Feb04","Mar24", "Dec08"], 'Madison')
store_file_health_nonurgent("isochrones_champaign", ["Jan07","May12", "Nov24"], 'Champaign')
store_file_health_nonurgent("isochrones_annarbor", ["Jan28","Mar31", "Nov17"], 'Annarbor')
store_file_health_nonurgent("isochrones_boston", ["Jan07","Mar10", "Dec15"], 'Boston')
store_file_health_nonurgent("isochrones_denver", ["Jan07","May26", "Nov24"], 'Denver')
store_file_health_nonurgent("isochrones_atlanta", ["Jan07","Apr28", "Dec08"], 'Atlanta')
store_file_health_nonurgent("isochrones_slc", ["Jan14","Apr28", "Nov24"], 'SLC')
store_file_health_nonurgent("isochrones_miami", ["Feb25","Apr28", "Nov03"], 'Miami')
store_file_health_nonurgent("isochrones_dallas", ["Jan07","Apr07", "Dec08"], 'Dallas')




store_file_grocery('isochrones_austin', ["Jan07","Mar31", "Dec01"], 'Austin')
store_file_grocery("isochrones_chicago", ["Jan07","Apr21", "Nov24"], 'Chicago')
store_file_grocery("isochrones_LA", ["Jan14","Jun23", "Dec01"], 'LA')
store_file_grocery("isochrones_louisville", ["Jan28","Mar31", "Nov17"], 'Louisville')
store_file_grocery("isochrones_nashville", ["Jan28","Apr07", "Nov24"], 'Nashville')
store_file_grocery("isochrones_nyc", ["Jan07","May05", "Dec01"], 'NYC')
store_file_grocery("isochrones_philadelphia", ["Jan28","Apr14", "Nov17"], 'Philadelphia')
store_file_grocery("isochrones_phoenix", ["Jan21","Apr14", "Dec01"], 'Phoenix')
store_file_grocery("isochrones_portland", ["Jan14","Apr21", "Dec15"], 'Portland')
store_file_grocery("isochrones_sanfrancisco", ["Jan28","Apr14", "Dec01"], 'SF')
store_file_grocery("isochrones_sanjose", ["Jan28","Mar31", "Dec01"], 'Sanjose')
store_file_grocery("isochrones_columbus", ["Jan07","May05", "Dec08"], 'Columbus')
store_file_grocery("isochrones_seattle", ["Feb04","Jun09","Dec08"], 'Seattle')
store_file_grocery("isochrones_madison", ["Feb04","Mar24", "Dec08"], 'Madison')
store_file_grocery("isochrones_champaign", ["Jan07","May12", "Nov24"], 'Champaign')
store_file_grocery("isochrones_annarbor", ["Jan28","Mar31", "Nov17"], 'Annarbor')
store_file_grocery("isochrones_boston", ["Jan07","Mar10", "Dec15"], 'Boston')
store_file_grocery("isochrones_denver", ["Jan07","May26", "Nov24"], 'Denver')
store_file_grocery("isochrones_atlanta", ["Jan07","Apr28", "Dec08"], 'Atlanta')
store_file_grocery("isochrones_slc", ["Jan14","Apr28", "Nov24"], 'SLC')
store_file_grocery("isochrones_miami", ["Feb25","Apr28", "Nov03"], 'Miami')
store_file_grocery("isochrones_dallas", ["Jan07","Apr07", "Dec08"], 'Dallas')


## SLD database
bg = gp.read_file(r'D:\Accessibility_study\Final isochrones1\Shapefiles_new\Original shapefiles\study_area_updated_10_24_21.shp')
bg.columns
bg.sort_values("GEOID", inplace = True) 
bg.drop_duplicates(subset ="GEOID", keep = "first", inplace = True) 


SLD = gp.read_file(r'D:\econ_analysis\SmartLocationDb\SmartLocationDb.gdb', driver="FileGDB")
SLD1 = SLD[['GEOID10','D1C', 'D1D', 'WORKERS','R_LOWWAGEWK','R_PCTLOWWAGE', 'D3a', 'D3amm', 'D3bmm4']].rename(columns = {
    'D1C': 'job_den','D1D': 'act_den','WORKERS':'tot_work', 
    'R_LOWWAGEWK':'low_inc_work','R_PCTLOWWAGE':'p_low_inc',
    'D3a':'road_dens','D3amm': 'mm_road_den','D3bmm4': 'int_den'})


blg = pd.merge(bg, SLD1, left_on = 'GEOID', right_on = 'GEOID10', how = "left")
blg['Area_sqm'] = list(blg['geometry'].area)
#blg['Area_acre'] = blg['Area_sqm'] * 0.00024711
#blg['Area_sqmi'] = blg['Area_sqm'] * 3.861E-07
#blg['jobs'] = blg['job_den'] * blg['Area_acre']
#blg['activity'] = blg['act_den'] * blg['Area_acre']
#blg['road'] = blg['road_dens'] * blg['Area_sqmi']
#blg['mm_road'] = blg['mm_road_den'] * blg['Area_sqmi']
#blg['int_road'] = blg['int_den'] * blg['Area_sqmi']



gdf = gp.GeoDataFrame(blg, geometry = 'geometry')
gdf.to_file(r'D:\Accessibility_study\Final isochrones1\shapefiles\study_area_edited.shp', driver = 'ESRI Shapefile')


## Joining isochrones to blg
blg = gp.read_file(r'D:\Accessibility_study\Final isochrones1\Shapefiles_new\Original shapefiles\study_area_updated_10_24_21.shp')

shp0=gp.read_file(r'D:\Accessibility_study\Final isochrones1\shapefiles_new\Differences\health_u_diff13_2700_morning.shp')
shp1=gp.read_file(r'D:\Accessibility_study\Final isochrones1\shapefiles_new\Original shapefiles_segmented\health_u_postcovid_2700_morning_m.shp')

shp0['Access'] = [0 for i in range(len(shp0))]
shp1['Access'] = [1 for i in range(len(shp1))]

shp1 = shp1[['layer', 'Access', 'geometry']]
shp0 = shp0[['layer', 'Access', 'geometry']]

shp = pd.concat([shp0, shp1])
shp['ID'] = [i for i in range(len(shp))]

shp_bg = gp.overlay(shp, blg, how='intersection')
shp_bg['shp_sqm'] = list(shp_bg['geometry'].area)
shp_bg['ratio'] = shp_bg['shp_sqm'] /shp_bg['Area_sqm']
shp_bg = shp_bg.sort_values(by = "ID")


shp_bg1 = shp_bg[shp_bg['ratio'] >=0.3]


#difference in isochrones in percentage
(sum(shp_bg1['shp_sqm'])/sum(shp_bg['shp_sqm']))


#dataframe with no duplicate
dat1 = shp_bg1.drop_duplicates(subset = "GEOID", keep = False)
#find duplicates
dat2 = shp_bg1.merge(dat1,indicator = True, how='left').loc[lambda x : x['_merge']!='both']
dat3 = dat2[dat2['Access']==1]
dat4 = dat3.drop_duplicates(subset = "GEOID", keep = False)

shp_bg1 = pd.concat ([dat1, dat4])

blg_list = list(shp_bg1['GEOID'].unique())
shp_bg2 = blg[blg['GEOID'].isin(blg_list)]

shp_bg2.columns

dat5 = shp_bg1[['GEOID', 'Access']]
shp_bg3 = pd.merge(dat5, shp_bg2, on = 'GEOID')

gdf = gp.GeoDataFrame(shp_bg1, geometry = 'geometry')
gdf.to_file(r'D:\Accessibility_study\Final isochrones1\shapefiles_new\Final aggregated layers\health_2700_morning_isochrones13_u.shp', driver = 'ESRI Shapefile')

gdf = gp.GeoDataFrame(shp_bg3, geometry = 'geometry')
gdf.to_file(r'D:\Accessibility_study\Final isochrones1\shapefiles_new\Final aggregated layers\health_2700_morning_blg13_u.shp', driver = 'ESRI Shapefile')


## Datasets and summary tables

dat1 = gp.read_file(r'D:\Accessibility_study\Final isochrones1\shapefiles_new\Final aggregated layers\health_2700_morning_isochrones12_u.shp')
dat2 = gp.read_file(r'D:\Accessibility_study\Final isochrones1\shapefiles_new\Final aggregated layers\health_2700_morning_isochrones13_u.shp')

dat1['Time'] = 'Morning'
dat2['Time'] = 'Morning'
dat1['Diff'] = 'Precovid vs covid'
dat2['Diff'] = 'Precovid vs postcovid'
dat1['Iso'] = '2700'
dat2['Iso'] = '2700'

dat_1800 = pd.concat([dat1, dat2])
dat_1800['pop_den'] = dat_1800['tot_pop']/(dat_1800['Area_sqm']*0.00024711)
dat_1800['Facility'] = 'Health_urgent'

layer_1800 = list(dat_1800['layer_1'].unique())
City_1800 = [i[:(len(i)-33)] for i in layer_1800] 
City_1800

layer_1800 = list(dat_1800['layer_1'].unique())
City_1800 = [i[:(len(i)-33)] for i in layer_1800] 

dat_1800['City'] = dat_1800['layer_1'].replace(layer_1800,City_1800)
dat_1800['area_type'] = ['rural' if dat_1800['act_den'].iloc[i] <= 0.5
                          else ('urban' if dat_1800['act_den'].iloc[i] >= 6 else 'suburban') 
                          for i in range(len(dat_1800))]

dat_1800['p_nv'] = dat_1800['p_ow_nv'] + dat_1800['p_re_nv']

dat1_1800 = dat_1800[['layer_1', 'City','Access', 'ID', 'GEOID', 'NAME', 'HH_size', 
                      'med_inc', 'tot_pop','p_his', 'p_nhw', 'p_black', 'p_other', 'p_hs', 'p_pov',
                      'p_noins', 'p_ow_nv', 'p_re_nv','p_nv','p_own', 'pop_den',
                      'job_den', 'act_den','p_low_inc', 'road_dens', 'mm_road_de',
                      'int_den', 'area_type','Area_sqm', 'shp_sqm', 'ratio', 'Time', 'Iso', 'Facility','Diff']]

dat2_1800 = pd.concat([dat1_1800, pd.get_dummies(dat1_1800['area_type'])], axis=1)
dat3_1800 = pd.concat([dat2_1800, pd.get_dummies(dat2_1800['City'])], axis=1)
dat3_1800.to_csv(r'D:\Accessibility_study\Final isochrones1\Summary\health_2700_morning_u.csv')

### summary tables
dat_1800 = pd.read_csv(r'D:\Accessibility_study\Final isochrones1\Summary\health_1800_midday_nu.csv')
summary_1800 = dat_1800.groupby(["Access", 'Diff', 'area_type']).agg({'p_pov': ["mean", "median", "std"],
                                                                'p_nhw': ["mean", "median", "std"],
                                                                'p_black': ["mean", "median", "std"],
                                                                'p_other': ["mean", "median", "std"],
                                                                'p_nv': ["mean", "median", "std"],
                                                                'p_low_inc': ["mean", "median", "std"],
                                                                'Area_sqm': ["sum"]}).reset_index()
summary_1800.to_csv(r'D:\Accessibility_study\Final isochrones1\Summary\health_summary_2700_morning_u.csv')

