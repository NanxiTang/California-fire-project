This is a practical file for rasterization of the data from the "California Fire Project".
The temperature data gained from stations within CA are discrete and are points. The goal of this file is to show how to transform the sf dataframe into a raster. 
It introduced the "extract()" function in the "terra" package, getting the points inside the exact cell and IDW (Inverse Distance Weighting) method in such order.
