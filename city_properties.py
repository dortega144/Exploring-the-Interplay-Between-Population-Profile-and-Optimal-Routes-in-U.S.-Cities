#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 13 20:06:32 2024

@author: blackgats
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 10 15:48:51 2024

@author: blackgats
"""
import operator
import igraph as ig
import networkx as nx
import osmnx as ox
import pandas as pd
import numpy as np

ox.__version__
ox.settings.use_cache=True




# get the street network for a place, and its area in square meters
place = "Raleigh, NC, USA" #Alameda, Alameda County, CA, USA
gdf = ox.geocode_to_gdf(place)
area = ox.projection.project_gdf(gdf).unary_union.area
G = ox.graph_from_place(place, network_type="drive") 
#G_proj = ox.project_graph(G,to_crs="6579")




#statsG = ox.basic_stats(G,area=area)
#print(pd.Series(statsG))
#
#nc = nx.average_node_connectivity(ox.convert.to_digraph(G))
ox.plot_graph(G)
GU = ox.utils_graph.convert.to_undirected(G)
statsGU = ox.basic_stats(GU,area=area)
#print(statsGU)
print(pd.Series(statsGU))


# add edge bearing
GU_mdigr_bearing = ox.bearing.add_edge_bearings(
    GU, precision=None)

# get bearing
GU_bearings = GU_mdigr_bearing.edges(data="bearing")

len(GU_bearings)

bearings = []
min_length = 0
weight = None

for u, v, data in GU_mdigr_bearing.edges(data=True):
    # ignore self-loops and checks minimum length
    if u != v and data["length"] >= min_length:
        if weight:
            # weight edges' bearings by some edge attribute value
            bearings.extend([data["bearing"]] * int(data[weight]))
        else:
            # don't weight bearings, just take one value per edge
            bearings.append(data["bearing"])

len(bearings)


# polar histogram
ox.plot.plot_orientation(GU_mdigr_bearing, num_bins=36, min_length=0,
                         weight=None, ax=None, figsize=(5, 5), area=True,
                         color='#003366', edgecolor='k', linewidth=0.5,
                         alpha=0.7, title=None, title_y=1.05, title_font=None,
                         xtick_font=None)


# orientation entropy for chicago
GU_entropy = ox.bearing.orientation_entropy(
  GU_mdigr_bearing, num_bins=36, min_length=0, weight=None)
print("Entropy",GU_entropy)
