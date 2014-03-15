# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 29, 2012
# Last modified: Feb 20, 2014
# Purpose:       Steps to do interpolation of modern occurrence data
#                and overlay grid etc.
# ====================================================================

# What comes first to generate "composite.occ2.rda"?

# Warning, these take a long time. These scripts were run on a server.

# Implement a bounding box within realms to interpolate provinces:
# (This script excludes the marine mammals and sharks which were based on range
# polygons. I bring them back in later.)
source("realm_bounding_box.R")

# Check a random selection of interpolations graphically:
source("test_realm_bounding_box.R")

# Bring back in the mammals and sharks. Derek did the spatial join by polygons
# to generate these data.
source("merge_interpolated_provs_with_mammals_and_sharks.R")

# Overlay a grid on the interpolated data to derive an equal area grid version
# for extracting range parameters for modelling.
source("overlay_grid_on_interpolated_provinces.R")

# alternative version without interpolation:
source("overlay_grid_on_raw_occurrences.R")
