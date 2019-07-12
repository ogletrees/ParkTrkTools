# ParkTrkTools
R package of tools for the project on mobile app data and parks

2019-07-11

This package contains some tools for the project on using mobile app data for park research.

Requires packages *tidycensus* and *tidyverse* (for *%>%* and string functions)

The functions inside are:

**tract_sample** - gets a weighted sample from census tract Age-Sex-Race table

**tract_ed** - gets educational attainment for a census tract GEOID

**tract_inc** - gets household income for a census tract GEOID
