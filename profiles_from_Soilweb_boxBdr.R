library(aqp)
library(soilDB)
library(sf)
library(sharpshootR)
library(httr)
library(dendextend)
library(latticeExtra)
#library(SoilTaxonomy)

## copy / paste viewport bounding-box from SoilWeb
## click somewhere on the map
## press 'b', BBOX is copied to the clipboard

#Example of bounding box: 
#Greenbush IAT segment, northern Kettle Moraine
#copy and paste your own bounding box coordinates 
#within the single quotes

bb<-'-88.0553 43.7875,-88.0553 43.7916,-88.0419 43.7916,-88.0419 43.7875,-88.0553 43.7875'


#The following lines use your bounding box to create a polygon,
#then identifying mapping units that overlap with the polygon,
#then extract from the soil survey database this information:
#--soil series that are components of those map units
#--properties of each series
#--horizons listed in an official description of each soil series
#--properties of each of those horizons (label, color, etc.)

# assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)
## init sf polygon
x <- st_as_sfc(wkt)
# set CRS as GCS WGS84
st_crs(x) <- 4326
# get overlapping map unit keys
# could also use SDA_query() with more elaborate SQL
m <- SDA_spatialQuery(x, what = 'mukey')
# compose SQL to return component details for these map unit keys
# return only:
# * map units overlapping with BBOX
# * major components
# * no misc. areas that might share name with a poorly-named soil series
sql <- sprintf(
  "SELECT mukey, cokey, compname, compkind, comppct_r 
  FROM component 
  WHERE mukey IN %s 
  --AND majcompflag = 'Yes'
  AND compkind != 'Miscellaneous area'
  ", format_SQL_in_statement(as.integer(m$mukey))
)
# send to SDA, result is a data.frame
s <- SDA_query(sql)
# get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)
# check out results
str(osd, 1)
# convert horizon boundary distinctness -> vertical distance
# see manual page
osd$SPC$hzd <- hzDistinctnessCodeToOffset(
  osd$SPC$distinctness, 
  codes = c('very abrupt', 'abrubt', 'clear', 'gradual', 'diffuse')
)
## arrange according to classification, accounting for order within keys
# data("ST_unique_list")
# osd$SPC$soilorder <- droplevels(factor(osd$SPC$soilorder, levels = ST_unique_list$order, ordered = TRUE))
# osd$SPC$suborder <- droplevels(factor(osd$SPC$suborder, levels = ST_unique_list$suborder, ordered = TRUE))
# osd$SPC$greatgroup <- droplevels(factor(osd$SPC$greatgroup, levels = ST_unique_list$greatgroup, ordered = TRUE))
# osd$SPC$subgroup <- droplevels(factor(osd$SPC$subgroup, levels = ST_unique_list$subgroup, ordered = TRUE))

#The information extracted in the above steps is in the form of a
#soil profile collection (SPC), an object in R designed for storing
#information on soil profiles: A group of profiles, each with a set
#of horizons, and properties assigned to horizons or to whole profiles

#The following lines use the SoilTaxonomyDendrogram function to create a 
#set of profile sketches including all profiles in the SPC
#The sketches are arranged using a dendrogram, a "tree" in which
#soils that have more similar classifications are placed closer to
#each other, on the same "branches" of the tree. Could also
#classify and arrange the soils by their properties

SoilTaxonomyDendrogram(
  spc = osd$SPC, 
  y.offset = 0.4, 
  rotationOrder = profile_id(osd$SPC)[order(osd$SPC$subgroup)],
  max.depth = 180,
  scaling.factor = 0.012, 
  cex.taxon.labels = 0.75,
  cex.id = 0.85,
  cex.names = 0.75,
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = TRUE,
  axis.line.offset = -3.5,
  hz.distinctness.offset = 'hzd'
)

# The following lines summarize the geomorphic positions in which
# each soil series is typically found

# there may be records missing from SPC / geomorphic component
nm <- intersect(profile_id(osd$SPC), osd$geomcomp$series)
# keep only those series that exist in both
sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)
# inverse problem: extra records in geomcomp summaries
# subset geomcopm
geomcomp.sub <- subset(osd$geomcomp, subset = series %in% profile_id(sub))
# viz geomorphic proportion summary, results contain clustering object
res <- vizGeomorphicComponent(geomcomp.sub)
print(res$fig)

# A new plot of soil profiles and a dendrogram based on similarity
#of geomorphic position
par(mar = c(0, 0, 0, 0))
plotProfileDendrogram(
  sub,
  clust = res$clust,
  y.offset = 0.2,
  dend.y.scale = 3,
  scaling.factor = 0.01,
  width = 0.3,
  name.style = 'center-center',
  plot.depth.axis = FALSE,
  hz.depths = TRUE,
  hz.distinctness.offset = 'hzd',
  cex.names = 0.6,
  cex.id = 0.6
)

# Similar to the last two parts but using hillslope position

# there may be records missing from SPC / hill slope position
nm <- intersect(profile_id(osd$SPC), osd$hillpos$series)
# keep only those series that exist in both
sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)
# inverse problem: extra records in hill slope summaries
# subset hillpos
hillpos.sub <- subset(osd$hillpos, subset = series %in% profile_id(sub))
# viz hill slope proportion summary, results contain clustering object
res <- vizHillslopePosition(hillpos.sub)
print(res$fig)
# arrange according to clustering of hillslope position
par(mar = c(0, 0, 0, 0))
plotProfileDendrogram(
  sub, 
  clust = res$clust, 
  dend.y.scale = 3, 
  y.offset = 0.2,
  scaling.factor = 0.01, 
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = FALSE, 
  hz.depths = TRUE, 
  hz.distinctness.offset = 'hzd', 
  cex.names = 0.6, 
  cex.id = 0.6
)

