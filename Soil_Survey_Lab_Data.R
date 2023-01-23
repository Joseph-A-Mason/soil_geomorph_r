# load libraries
library(aqp)
library(soilDB)
library(lattice)
library(maps)

#Name the four soil series you will look up data for
#Replace the names in quotes with your series of interest
#use all lower case

Series1<-"valentine"
Series2<-"valent"
Series3<-"chelsea"
Series4<-"plainfield"

# fetch KSSL data by series name
sn <- c(Series1, Series2, Series3, Series4)
g <- fetchKSSL(series = sn, progress = FALSE)
# estimate soil depth based on horizon designations
sdc <- getSoilDepthClass(g, name='hzn_desgn')

#The following lines add site locations to the profiles found for
#each series, summarize soil depth, print a table you can look at to
#how many soil profiles were extracted for each series, and then turn
#all series names to lowercase (some entries are capitalized in the 
#database, others are not, so without doing this you could miss some
#profiles from the series you're interested in)

# splice-into site data
site(g) <- sdc
# summarize soil depth by taxonname
tapply(g$depth, g$taxonname, summary)
# check
table(g$taxonname)
# normalize via lower-case
g$taxonname <- tolower(g$taxonname)
# OK
table(g$taxonname)

#This creates subsets of soil profiles by lowercase series names
#so if there were any cases of one series with both capitalized and
#noncapitalized names they are grouped together by this step

#any cases where the 
Series1lc <- subset(g, taxonname == Series1)
Series2lc <- subset(g, taxonname == Series2)
Series3lc <- subset(g, taxonname == Series3)
Series4lc <- subset(g, taxonname == Series4)

# Generate a basemap of your state of interest, with county boundaries
# Substitute your state for Minnesota (inside the quotes)
#Change the xlim (western and eastern longitude) and ylim (southern
#and northern latitude), in decimal degrees. Can get from Google
#Earth

map('state', '.', xlim=c(-106.0, -87), ylim=c(37.0, 49.0), lforce='e')
# add long/lat axes
map.axes()
# add locations of Series 1
points(y ~ x, data=site(Series1lc), pch=21, bg='RoyalBlue')
# add locations of Series 2
points(y ~ x, data=site(Series2lc), pch=21, bg='DarkRed')
# add locations of Series 3
points(y ~ x, data=site(Series3lc), pch=21, bg='DarkGreen')
# add locations of Series 4
points(y ~ x, data=site(Series4lc), pch=21, bg='Orange')
# add a simple legend
legend('topleft', pch=21, pt.bg=c('RoyalBlue', 'DarkRed', 'DarkGreen', 'Orange'), 
       legend=c(Series1, Series2, Series3, Series4), bty='n')

# converts series names to a factor for grouping in plots of soil properties
g$taxonname <- factor(g$taxonname)

#Produce plots of all the individual soil profiles from the four series
#shaded by different soil properties (leaves unshaded where there is no
#data for that property). 

par(mar=c(0,0,4,1))

groupedProfilePlot(g, groups = 'taxonname', color='silt', print.id=FALSE, name=NA)

groupedProfilePlot(g, groups = 'taxonname', color='clay', print.id=FALSE, name=NA)

groupedProfilePlot(g, groups = 'taxonname', color='estimated_om', print.id=FALSE, name=NA)

groupedProfilePlot(g, groups = 'taxonname', color='estimated_ph_h2o', print.id=FALSE, name=NA)

#Aggregation by 1 cm slices. Essentially, the slab function creates 1 cm
#thick slices from all horizons so the profiles can be compared
#at the same depths from the surface downward. Properties including
# % Clay, pH, Organic Matter, and Base Saturation at pH 8.2 are assigned
#to each 1 cm slab
g.slab <- slab(g, taxonname ~ silt + clay + estimated_om + estimated_ph_h2o)
# inspect stacked data structure (check on whether operation is done correctly)
str(g.slab)

#The following steps are prep for plots of soil properties by series

# re-name soils with series name + number of pedons
new.levels <- c(Series1, Series2, Series3, Series4)
new.labels <- paste(new.levels, ' [', c(length(Series1lc), length(Series2lc), length(Series3lc), length(Series4lc)), ' pedons]', sep='')
g.slab$taxonname <- factor(g.slab$taxonname, levels=new.levels, labels=new.labels)
# new names should match the order in:
levels(g.slab$variable)
# re-name soil property labels-- order is critical !
levels(g.slab$variable) <- c('Silt (%)', 'Clay (%)','Estimated OM (%)', 'ph 1:1 H2O')
# define plotting style
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen', 'Orange'), lwd=2))

#Now we produce a set of four plots of soil properties, with lines
#showing the median for each series, with shading to show the
#25th to 75th percentile range. Can change to 5th to 95th percentile.
#Numbers on the side of each graph panel show percent of profiles
#in each series contributing to values for that series at a given depth.

xyplot(top ~ p.q50 | variable, groups=taxonname, data=g.slab, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=g.slab$p.q25, upper=g.slab$p.q75, ylim=c(155,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=g.slab$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(4,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps,
       auto.key=list(columns=3, lines=TRUE, points=FALSE)
)
