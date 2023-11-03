# setup
library(terra)
library(sf)

indir <- file.path("data")
dir.create(indir, showWarnings = FALSE)
outdir <- file.path("output")
dir.create(outdir, showWarnings = FALSE)

west_africa <- st_read(file.path(outdir, "west_africa.gpkg"))
west_africa_c <- st_centroid(west_africa)
fatalities_class <- rast(file.path(outdir, "fatalitites_class.tif"))

png(file.path(outdir, "classified_density.png"), width=9, height=7, units="in", res=300)
pax=list(side=2:3, tick=c(1,4), lab=1:2, retro=TRUE)
plot(fatalities_class, main = "classified fatalitites density 2018-2022",
     plg = list(loc = "topright"), pax=pax)
plot(west_africa["name"], add = TRUE, col = NA, lwd = 1)
text(vect(west_africa_c), labels=west_africa_c$name, add = TRUE, halo = TRUE, cex = 0.8)
plot(graticule(5, 5), add = T, lwd = 0.2)
dev.off()
