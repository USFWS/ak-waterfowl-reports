#code to make object for report
library(tidyverse)
library(AKaerial)
library(kableExtra)
library(tinytex)
library(zoo)
library(tmap)

#define species results function
sppfig <- function(spp = "EMGO", index1 = "itotal", index2 = "ibb"){
  if(spp %in% unique(AKaerial::YKGHistoric$combined$Species)){
    df <- AKaerial::YKGHistoric$combined
    }else{
      df <- AKaerial::YKDHistoric$combined
    } 
  df <- df |>
    filter(Species == spp) |>
    select(Year, index = index1, se = paste0(index1,".se"), 
           index2 = index2, 
           se2 = paste0(index2,".se")) |> 
    mutate(upper = index +1.96*se, lower = index - 1.96*se, upper2 = index2+1.96*se2, 
           lower2 = index2 - 1.96*se2)
  df2 <- read_csv(file = paste0("data/plot_trends/data/",spp,"_bootfit.csv"))
  gg <- ggplot(data = df2) + 
    geom_ribbon(aes(x=Year, ymin=pmax(0, lower), ymax = upper), fill = "lightgray", 
                alpha = 0.5) + 
    geom_line(aes(x=Year, y=mean), lwd = 1) + 
    geom_pointrange(data = df, aes(x=Year-0.1, y=index, ymin=pmax(0,lower), ymax = upper, 
                                   col = "Indicated Total")) + 
    geom_pointrange(data = df, aes(x=Year+0.1, y=index2, ymin=pmax(0,lower2), ymax = upper2, 
                                   col = "Indicated Breeding")) +
    labs(x = "Year", y = "Population Index") +
    scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 4)) + 
    #scale_y_continuous discontinuous use of scientific notation to abbreviate large-numbered axis labels  
    scale_y_continuous(labels = scales::comma) +
    scale_colour_manual(values=c("Indicated Total"= "black", "Indicated Breeding" = "darkgray")) +  
    theme_bw() + 
    theme(legend.position = "top", legend.title=element_blank()) +
    theme(legend.position = "top", legend.title=element_blank(), legend.text=element_text(size=11)) +
    theme(text=element_text(family="Times")) +
    theme(axis.title=element_text(size=13,face="bold")) +
    theme(axis.text.x = element_text(size=11), axis.text.y = element_text(size=11))
  return(gg)
}
#test
#sppfig()
################################################################################
#Table 1 AIRCREWS AND DATES OF ANNUAL SURVEYS - is currently drawn from a .csv file Heather created. Ultimately, it should come directly from the data.
table1 <- read.csv("data/Table1_YKDCrewDates.csv", stringsAsFactors = F, header=TRUE)
dat <- AKaerial::CompositeLong()

#Data objects and layers for making Fig. 1 - Study area map of the ACP with strata, most recent year transects, and village names
effort = YKDHistoric$expanded.table %>% filter(Year==max(YKDHistoric$expanded.table$Year)) %>% group_by(strata, Observer) %>% summarize (layer = mean(layer.area), sampled = mean(total.area), sf = sampled/layer) %>% group_by(strata) %>% summarize(layer=mean(layer), sf=sum(sf))


this.year = max(YKDHistoric$combined$Year)

this.layer = MasterFileList$LAYER[MasterFileList$AREA=="YKD" & MasterFileList$YEAR==this.year]

#Making a study area map with strata, transects, and relevant cities
#creates a list of desired towns to display on study area map
ykd_cities <- data.frame(
  city = c("Newtok", "Hooper Bay", "Chevak", "Mertarvik", "Kotlik", "Emmonak", "Kipnuk", "Scammon Bay", "Chefornak", "Nightmute"),
  lat = c(60.9444, 61.528980, 61.527673, 60.82504559, 63.0358, 62.7772477, 59.937619, 61.843, 60.159070, 60.4914),
  lon = c(-164.6417, -166.096196, -165.578702, -164.471214, -163.5597, -164.5319605, -164.043926, -165.582, -164.269437, -164.8261))        


ykd_cities = st_as_sf(ykd_cities, coords = c("lon","lat"))

st_crs(ykd_cities) = 4269
#Data objects created from YKD survey data in the RDR: x-y coordinates associated with species observations, used to make maps
ykd.strata = read_sf("data/YKD_DesignStrata.gpkg") |>
  filter(STRATNAME != "Nonhabitat") |>
  st_transform(crs=4326)
ykd.transects=read_sf("data/YKD_DesignTrans.gpkg", this.layer) |>
  st_transform(crs=4326)


#Data objects and layers for making Fig. 1 - Study area map of the ACP with strata, most recent year transects, and village names
tmap_mode("plot")

new_bbox = st_bbox(ykd.strata)
new_bbox[1]=-162.5
new_bbox[2]=59.5
new_bbox[3]=-166.5
new_bbox[4]=63.5
new_bbox=st_as_sfc(new_bbox)

fig1 = tm_shape(ykd.strata, bbox = new_bbox) +
  tm_polygons(fill = "STRATNAME",
              fill_alpha = 0.5,
              fill.legend = tm_legend(title= "Stratum", position = c("RIGHT", "TOP"),
                                      width=5, height=6, title.size=.95, frame = FALSE, text.size = 0.85)) +
  tm_shape(ykd.transects) +
  tm_lines(col_alpha=.5) +
  tm_shape(ykd_cities) +
  tm_symbols(size = 0.5, col = "black") +
  tm_graticules(lines=FALSE) +
  tm_text("city", fontface="bold", size=1.0, ymod=.9, xmod=.5) +
  tm_compass(position = c("LEFT", "TOP"))

tmap_save(fig1, "data/fig1.png")

#Reading species-level tables here to allow for n-year averaging

EMGO <- YKGHistoric$combined %>%
  filter(Species=="EMGO") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

EMGO3 = zoo::rollmean(EMGO$itotal, k = 3)
EMGO3se = sqrt(zoo::rollmean(EMGO$itotal.se^2, k=3)[length(EMGO3)])
EMGO3 = EMGO3[length(EMGO3)]
EMGOh = zoo::rollmean(EMGO$itotal, k = length(EMGO$itotal), na.rm=TRUE)
EMGOhse = sqrt(zoo::rollmean(EMGO$itotal.se^2, k=length(EMGO$itotal), na.rm=TRUE))
EMGOentry = data.frame(Species="Emperor goose", 
                       Code="EMGO",
                       Index="ITB",
                       Y3=round(EMGO3,0),
                       Y3se=round(EMGO3se,0),
                       LT=round(EMGOh,0),
                       LTse=round(EMGOhse,0))

GWFG <- YKGHistoric$combined %>%
  filter(Species=="GWFG") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

GWFG3 = zoo::rollmean(GWFG$itotal, k = 3)
GWFG3se = sqrt(zoo::rollmean(GWFG$itotal.se^2, k=3)[length(GWFG3)])
GWFG3 = GWFG3[length(GWFG3)]
GWFGh = zoo::rollmean(GWFG$itotal, k = length(GWFG$itotal), na.rm=TRUE)
GWFGhse = sqrt(zoo::rollmean(GWFG$itotal.se^2, k=length(GWFG$itotal), na.rm=TRUE))
GWFGentry = data.frame(Species="Greater white-fronted goose", 
                       Code="GWFG",
                       Index="ITB",
                       Y3=round(GWFG3,0),
                       Y3se=round(GWFG3se,0),
                       LT=round(GWFGh,0),
                       LTse=round(GWFGhse,0))


BRAN <- YKGHistoric$combined %>%
  filter(Species=="BRAN") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

BRAN3 = zoo::rollmean(BRAN$itotal, k = 3)
BRAN3se = sqrt(zoo::rollmean(BRAN$itotal.se^2, k=3)[length(BRAN3)])
BRAN3 = BRAN3[length(BRAN3)]
BRANh = zoo::rollmean(BRAN$itotal, k = length(BRAN$itotal), na.rm=TRUE)
BRANhse = sqrt(zoo::rollmean(BRAN$itotal.se^2, k=length(BRAN$itotal), na.rm=TRUE))
BRANentry = data.frame(Species="Brant", 
                       Code="BRAN",
                       Index="ITB",
                       Y3=round(BRAN3,0),
                       Y3se=round(BRAN3se,0),
                       LT=round(BRANh,0),
                       LTse=round(BRANhse,0))


CCGO <- YKGHistoric$combined %>%
  filter(Species=="CCGO") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

CCGO3 = zoo::rollmean(CCGO$itotal, k = 3)
CCGO3se = sqrt(zoo::rollmean(CCGO$itotal.se^2, k=3)[length(CCGO3)])
CCGO3 = CCGO3[length(CCGO3)]
CCGOh = zoo::rollmean(CCGO$itotal, k = length(CCGO$itotal), na.rm=TRUE)
CCGOhse = sqrt(zoo::rollmean(CCGO$itotal.se^2, k=length(CCGO$itotal), na.rm=TRUE))
CCGOentry = data.frame(Species="Cackling Canada goose", 
                       Code="CCGO",
                       Index="ITB",
                       Y3=round(CCGO3,0),
                       Y3se=round(CCGO3se,0),
                       LT=round(CCGOh,0),
                       LTse=round(CCGOhse,0))


TAVS <- YKGHistoric$combined %>%
  filter(Species=="TAVS") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

TAVS3 = zoo::rollmean(TAVS$itotal, k = 3)
TAVS3se = sqrt(zoo::rollmean(TAVS$itotal.se^2, k=3)[length(TAVS3)])
TAVS3 = TAVS3[length(TAVS3)]
TAVSh = zoo::rollmean(TAVS$itotal, k = length(TAVS$itotal), na.rm=TRUE)
TAVShse = sqrt(zoo::rollmean(TAVS$itotal.se^2, k=length(TAVS$itotal), na.rm=TRUE))
TAVSentry = data.frame(Species="Taverner's Canada goose", 
                       Code="TAVS",
                       Index="ITB",
                       Y3=round(TAVS3,0),
                       Y3se=round(TAVS3se,0),
                       LT=round(TAVSh,0),
                       LTse=round(TAVShse,0))


SWAN <- YKGHistoric$combined %>%
  filter(Species=="SWAN") %>%
  select(Year, sing1pair2, sing1pair2.se, total, total.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

SWAN3 = zoo::rollmean(SWAN$total, k = 3)
SWAN3se = sqrt(zoo::rollmean(SWAN$total.se^2, k=3)[length(SWAN3)])
SWAN3 = SWAN3[length(SWAN3)]
SWANh = zoo::rollmean(SWAN$total, k = length(SWAN$total), na.rm=TRUE)
SWANhse = sqrt(zoo::rollmean(SWAN$total.se^2, k=length(SWAN$total), na.rm=TRUE))
SWANentry = data.frame(Species="Tundra swan", 
                       Code="SWAN",
                       Index="TB",
                       Y3=round(SWAN3,0),
                       Y3se=round(SWAN3se,0),
                       LT=round(SWANh,0),
                       LTse=round(SWANhse,0))


NSHO <- YKDHistoric$combined %>%
  filter(Species=="NSHO") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

NSHO3 = zoo::rollmean(NSHO$itotal, k = 3)
NSHO3se = sqrt(zoo::rollmean(NSHO$itotal.se^2, k=3)[length(NSHO3)])
NSHO3 = NSHO3[length(NSHO3)]
NSHOh = zoo::rollmean(NSHO$itotal, k = length(NSHO$itotal), na.rm=TRUE)
NSHOhse = sqrt(zoo::rollmean(NSHO$itotal.se^2, k=length(NSHO$itotal), na.rm=TRUE))
NSHOentry = data.frame(Species="Northern shoveler", 
                       Code="NSHO",
                       Index="ITB",
                       Y3=round(NSHO3,0),
                       Y3se=round(NSHO3se,0),
                       LT=round(NSHOh,0),
                       LTse=round(NSHOhse,0))


AMWI <- YKDHistoric$combined %>%
  filter(Species=="AMWI") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

AMWI3 = zoo::rollmean(AMWI$itotal, k = 3)
AMWI3se = sqrt(zoo::rollmean(AMWI$itotal.se^2, k=3)[length(AMWI3)])
AMWI3 = AMWI3[length(AMWI3)]
AMWIh = zoo::rollmean(AMWI$itotal, k = length(AMWI$itotal), na.rm=TRUE)
AMWIhse = sqrt(zoo::rollmean(AMWI$itotal.se^2, k=length(AMWI$itotal), na.rm=TRUE))
AMWIentry = data.frame(Species="American wigeon", 
                       Code="AMWI",
                       Index="ITB",
                       Y3=round(AMWI3,0),
                       Y3se=round(AMWI3se,0),
                       LT=round(AMWIh,0),
                       LTse=round(AMWIhse,0))


MALL <- YKDHistoric$combined %>%
  filter(Species=="MALL") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

MALL3 = zoo::rollmean(MALL$itotal, k = 3)
MALL3se = sqrt(zoo::rollmean(MALL$itotal.se^2, k=3)[length(MALL3)])
MALL3 = MALL3[length(MALL3)]
MALLh = zoo::rollmean(MALL$itotal, k = length(MALL$itotal), na.rm=TRUE)
MALLhse = sqrt(zoo::rollmean(MALL$itotal.se^2, k=length(MALL$itotal), na.rm=TRUE))
MALLentry = data.frame(Species="Mallard", 
                       Code="MALL",
                       Index="ITB",
                       Y3=round(MALL3,0),
                       Y3se=round(MALL3se,0),
                       LT=round(MALLh,0),
                       LTse=round(MALLhse,0))

NOPI <- YKDHistoric$combined %>%
  filter(Species=="NOPI") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

NOPI3 = zoo::rollmean(NOPI$itotal, k = 3)
NOPI3se = sqrt(zoo::rollmean(NOPI$itotal.se^2, k=3)[length(NOPI3)])
NOPI3 = NOPI3[length(NOPI3)]
NOPIh = zoo::rollmean(NOPI$itotal, k = length(NOPI$itotal), na.rm=TRUE)
NOPIhse = sqrt(zoo::rollmean(NOPI$itotal.se^2, k=length(NOPI$itotal), na.rm=TRUE))
NOPIentry = data.frame(Species="Northern pintail", 
                       Code="NOPI",
                       Index="ITB",
                       Y3=round(NOPI3,0),
                       Y3se=round(NOPI3se,0),
                       LT=round(NOPIh,0),
                       LTse=round(NOPIhse,0))


GWTE <- YKDHistoric$combined %>%
  filter(Species=="GWTE") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

GWTE3 = zoo::rollmean(GWTE$itotal, k = 3)
GWTE3se = sqrt(zoo::rollmean(GWTE$itotal.se^2, k=3)[length(GWTE3)])
GWTE3 = GWTE3[length(GWTE3)]
GWTEh = zoo::rollmean(GWTE$itotal, k = length(GWTE$itotal), na.rm=TRUE)
GWTEhse = sqrt(zoo::rollmean(GWTE$itotal.se^2, k=length(GWTE$itotal), na.rm=TRUE))
GWTEentry = data.frame(Species="American green-winged teal", 
                       Code="GWTE",
                       Index="ITB",
                       Y3=round(GWTE3,0),
                       Y3se=round(GWTE3se,0),
                       LT=round(GWTEh,0),
                       LTse=round(GWTEhse,0))


CANV <- YKDHistoric$combined %>%
  filter(Species=="CANV") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

CANV3 = zoo::rollmean(CANV$itotal, k = 3)
CANV3se = sqrt(zoo::rollmean(CANV$itotal.se^2, k=3)[length(CANV3)])
CANV3 = CANV3[length(CANV3)]
CANVh = zoo::rollmean(CANV$itotal, k = length(CANV$itotal), na.rm=TRUE)
CANVhse = sqrt(zoo::rollmean(CANV$itotal.se^2, k=length(CANV$itotal), na.rm=TRUE))
CANVentry = data.frame(Species="Surf scoter", 
                       Code="CANV",
                       Index="ITB",
                       Y3=round(CANV3,0),
                       Y3se=round(CANV3se,0),
                       LT=round(CANVh,0),
                       LTse=round(CANVhse,0))


UNSC <- YKDHistoric$combined %>%
  filter(Species=="UNSC") %>%
  select(Year, sing1pair2, sing1pair2.se, total, total.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

UNSC3 = zoo::rollmean(UNSC$total, k = 3)
UNSC3se = sqrt(zoo::rollmean(UNSC$total.se^2, k=3)[length(UNSC3)])
UNSC3 = UNSC3[length(UNSC3)]
UNSCh = zoo::rollmean(UNSC$total, k = length(UNSC$total), na.rm=TRUE)
UNSChse = sqrt(zoo::rollmean(UNSC$total.se^2, k=length(UNSC$total), na.rm=TRUE))
UNSCentry = data.frame(Species="Scaup species", 
                       Code="UNSC",
                       Index="TB",
                       Y3=round(UNSC3,0),
                       Y3se=round(UNSC3se,0),
                       LT=round(UNSCh,0),
                       LTse=round(UNSChse,0))


SPEI <- YKDHistoric$combined %>%
  filter(Species=="SPEI") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

SPEI3 = zoo::rollmean(SPEI$itotal, k = 3)
SPEI3se = sqrt(zoo::rollmean(SPEI$itotal.se^2, k=3)[length(SPEI3)])
SPEI3 = SPEI3[length(SPEI3)]
SPEIh = zoo::rollmean(SPEI$itotal, k = length(SPEI$itotal), na.rm=TRUE)
SPEIhse = sqrt(zoo::rollmean(SPEI$itotal.se^2, k=length(SPEI$itotal), na.rm=TRUE))
SPEIentry = data.frame(Species="Spectacled eider", 
                       Code="SPEI",
                       Index="ITB",
                       Y3=round(SPEI3,0),
                       Y3se=round(SPEI3se,0),
                       LT=round(SPEIh,0),
                       LTse=round(SPEIhse,0))


COEI <- YKDHistoric$combined %>%
  filter(Species=="COEI") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

COEI3 = zoo::rollmean(COEI$itotal, k = 3)
COEI3se = sqrt(zoo::rollmean(COEI$itotal.se^2, k=3)[length(COEI3)])
COEI3 = COEI3[length(COEI3)]
COEIh = zoo::rollmean(COEI$itotal, k = length(COEI$itotal), na.rm=TRUE)
COEIhse = sqrt(zoo::rollmean(COEI$itotal.se^2, k=length(COEI$itotal), na.rm=TRUE))
COEIentry = data.frame(Species="Common eider", 
                       Code="COEI",
                       Index="ITB",
                       Y3=round(COEI3,0),
                       Y3se=round(COEI3se,0),
                       LT=round(COEIh,0),
                       LTse=round(COEIhse,0))


BLSC <- YKDHistoric$combined %>%
  filter(Species=="BLSC") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

BLSC3 = zoo::rollmean(BLSC$itotal, k = 3)
BLSC3se = sqrt(zoo::rollmean(BLSC$itotal.se^2, k=3)[length(BLSC3)])
BLSC3 = BLSC3[length(BLSC3)]
BLSCh = zoo::rollmean(BLSC$itotal, k = length(BLSC$itotal), na.rm=TRUE)
BLSChse = sqrt(zoo::rollmean(BLSC$itotal.se^2, k=length(BLSC$itotal), na.rm=TRUE))
BLSCentry = data.frame(Species="Black scoter", 
                       Code="BLSC",
                       Index="ITB",
                       Y3=round(BLSC3,0),
                       Y3se=round(BLSC3se,0),
                       LT=round(BLSCh,0),
                       LTse=round(BLSChse,0))


LTDU <- YKDHistoric$combined %>%
  filter(Species=="LTDU") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

LTDU3 = zoo::rollmean(LTDU$itotal, k = 3)
LTDU3se = sqrt(zoo::rollmean(LTDU$itotal.se^2, k=3)[length(LTDU3)])
LTDU3 = LTDU3[length(LTDU3)]
LTDUh = zoo::rollmean(LTDU$itotal, k = length(LTDU$itotal), na.rm=TRUE)
LTDUhse = sqrt(zoo::rollmean(LTDU$itotal.se^2, k=length(LTDU$itotal), na.rm=TRUE))
LTDUentry = data.frame(Species="Long-tailed duck", 
                       Code="LTDU",
                       Index="ITB",
                       Y3=round(LTDU3,0),
                       Y3se=round(LTDU3se,0),
                       LT=round(LTDUh,0),
                       LTse=round(LTDUhse,0))


RBME <- YKDHistoric$combined %>%
  filter(Species=="RBME") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

RBME3 = zoo::rollmean(RBME$itotal, k = 3)
RBME3se = sqrt(zoo::rollmean(RBME$itotal.se^2, k=3)[length(RBME3)])
RBME3 = RBME3[length(RBME3)]
RBMEh = zoo::rollmean(RBME$itotal, k = length(RBME$itotal), na.rm=TRUE)
RBMEhse = sqrt(zoo::rollmean(RBME$itotal.se^2, k=length(RBME$itotal), na.rm=TRUE))
RBMEentry = data.frame(Species="Red-breasted merganser", 
                       Code="RBME",
                       Index="ITB",
                       Y3=round(RBME3,0),
                       Y3se=round(RBME3se,0),
                       LT=round(RBMEh,0),
                       LTse=round(RBMEhse,0))

SACR <- YKGHistoric$combined %>%
  filter(Species=="SACR") %>%
  select(Year, ibb, ibb.se, itotal, itotal.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

SACR3 = zoo::rollmean(SACR$itotal, k = 3)
SACR3se = sqrt(zoo::rollmean(SACR$itotal.se^2, k=3)[length(SACR3)])
SACR3 = SACR3[length(SACR3)]
SACRh = zoo::rollmean(SACR$itotal, k = length(SACR$itotal), na.rm=TRUE)
SACRhse = sqrt(zoo::rollmean(SACR$itotal.se^2, k=length(SACR$itotal), na.rm=TRUE))
SACRentry = data.frame(Species="Sandhill crane", 
                       Code="SACR",
                       Index="ITB",
                       Y3=round(SACR3,0),
                       Y3se=round(SACR3se,0),
                       LT=round(SACRh,0),
                       LTse=round(SACRhse,0))

JAEG <- YKDHistoric$combined %>%
  filter(Species=="JAEG") %>%
  select(Year, sing1pair2, sing1pair2.se, total, total.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

JAEG3 = zoo::rollmean(JAEG$total, k = 3)
JAEG3se = sqrt(zoo::rollmean(JAEG$total.se^2, k=3)[length(JAEG3)])
JAEG3 = JAEG3[length(JAEG3)]
JAEGh = zoo::rollmean(JAEG$total, k = length(JAEG$total), na.rm=TRUE)
JAEGhse = sqrt(zoo::rollmean(JAEG$total.se^2, k=length(JAEG$total), na.rm=TRUE))
JAEGentry = data.frame(Species="Jaeger species", 
                       Code="JAEG",
                       Index="TB",
                       Y3=round(JAEG3,0),
                       Y3se=round(JAEG3se,0),
                       LT=round(JAEGh,0),
                       LTse=round(JAEGhse,0))


RTLO <- YKDHistoric$combined %>%
  filter(Species=="RTLO") %>%
  select(Year, sing1pair2, sing1pair2.se, total, total.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

#3-year (k=3) average on total index (RTLO$total)
RTLO3 = zoo::rollmean(RTLO$total, k = 3)
#3-year average variance turned into SE 
RTLO3se = sqrt(zoo::rollmean(RTLO$total.se^2, k=3)[length(RTLO3)])

#Now just grab the last one
RTLO3 = RTLO3[length(RTLO3)]

#Historic average (k=length()) total index (RTLO$total), removing NAs (na.rm=TRUE)
RTLOh = zoo::rollmean(RTLO$total, k = length(RTLO$total), na.rm=TRUE)
RTLOhse = sqrt(zoo::rollmean(RTLO$total.se^2, k=length(RTLO$total), na.rm=TRUE))

RTLOentry = data.frame(Species="Red-throated loon", 
                       Code="RTLO",
                       Index="TB",
                       Y3=round(RTLO3,0),
                       Y3se=round(RTLO3se,0),
                       LT=round(RTLOh,0),
                       LTse=round(RTLOhse,0))

PALO <- YKDHistoric$combined %>%
  filter(Species=="PALO") %>%
  select(Year, sing1pair2, sing1pair2.se, total, total.se) %>%
  complete(Year=c(min(Year):max(Year)), fill=list(NA)) %>%
  mutate(Year=as.character(Year))

PALO3 = zoo::rollmean(PALO$total, k = 3)
PALO3se = sqrt(zoo::rollmean(PALO$total.se^2, k=3)[length(PALO3)])

PALO3 = PALO3[length(PALO3)]

PALOh = zoo::rollmean(PALO$total, k = length(PALO$total), na.rm=TRUE)
PALOhse = sqrt(zoo::rollmean(PALO$total.se^2, k=length(PALO$total), na.rm=TRUE))

PALOentry = data.frame(Species="Pacific loon", 
                       Code="PALO",
                       Index="TB",
                       Y3=round(PALO3,0),
                       Y3se=round(PALO3se,0),
                       LT=round(PALOh,0),
                       LTse=round(PALOhse,0))



#Table 2 rbind calls on all the summary 'entry' data frames for each species to populate the rows of Table 2. the rbind species list does need to be in desired order (in our case taxonomic based on most recent AOS)
table2 = rbind(EMGOentry, GWFGentry, BRANentry, CCGOentry, TAVSentry, SWANentry, NSHOentry, AMWIentry,  MALLentry, NOPIentry, GWTEentry, CANVentry, UNSCentry, SPEIentry, COEIentry, BLSCentry, LTDUentry, RBMEentry, SACRentry, JAEGentry, RTLOentry, PALOentry) #ADD SEOW? CORA? GADW? RNGR?

spplist <- table2[,1:2]
spptext <- table2[,1]
sppAOU <- table2[,2]
#TRENDS code from Erik O. This code pulls from a folder in our working directory that has plot_trends .csv output from his observer effects GAM model run elsewhere. "mutate" is a function that allows the same work to be done on each species, but just specifying the species.

path = "data/plot_trends/data/"
sppdf <- data.frame(NULL)
for( i in spplist$Code){
  tmp <- read_csv(file = paste0(path,i,"_trend.csv")) |>
    last() |>
    mutate(Species = i)
  sppdf <- rbind(sppdf, tmp)
}
#Code below creates 95% confidence intervals in parentheses to be displayed right after long-term average trend point estimate in Table 2.
sppdf$confInt <- paste0(format(round(sppdf$mean, 2), nsmall = 2), " (",
                        format(round(sppdf$lower, 2), nsmall = 2), " - ", 
                        format(round(sppdf$upper, 2), nsmall = 2), ")")
sppdf$P <- format(round(sppdf$P, 2), nsmall = 2)
sppdf <- select(sppdf, Species, Trend=confInt, P)
sppdf2 <- sppdf
#Adds last column to Table 2 with trend and CIs.
table2 <- table2 |>
  # cbind(table2, select(sppdf, -Species)) |>
  # table2 <- add_column(table2, sppdf$Trend, .after = 7) |>
  #   add_column(sppdf$P, .after = Trend) |>
  select(-Code)
################################################################################
## make table 3 for 10-year trend
sppdf <- data.frame(NULL)
for( i in spplist$Code){
  tmp <- read_csv(file = paste0(path,i,"_trend.csv")) |>
    nth(n=10) |>
    mutate(Species = i)
  sppdf <- rbind(sppdf, tmp)
}
#Code below creates 95% confidence intervals in parentheses to be displayed right after long-term average trend point estimate in Table 2.
sppdf$confInt <- paste0(format(round(sppdf$mean, 2), nsmall = 2), " (",
                        format(round(sppdf$lower, 2), nsmall = 2), " - ", 
                        format(round(sppdf$upper, 2), nsmall = 2), ")")
sppdf$P <- format(round(sppdf$P, 2), nsmall = 2)
sppdf <- select(sppdf, Species, Trend=confInt, P)
table3 <- table2 |> select(Species, Index) |>
  cbind(select(sppdf, Trend10 = Trend, P10 = P)) |>
  cbind(select(sppdf2, -Species))
################################################################################
##write files for appendices
#make figures
for(i in 1:length(sppAOU)){
  s <- sppfig(spp = sppAOU[i])
  ggsave(plot = s, filename=paste0("data/plot_trends/figures/",sppAOU[i],"index.png"))
  if(sppAOU[i] != "TAVS"){
    m <- AKaerial::ObsMap(area="YKG", species=sppAOU[i])
    ggsave(plot=m, filename=paste0("data/plot_trends/figures/",sppAOU[i],"map.png"))
  }
}
################################################################################
#write species-specific output tables to file
spplist[,1] <- str_remove(spplist[,1], "/") |>
  str_remove(" ") |> str_remove("'")
for(i in 1:dim(spplist)[1]){
  write.csv(get(spplist[i,2]), file=paste0("results/Table_", i+2, "_", 
                                           spplist[i,1], ".csv"), 
            quote = FALSE, row.names=FALSE)
}
save.image(file="data/YKD_Report.RData")
