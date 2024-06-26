library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)

#Read in GADM Shapefile and Rainfall clusters
cl2test <-read_sf(dsn = ".", layer = "rainfallbilinear")
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents")
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")



#Set working directory, the tif files could be instead organised into folders instead which are then uploaded in bulk and automatically stacked
# This would save a lot of code and computational space, but is currently written this way due to ease of organisation at the time
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Rainfall")
#Read in tif files for each year by month
Jan2<-'chirps-v2.0_2002.01.tif' 
Jan3<-'chirps-v2.0_2003.01.tif' 
Jan4<-'chirps-v2.0_2004.01.tif' 
Jan5<-'chirps-v2.0_2005.01.tif' 
Jan6<-'chirps-v2.0_2006.01.tif' 
Jan7<-'chirps-v2.0_2007.01.tif' 
Jan8<-'chirps-v2.0_2008.01.tif' 
Jan9<-'chirps-v2.0_2009.01.tif' 
Jan10<-'chirps-v2.0_2010.01.tif' 
Jan11<-'chirps-v2.0_2011.01.tif' 
Jan12<-'chirps-v2.0_2012.01.tif' 
Jan13<-'chirps-v2.0_2013.01.tif' 
Jan14<-'chirps-v2.0_2014.01.tif' 
Jan15<-'chirps-v2.0_2015.01.tif' 
Jan16<-'chirps-v2.0_2016.01.tif' 
Jan17<-'chirps-v2.0_2017.01.tif' 
Jan18<-'chirps-v2.0_2018.01.tif' 
Jan19<-'chirps-v2.0_2019.01.tif' 
Jan20<-'chirps-v2.0_2020.01.tif' 
Jan21<-'chirps-v2.0_2021.01.tif' 
Jan22<-'chirps-v2.0_2022.01.tif' 
Jan2<- raster(Jan2)
Jan3<- raster(Jan3)
Jan4<- raster(Jan4)
Jan5<- raster(Jan5)
Jan6<- raster(Jan6)
Jan7<- raster(Jan7)
Jan8<- raster(Jan8)
Jan9<- raster(Jan9)
Jan10<- raster(Jan10)
Jan11<- raster(Jan11)
Jan12<- raster(Jan12)
Jan13<- raster(Jan13)
Jan14<- raster(Jan14)
Jan15<- raster(Jan15)
Jan16<- raster(Jan16)
Jan17<- raster(Jan17)
Jan18<- raster(Jan18)
Jan19<- raster(Jan19)
Jan20<- raster(Jan20)
Jan21<- raster(Jan21)
Jan22<- raster(Jan22)
# Stack these, calculate monthly rainfall adn then extract average by district to turn into a shapefile
mean_jan <- stack(Jan2, Jan3,Jan4,Jan5,Jan6,Jan7,Jan8,Jan9,Jan10, Jan11, Jan12,
Jan13, Jan14, Jan15, Jan16, Jan17, Jan18, Jan19, Jan20, Jan21, Jan22)
mean_jan <- calc(mean_jan, mean)
mean_jan<-data.frame(shape,extract(mean_jan, shape, fun=mean, na.rm=T, touches=TRUE))
mean_jan$Jan<-mean_jan$extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE.


#Repeat for each month: Feb
Feb2<-'chirps-v2.0_2002.02.tif' 
Feb3<-'chirps-v2.0_2003.02.tif' 
Feb4<-'chirps-v2.0_2004.02.tif' 
Feb5<-'chirps-v2.0_2005.02.tif' 
Feb6<-'chirps-v2.0_2006.02.tif' 
Feb7<-'chirps-v2.0_2007.02.tif' 
Feb8<-'chirps-v2.0_2008.02.tif' 
Feb9<-'chirps-v2.0_2009.02.tif' 
Feb10<-'chirps-v2.0_2010.02.tif' 
Feb11<-'chirps-v2.0_2011.02.tif' 
Feb12<-'chirps-v2.0_2012.02.tif' 
Feb13<-'chirps-v2.0_2013.02.tif' 
Feb14<-'chirps-v2.0_2014.02.tif' 
Feb15<-'chirps-v2.0_2015.02.tif' 
Feb16<-'chirps-v2.0_2016.02.tif' 
Feb17<-'chirps-v2.0_2017.02.tif' 
Feb18<-'chirps-v2.0_2018.02.tif' 
Feb19<-'chirps-v2.0_2019.02.tif' 
Feb20<-'chirps-v2.0_2020.02.tif' 
Feb21<-'chirps-v2.0_2021.02.tif' 
Feb22<-'chirps-v2.0_2022.02.tif' 

Feb2<- raster(Feb2)
Feb3<- raster(Feb3)
Feb4<- raster(Feb4)
Feb5<- raster(Feb5)
Feb6<- raster(Feb6)
Feb7<- raster(Feb7)
Feb8<- raster(Feb8)
Feb9<- raster(Feb9)
Feb10<- raster(Feb10)
Feb11<- raster(Feb11)
Feb12<- raster(Feb12)
Feb13<- raster(Feb13)
Feb14<- raster(Feb14)
Feb15<- raster(Feb15)
Feb16<- raster(Feb16)
Feb17<- raster(Feb17)
Feb18<- raster(Feb18)
Feb19<- raster(Feb19)
Feb20<- raster(Feb20)
Feb21<- raster(Feb21)
Feb22<- raster(Feb22)
mean_Feb <- stack(  Feb2, Feb3,Feb4,Feb5,Feb6,Feb7,Feb8,Feb9,Feb10, Feb11, Feb12,
  Feb13, Feb14, Feb15, Feb16, Feb17, Feb18, Feb19, Feb20, Feb21, Feb22)
mean_Feb <- calc(mean_Feb, mean)
mean_Feb<-data.frame(shape,extract(mean_Feb, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Feb$Feb<-mean_Feb$extract.mean_Feb..shape..fun...mean..na.rm...T..touches...TRUE.

#Mar
Mar2<-'chirps-v2.0_2002.03.tif' 
Mar3<-'chirps-v2.0_2003.03.tif' 
Mar4<-'chirps-v2.0_2004.03.tif' 
Mar5<-'chirps-v2.0_2005.03.tif' 
Mar6<-'chirps-v2.0_2006.03.tif' 
Mar7<-'chirps-v2.0_2007.03.tif' 
Mar8<-'chirps-v2.0_2008.03.tif' 
Mar9<-'chirps-v2.0_2009.03.tif' 
Mar10<-'chirps-v2.0_2010.03.tif' 
Mar11<-'chirps-v2.0_2011.03.tif' 
Mar12<-'chirps-v2.0_2012.03.tif' 
Mar13<-'chirps-v2.0_2013.03.tif' 
Mar14<-'chirps-v2.0_2014.03.tif' 
Mar15<-'chirps-v2.0_2015.03.tif' 
Mar16<-'chirps-v2.0_2016.03.tif' 
Mar17<-'chirps-v2.0_2017.03.tif' 
Mar18<-'chirps-v2.0_2018.03.tif' 
Mar19<-'chirps-v2.0_2019.03.tif' 
Mar20<-'chirps-v2.0_2020.03.tif' 
Mar21<-'chirps-v2.0_2021.03.tif' 
Mar22<-'chirps-v2.0_2022.03.tif' 

Mar2<- raster(Mar2)
Mar3<- raster(Mar3)
Mar4<- raster(Mar4)
Mar5<- raster(Mar5)
Mar6<- raster(Mar6)
Mar7<- raster(Mar7)
Mar8<- raster(Mar8)
Mar9<- raster(Mar9)
Mar10<- raster(Mar10)
Mar11<- raster(Mar11)
Mar12<- raster(Mar12)
Mar13<- raster(Mar13)
Mar14<- raster(Mar14)
Mar15<- raster(Mar15)
Mar16<- raster(Mar16)
Mar17<- raster(Mar17)
Mar18<- raster(Mar18)
Mar19<- raster(Mar19)
Mar20<- raster(Mar20)
Mar21<- raster(Mar21)
Mar22<- raster(Mar22)
mean_Mar <- stack(Mar2, Mar3,Mar4,Mar5,Mar6,Mar7,Mar8,Mar9,Mar10, Mar11, Mar12,
Mar13, Mar14, Mar15, Mar16, Mar17, Mar18, Mar19, Mar20, Mar21, Mar22)
mean_Mar <- calc(mean_Mar, mean)
mean_Mar<-data.frame(shape,extract(mean_Mar, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Mar$Mar<-mean_Mar$extract.mean_Mar..shape..fun...mean..na.rm...T..touches...TRUE.

#Apr
Apr2<-'chirps-v2.0_2002.04.tif' 
Apr3<-'chirps-v2.0_2003.04.tif' 
Apr4<-'chirps-v2.0_2004.04.tif' 
Apr5<-'chirps-v2.0_2005.04.tif' 
Apr6<-'chirps-v2.0_2006.04.tif' 
Apr7<-'chirps-v2.0_2007.04.tif' 
Apr8<-'chirps-v2.0_2008.04.tif' 
Apr9<-'chirps-v2.0_2009.04.tif' 
Apr10<-'chirps-v2.0_2010.04.tif' 
Apr11<-'chirps-v2.0_2011.04.tif' 
Apr12<-'chirps-v2.0_2012.04.tif' 
Apr13<-'chirps-v2.0_2013.04.tif' 
Apr14<-'chirps-v2.0_2014.04.tif' 
Apr15<-'chirps-v2.0_2015.04.tif' 
Apr16<-'chirps-v2.0_2016.04.tif' 
Apr17<-'chirps-v2.0_2017.04.tif' 
Apr18<-'chirps-v2.0_2018.04.tif' 
Apr19<-'chirps-v2.0_2019.04.tif' 
Apr20<-'chirps-v2.0_2020.04.tif' 
Apr21<-'chirps-v2.0_2021.04.tif' 
Apr22<-'chirps-v2.0_2022.04.tif' 


Apr2<- raster(Apr2)
Apr3<- raster(Apr3)
Apr4<- raster(Apr4)
Apr5<- raster(Apr5)
Apr6<- raster(Apr6)
Apr7<- raster(Apr7)
Apr8<- raster(Apr8)
Apr9<- raster(Apr9)
Apr10<- raster(Apr10)
Apr11<- raster(Apr11)
Apr12<- raster(Apr12)
Apr13<- raster(Apr13)
Apr14<- raster(Apr14)
Apr15<- raster(Apr15)
Apr16<- raster(Apr16)
Apr17<- raster(Apr17)
Apr18<- raster(Apr18)
Apr19<- raster(Apr19)
Apr20<- raster(Apr20)
Apr21<- raster(Apr21)
Apr22<- raster(Apr22)
mean_Apr <- stack(Apr2, Apr3,Apr4,Apr5,Apr6,Apr7,Apr8,Apr9,Apr10, Apr11, Apr12,Apr13, Apr14, Apr15, Apr16, Apr17, Apr18, Apr19, Apr20, Apr21, Apr22)
mean_Apr <- calc(mean_Apr, mean)
mean_Apr<-data.frame(shape,extract(mean_Apr, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Apr$Apr<-mean_Apr$extract.mean_Apr..shape..fun...mean..na.rm...T..touches...TRUE.


#May
May2<-'chirps-v2.0_2002.05.tif' 
May3<-'chirps-v2.0_2003.05.tif' 
May4<-'chirps-v2.0_2004.05.tif' 
May5<-'chirps-v2.0_2005.05.tif' 
May6<-'chirps-v2.0_2006.05.tif' 
May7<-'chirps-v2.0_2007.05.tif' 
May8<-'chirps-v2.0_2008.05.tif' 
May9<-'chirps-v2.0_2009.05.tif' 
May10<-'chirps-v2.0_2010.05.tif' 
May11<-'chirps-v2.0_2011.05.tif' 
May12<-'chirps-v2.0_2012.05.tif' 
May13<-'chirps-v2.0_2013.05.tif' 
May14<-'chirps-v2.0_2014.05.tif' 
May15<-'chirps-v2.0_2015.05.tif' 
May16<-'chirps-v2.0_2016.05.tif' 
May17<-'chirps-v2.0_2017.05.tif' 
May18<-'chirps-v2.0_2018.05.tif' 
May19<-'chirps-v2.0_2019.05.tif' 
May20<-'chirps-v2.0_2020.05.tif' 
May21<-'chirps-v2.0_2021.05.tif' 
May22<-'chirps-v2.0_2022.05.tif' 


May2<- raster(May2)
May3<- raster(May3)
May4<- raster(May4)
May5<- raster(May5)
May6<- raster(May6)
May7<- raster(May7)
May8<- raster(May8)
May9<- raster(May9)
May10<- raster(May10)
May11<- raster(May11)
May12<- raster(May12)
May13<- raster(May13)
May14<- raster(May14)
May15<- raster(May15)
May16<- raster(May16)
May17<- raster(May17)
May18<- raster(May18)
May19<- raster(May19)
May20<- raster(May20)
May21<- raster(May21)
May22<- raster(May22)
mean_May <- stack(May2, May3,May4,May5,May6,May7,May8,May9,May10, May11, May12,May13, May14, May15, May16, May17, May18, May19, May20, May21, May22)
mean_May <- calc(mean_May, mean)
mean_May<-data.frame(shape,extract(mean_May, shape, fun=mean, na.rm=T, touches=TRUE))
mean_May$May<-mean_May$extract.mean_May..shape..fun...mean..na.rm...T..touches...TRUE.


#June
June2<-'chirps-v2.0_2002.06.tif' 
June3<-'chirps-v2.0_2003.06.tif' 
June4<-'chirps-v2.0_2004.06.tif' 
June5<-'chirps-v2.0_2005.06.tif' 
June6<-'chirps-v2.0_2006.06.tif' 
June7<-'chirps-v2.0_2007.06.tif' 
June8<-'chirps-v2.0_2008.06.tif' 
June9<-'chirps-v2.0_2009.06.tif' 
June10<-'chirps-v2.0_2010.06.tif' 
June11<-'chirps-v2.0_2011.06.tif' 
June12<-'chirps-v2.0_2012.06.tif' 
June13<-'chirps-v2.0_2013.06.tif' 
June14<-'chirps-v2.0_2014.06.tif' 
June15<-'chirps-v2.0_2015.06.tif' 
June16<-'chirps-v2.0_2016.06.tif' 
June17<-'chirps-v2.0_2017.06.tif' 
June18<-'chirps-v2.0_2018.06.tif' 
June19<-'chirps-v2.0_2019.06.tif' 
June20<-'chirps-v2.0_2020.06.tif' 
June21<-'chirps-v2.0_2021.06.tif' 
June22<-'chirps-v2.0_2022.06.tif' 



June2<- raster(June2)
June3<- raster(June3)
June4<- raster(June4)
June5<- raster(June5)
June6<- raster(June6)
June7<- raster(June7)
June8<- raster(June8)
June9<- raster(June9)
June10<- raster(June10)
June11<- raster(June11)
June12<- raster(June12)
June13<- raster(June13)
June14<- raster(June14)
June15<- raster(June15)
June16<- raster(June16)
June17<- raster(June17)
June18<- raster(June18)
June19<- raster(June19)
June20<- raster(June20)
June21<- raster(June21)
June22<- raster(June22)
mean_June <- stack(June2, June3,June4,June5,June6,June7,June8,June9,June10, June11, June12,June13, June14, June15, June16, June17, June18, June19, June20, June21, June22)
mean_June <- calc(mean_June, mean)
mean_June<-data.frame(shape,extract(mean_June, shape, fun=mean, na.rm=T, touches=TRUE))
mean_June$June<-mean_June$extract.mean_June..shape..fun...mean..na.rm...T..touches...TRUE.

#July
July2<-'chirps-v2.0_2002.07.tif' 
July3<-'chirps-v2.0_2003.07.tif' 
July4<-'chirps-v2.0_2004.07.tif' 
July5<-'chirps-v2.0_2005.07.tif' 
July6<-'chirps-v2.0_2006.07.tif' 
July7<-'chirps-v2.0_2007.07.tif' 
July8<-'chirps-v2.0_2008.07.tif' 
July9<-'chirps-v2.0_2009.07.tif' 
July10<-'chirps-v2.0_2010.07.tif' 
July11<-'chirps-v2.0_2011.07.tif' 
July12<-'chirps-v2.0_2012.07.tif' 
July13<-'chirps-v2.0_2013.07.tif' 
July14<-'chirps-v2.0_2014.07.tif' 
July15<-'chirps-v2.0_2015.07.tif' 
July16<-'chirps-v2.0_2016.07.tif' 
July17<-'chirps-v2.0_2017.07.tif' 
July18<-'chirps-v2.0_2018.07.tif' 
July19<-'chirps-v2.0_2019.07.tif' 
July20<-'chirps-v2.0_2020.07.tif' 
July21<-'chirps-v2.0_2021.07.tif' 
July22<-'chirps-v2.0_2022.07.tif' 

July2<- raster(July2)
July3<- raster(July3)
July4<- raster(July4)
July5<- raster(July5)
July6<- raster(July6)
July7<- raster(July7)
July8<- raster(July8)
July9<- raster(July9)
July10<- raster(July10)
July11<- raster(July11)
July12<- raster(July12)
July13<- raster(July13)
July14<- raster(July14)
July15<- raster(July15)
July16<- raster(July16)
July17<- raster(July17)
July18<- raster(July18)
July19<- raster(July19)
July20<- raster(July20)
July21<- raster(July21)

mean_July <- stack(July2, July3,July4,July5,July6,July7,July8,July9,July10, July11, July12,July13, July14, July15, July16, July17, July18, July19, July20, July21)
mean_July <- calc(mean_July, mean)
mean_July<-data.frame(shape,extract(mean_July, shape, fun=mean, na.rm=T, touches=TRUE))
mean_July$July<-mean_July$extract.mean_July..shape..fun...mean..na.rm...T..touches...TRUE.


#Aug
Aug2<-'chirps-v2.0_2002.08.tif' 
Aug3<-'chirps-v2.0_2003.08.tif' 
Aug4<-'chirps-v2.0_2004.08.tif' 
Aug5<-'chirps-v2.0_2005.08.tif' 
Aug6<-'chirps-v2.0_2006.08.tif' 
Aug7<-'chirps-v2.0_2007.08.tif' 
Aug8<-'chirps-v2.0_2008.08.tif' 
Aug9<-'chirps-v2.0_2009.08.tif' 
Aug10<-'chirps-v2.0_2010.08.tif' 
Aug11<-'chirps-v2.0_2011.08.tif' 
Aug12<-'chirps-v2.0_2012.08.tif' 
Aug13<-'chirps-v2.0_2013.08.tif' 
Aug14<-'chirps-v2.0_2014.08.tif' 
Aug15<-'chirps-v2.0_2015.08.tif' 
Aug16<-'chirps-v2.0_2016.08.tif' 
Aug17<-'chirps-v2.0_2017.08.tif' 
Aug18<-'chirps-v2.0_2018.08.tif' 
Aug19<-'chirps-v2.0_2019.08.tif' 
Aug20<-'chirps-v2.0_2020.08.tif' 
Aug21<-'chirps-v2.0_2021.08.tif' 
Aug22<-'chirps-v2.0_2022.08.tif' 

Aug2<- raster(Aug2)
Aug3<- raster(Aug3)
Aug4<- raster(Aug4)
Aug5<- raster(Aug5)
Aug6<- raster(Aug6)
Aug7<- raster(Aug7)
Aug8<- raster(Aug8)
Aug9<- raster(Aug9)
Aug10<- raster(Aug10)
Aug11<- raster(Aug11)
Aug12<- raster(Aug12)
Aug13<- raster(Aug13)
Aug14<- raster(Aug14)
Aug15<- raster(Aug15)
Aug16<- raster(Aug16)
Aug17<- raster(Aug17)
Aug18<- raster(Aug18)
Aug19<- raster(Aug19)
Aug20<- raster(Aug20)
Aug21<- raster(Aug21)
Aug22<- raster(Aug22)
mean_Aug <- stack(Aug2, Aug3,Aug4,Aug5,Aug6,Aug7,Aug8,Aug9,Aug10, Aug11, Aug12,Aug13, Aug14, Aug15, Aug16, Aug17, Aug18, Aug19, Aug20, Aug21)
mean_Aug <- calc(mean_Aug, mean)
mean_Aug<-data.frame(shape,extract(mean_Aug, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Aug$Aug<-mean_Aug$extract.mean_Aug..shape..fun...mean..na.rm...T..touches...TRUE.

#Sep
Sep2<-'chirps-v2.0_2002.09.tif' 
Sep3<-'chirps-v2.0_2003.09.tif' 
Sep4<-'chirps-v2.0_2004.09.tif' 
Sep5<-'chirps-v2.0_2005.09.tif' 
Sep6<-'chirps-v2.0_2006.09.tif' 
Sep7<-'chirps-v2.0_2007.09.tif' 
Sep8<-'chirps-v2.0_2008.09.tif' 
Sep9<-'chirps-v2.0_2009.09.tif' 
Sep10<-'chirps-v2.0_2010.09.tif' 
Sep11<-'chirps-v2.0_2011.09.tif' 
Sep12<-'chirps-v2.0_2012.09.tif' 
Sep13<-'chirps-v2.0_2013.09.tif' 
Sep14<-'chirps-v2.0_2014.09.tif' 
Sep15<-'chirps-v2.0_2015.09.tif' 
Sep16<-'chirps-v2.0_2016.09.tif' 
Sep17<-'chirps-v2.0_2017.09.tif' 
Sep18<-'chirps-v2.0_2018.09.tif' 
Sep19<-'chirps-v2.0_2019.09.tif' 
Sep20<-'chirps-v2.0_2020.09.tif' 
Sep21<-'chirps-v2.0_2021.09.tif' 
Sep22<-'chirps-v2.0_2022.09.tif' 

Sep2<- raster(Sep2)
Sep3<- raster(Sep3)
Sep4<- raster(Sep4)
Sep5<- raster(Sep5)
Sep6<- raster(Sep6)
Sep7<- raster(Sep7)
Sep8<- raster(Sep8)
Sep9<- raster(Sep9)
Sep10<- raster(Sep10)
Sep11<- raster(Sep11)
Sep12<- raster(Sep12)
Sep13<- raster(Sep13)
Sep14<- raster(Sep14)
Sep15<- raster(Sep15)
Sep16<- raster(Sep16)
Sep17<- raster(Sep17)
Sep18<- raster(Sep18)
Sep19<- raster(Sep19)
Sep20<- raster(Sep20)
Sep21<- raster(Sep21)
Sep22<- raster(Sep22)
mean_Sep <- stack(Sep2, Sep3,Sep4,Sep5,Sep6,Sep7,Sep8,Sep9,Sep10, Sep11, Sep12,Sep13, Sep14, Sep15, Sep16, Sep17, Sep18, Sep19, Sep20, Sep21)
mean_Sep <- calc(mean_Sep, mean)
mean_Sep<-data.frame(shape,extract(mean_Sep, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Sep$Sep<-mean_Sep$extract.mean_Sep..shape..fun...mean..na.rm...T..touches...TRUE.


#Oct
Oct2<-'chirps-v2.0_2002.10.tif' 
Oct3<-'chirps-v2.0_2003.10.tif' 
Oct4<-'chirps-v2.0_2004.10.tif' 
Oct5<-'chirps-v2.0_2005.10.tif' 
Oct6<-'chirps-v2.0_2006.10.tif' 
Oct7<-'chirps-v2.0_2007.10.tif' 
Oct8<-'chirps-v2.0_2008.10.tif' 
Oct9<-'chirps-v2.0_2009.10.tif' 
Oct10<-'chirps-v2.0_2010.10.tif' 
Oct11<-'chirps-v2.0_2011.10.tif' 
Oct12<-'chirps-v2.0_2012.10.tif' 
Oct13<-'chirps-v2.0_2013.10.tif' 
Oct14<-'chirps-v2.0_2014.10.tif' 
Oct15<-'chirps-v2.0_2015.10.tif' 
Oct16<-'chirps-v2.0_2016.10.tif' 
Oct17<-'chirps-v2.0_2017.10.tif' 
Oct18<-'chirps-v2.0_2018.10.tif' 
Oct19<-'chirps-v2.0_2019.10.tif' 
Oct20<-'chirps-v2.0_2020.10.tif' 
Oct21<-'chirps-v2.0_2021.10.tif' 
Oct22<-'chirps-v2.0_2022.10.tif' 

Oct2<- raster(Oct2)
Oct3<- raster(Oct3)
Oct4<- raster(Oct4)
Oct5<- raster(Oct5)
Oct6<- raster(Oct6)
Oct7<- raster(Oct7)
Oct8<- raster(Oct8)
Oct9<- raster(Oct9)
Oct10<- raster(Oct10)
Oct11<- raster(Oct11)
Oct12<- raster(Oct12)
Oct13<- raster(Oct13)
Oct14<- raster(Oct14)
Oct15<- raster(Oct15)
Oct16<- raster(Oct16)
Oct17<- raster(Oct17)
Oct18<- raster(Oct18)
Oct19<- raster(Oct19)
Oct20<- raster(Oct20)
Oct21<- raster(Oct21)
Oct22<- raster(Oct22)
mean_Oct <- stack(Oct2, Oct3,Oct4,Oct5,Oct6,Oct7,Oct8,Oct9,Oct10, Oct11, Oct12,Oct13, Oct14, Oct15, Oct16, Oct17, Oct18, Oct19, Oct20, Oct21)
mean_Oct <- calc(mean_Oct, mean)
mean_Oct<-data.frame(shape,extract(mean_Oct, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Oct$Oct<-mean_Oct$extract.mean_Oct..shape..fun...mean..na.rm...T..touches...TRUE.

#Nov
Nov2<-'chirps-v2.0_2002.11.tif' 
Nov3<-'chirps-v2.0_2003.11.tif' 
Nov4<-'chirps-v2.0_2004.11.tif' 
Nov5<-'chirps-v2.0_2005.11.tif' 
Nov6<-'chirps-v2.0_2006.11.tif' 
Nov7<-'chirps-v2.0_2007.11.tif' 
Nov8<-'chirps-v2.0_2008.11.tif' 
Nov9<-'chirps-v2.0_2009.11.tif' 
Nov10<-'chirps-v2.0_2010.11.tif' 
Nov11<-'chirps-v2.0_2011.11.tif' 
Nov12<-'chirps-v2.0_2012.11.tif' 
Nov13<-'chirps-v2.0_2013.11.tif' 
Nov14<-'chirps-v2.0_2014.11.tif' 
Nov15<-'chirps-v2.0_2015.11.tif' 
Nov16<-'chirps-v2.0_2016.11.tif' 
Nov17<-'chirps-v2.0_2017.11.tif' 
Nov18<-'chirps-v2.0_2018.11.tif' 
Nov19<-'chirps-v2.0_2019.11.tif' 
Nov20<-'chirps-v2.0_2020.11.tif' 
Nov21<-'chirps-v2.0_2021.11.tif' 
Nov22<-'chirps-v2.0_2022.11.tif' 

Nov2<- raster(Nov2)
Nov3<- raster(Nov3)
Nov4<- raster(Nov4)
Nov5<- raster(Nov5)
Nov6<- raster(Nov6)
Nov7<- raster(Nov7)
Nov8<- raster(Nov8)
Nov9<- raster(Nov9)
Nov10<- raster(Nov10)
Nov11<- raster(Nov11)
Nov12<- raster(Nov12)
Nov13<- raster(Nov13)
Nov14<- raster(Nov14)
Nov15<- raster(Nov15)
Nov16<- raster(Nov16)
Nov17<- raster(Nov17)
Nov18<- raster(Nov18)
Nov19<- raster(Nov19)
Nov20<- raster(Nov20)
Nov21<- raster(Nov21)
Nov22<- raster(Nov22)
mean_Nov <- stack(Nov2, Nov3,Nov4,Nov5,Nov6,Nov7,Nov8,Nov9,Nov10, Nov11, Nov12,Nov13, Nov14, Nov15, Nov16, Nov17, Nov18, Nov19, Nov20, Nov21)
mean_Nov <- calc(mean_Nov, mean)
mean_Nov<-data.frame(shape,extract(mean_Nov, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Nov$Nov<-mean_Nov$extract.mean_Nov..shape..fun...mean..na.rm...T..touches...TRUE.

#Dec
Dec2<-'chirps-v2.0_2002.12.tif' 
Dec3<-'chirps-v2.0_2003.12.tif' 
Dec4<-'chirps-v2.0_2004.12.tif' 
Dec5<-'chirps-v2.0_2005.12.tif' 
Dec6<-'chirps-v2.0_2006.12.tif' 
Dec7<-'chirps-v2.0_2007.12.tif' 
Dec8<-'chirps-v2.0_2008.12.tif' 
Dec9<-'chirps-v2.0_2009.12.tif' 
Dec10<-'chirps-v2.0_2010.12.tif' 
Dec11<-'chirps-v2.0_2011.12.tif' 
Dec12<-'chirps-v2.0_2012.12.tif' 
Dec13<-'chirps-v2.0_2013.12.tif'
Dec14<-'chirps-v2.0_2014.12.tif' 
Dec15<-'chirps-v2.0_2015.12.tif' 
Dec16<-'chirps-v2.0_2016.12.tif' 
Dec17<-'chirps-v2.0_2017.12.tif' 
Dec18<-'chirps-v2.0_2018.12.tif' 
Dec19<-'chirps-v2.0_2019.12.tif' 
Dec20<-'chirps-v2.0_2020.12.tif' 
Dec21<-'chirps-v2.0_2021.12.tif' 
Dec22<-'chirps-v2.0_2022.12.tif' 

Dec2<- raster(Dec2)
Dec3<- raster(Dec3)
Dec4<- raster(Dec4)
Dec5<- raster(Dec5)
Dec6<- raster(Dec6)
Dec7<- raster(Dec7)
Dec8<- raster(Dec8)
Dec9<- raster(Dec9)
Dec10<- raster(Dec10)
Dec11<- raster(Dec11)
Dec12<- raster(Dec12)
Dec13<- raster(Dec13)
Dec14<- raster(Dec14)
Dec15<- raster(Dec15)
Dec16<- raster(Dec16)
Dec17<- raster(Dec17)
Dec18<- raster(Dec18)
Dec19<- raster(Dec19)
Dec20<- raster(Dec20)
Dec21<- raster(Dec21)
Dec22<- raster(Dec22)
mean_Dec <- stack(Dec2, Dec3,Dec4,Dec5,Dec6,Dec7,Dec8,Dec9,Dec10, Dec11, Dec12,Dec13, Dec14, Dec15, Dec16, Dec17, Dec18, Dec19, Dec20, Dec21)
mean_Dec <- calc(mean_Dec, mean)
mean_Dec<-data.frame(shape,extract(mean_Dec, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Dec$Dec<-mean_Dec$extract.mean_Dec..shape..fun...mean..na.rm...T..touches...TRUE.

#Subset each month to not include islands, as these are not included in final GADM shapefile analysis and have large amounts of missing data
mean_jan <- subset(mean_jan, COUNTRY != "Cabo Verde")
mean_jan <- subset(mean_jan, COUNTRY != "Mauritius")
mean_jan <- subset(mean_jan, COUNTRY != "Seychelles")
mean_jan <- subset(mean_jan, COUNTRY != "São Tomé and Príncipe")
mean_jan <- subset(mean_jan, COUNTRY != "Comoros")

mean_Feb <- subset(mean_Feb, COUNTRY != "Cabo Verde")
mean_Feb <- subset(mean_Feb, COUNTRY != "Mauritius")
mean_Feb <- subset(mean_Feb, COUNTRY != "Seychelles")
mean_Feb <- subset(mean_Feb, COUNTRY != "São Tomé and Príncipe")
mean_Feb <- subset(mean_Feb, COUNTRY != "Comoros")

mean_Mar <- subset(mean_Mar, COUNTRY != "Cabo Verde")
mean_Mar <- subset(mean_Mar, COUNTRY != "Mauritius")
mean_Mar <- subset(mean_Mar, COUNTRY != "Seychelles")
mean_Mar <- subset(mean_Mar, COUNTRY != "São Tomé and Príncipe")
mean_Mar <- subset(mean_Mar, COUNTRY != "Comoros")

mean_Apr <- subset(mean_Apr, COUNTRY != "Cabo Verde")
mean_Apr <- subset(mean_Apr, COUNTRY != "Mauritius")
mean_Apr <- subset(mean_Apr, COUNTRY != "Seychelles")
mean_Apr <- subset(mean_Apr, COUNTRY != "São Tomé and Príncipe")
mean_Apr <- subset(mean_Apr, COUNTRY != "Comoros")

mean_May <- subset(mean_May, COUNTRY != "Cabo Verde")
mean_May <- subset(mean_May, COUNTRY != "Mauritius")
mean_May <- subset(mean_May, COUNTRY != "Seychelles")
mean_May <- subset(mean_May, COUNTRY != "São Tomé and Príncipe")
mean_May <- subset(mean_May, COUNTRY != "Comoros")

mean_June <- subset(mean_June, COUNTRY != "Cabo Verde")
mean_June <- subset(mean_June, COUNTRY != "Mauritius")
mean_June <- subset(mean_June, COUNTRY != "Seychelles")
mean_June <- subset(mean_June, COUNTRY != "São Tomé and Príncipe")
mean_June <- subset(mean_June, COUNTRY != "Comoros")

mean_July <- subset(mean_July, COUNTRY != "Cabo Verde")
mean_July <- subset(mean_July, COUNTRY != "Mauritius")
mean_July <- subset(mean_July, COUNTRY != "Seychelles")
mean_July <- subset(mean_July, COUNTRY != "São Tomé and Príncipe")
mean_July <- subset(mean_July, COUNTRY != "Comoros")

mean_Aug <- subset(mean_Aug, COUNTRY != "Cabo Verde")
mean_Aug <- subset(mean_Aug, COUNTRY != "Mauritius")
mean_Aug <- subset(mean_Aug, COUNTRY != "Seychelles")
mean_Aug <- subset(mean_Aug, COUNTRY != "São Tomé and Príncipe")
mean_Aug <- subset(mean_Aug, COUNTRY != "Comoros")

mean_Sep <- subset(mean_Sep, COUNTRY != "Cabo Verde")
mean_Sep <- subset(mean_Sep, COUNTRY != "Mauritius")
mean_Sep <- subset(mean_Sep, COUNTRY != "Seychelles")
mean_Sep <- subset(mean_Sep, COUNTRY != "São Tomé and Príncipe")
mean_Sep <- subset(mean_Sep, COUNTRY != "Comoros")

mean_Oct <- subset(mean_Oct, COUNTRY != "Cabo Verde")
mean_Oct <- subset(mean_Oct, COUNTRY != "Mauritius")
mean_Oct <- subset(mean_Oct, COUNTRY != "Seychelles")
mean_Oct <- subset(mean_Oct, COUNTRY != "São Tomé and Príncipe")
mean_Oct <- subset(mean_Oct, COUNTRY != "Comoros")

mean_Nov <- subset(mean_Nov, COUNTRY != "Cabo Verde")
mean_Nov <- subset(mean_Nov, COUNTRY != "Mauritius")
mean_Nov <- subset(mean_Nov, COUNTRY != "Seychelles")
mean_Nov <- subset(mean_Nov, COUNTRY != "São Tomé and Príncipe")
mean_Nov <- subset(mean_Nov, COUNTRY != "Comoros")

mean_Dec <- subset(mean_Dec, COUNTRY != "Cabo Verde")
mean_Dec <- subset(mean_Dec, COUNTRY != "Mauritius")
mean_Dec <- subset(mean_Dec, COUNTRY != "Seychelles")
mean_Dec <- subset(mean_Dec, COUNTRY != "São Tomé and Príncipe")
mean_Dec <- subset(mean_Dec, COUNTRY != "Comoros")


#Subset dataframes
mean_jan<-mean_jan[ , c('GID_2', 'NAME_2','extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Feb<-mean_Feb[ , c('GID_2', 'NAME_2','extract.mean_Feb..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Mar<-mean_Mar[ , c('GID_2', 'NAME_2','extract.mean_Mar..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Apr<-mean_Apr[ , c('GID_2', 'NAME_2','extract.mean_Apr..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_May<-mean_May[ , c('GID_2', 'NAME_2','extract.mean_May..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_June<-mean_June[ , c('GID_2', 'NAME_2','extract.mean_June..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_July<-mean_July[ , c('GID_2', 'NAME_2','extract.mean_July..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Aug<-mean_Aug[ , c('GID_2', 'NAME_2','extract.mean_Aug..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Sep<-mean_Sep[ , c('GID_2', 'NAME_2','extract.mean_Sep..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Oct<-mean_Oct[ , c('GID_2', 'NAME_2','extract.mean_Oct..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Nov<-mean_Nov[ , c('GID_2', 'NAME_2','extract.mean_Nov..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Dec<-mean_Dec[ , c('GID_2', 'NAME_2','extract.mean_Dec..shape..fun...mean..na.rm...T..touches...TRUE.')]
cl2test<-cl2test[ , c('GID_2', 'NAME_2_x', 'zonalcat')]


#Bind monthly average and rainfall clusters into one data frame
test<-cbind(mean_jan,mean_Feb,mean_Mar, mean_Apr, mean_May, mean_June,mean_July,mean_Aug,mean_Sep,mean_Oct,mean_Nov,mean_Dec,cl2test)
#Change row names
test$Jan<-test$extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE.
test$Feb<-test$extract.mean_Feb..shape..fun...mean..na.rm...T..touches...TRUE.
test$Mar<-test$extract.mean_Mar..shape..fun...mean..na.rm...T..touches...TRUE.
test$Apr<-test$extract.mean_Apr..shape..fun...mean..na.rm...T..touches...TRUE.
test$May<-test$extract.mean_May..shape..fun...mean..na.rm...T..touches...TRUE.
test$June<-test$extract.mean_June..shape..fun...mean..na.rm...T..touches...TRUE.
test$July<-test$extract.mean_July..shape..fun...mean..na.rm...T..touches...TRUE.
test$Aug<-test$extract.mean_Aug..shape..fun...mean..na.rm...T..touches...TRUE.
test$Sep<-test$extract.mean_Sep..shape..fun...mean..na.rm...T..touches...TRUE.
test$Oct<-test$extract.mean_Oct..shape..fun...mean..na.rm...T..touches...TRUE.
test$Nov<-test$extract.mean_Nov..shape..fun...mean..na.rm...T..touches...TRUE.
test$Dec<-test$extract.mean_Dec..shape..fun...mean..na.rm...T..touches...TRUE.
test$Zone<-test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
test$ADMN2_code<-test$GID_2
test$ADMN2_name<-test$NAME_2
# Load necessary libraries
library(dplyr)
library(tidyr)
library(wesanderson)
library(RColorBrewer)

# Select specific columns from the 'test' dataset
datasets <- test[, c('zonalcat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')]

# Group data by 'zonalcat' and calculate the mean for each group
df <- datasets %>%
  group_by(zonalcat) %>%
  summarise_all("mean")

# Remove the 7th row from the dataframe
df <- df[-7, ]

# Convert the dataset from wide to long format
data_long <- gather(df, month, Rainfall, Jan:Dec, factor_key = TRUE)
data_long

# Use a color palette from the 'wesanderson' package
test_palette <- wes_palette("FantasticFox1", n = 6, type = "continuous")

# Plot the data using ggplot2
ggplot(data_long, aes(x = month, y = Rainfall, group = zonalcat)) +
  geom_line(aes(color = zonalcat)) +
  geom_point(aes(color = zonalcat)) +
  scale_color_manual(values = test_palette) +
  labs(title = "Rainfall classes", x = "Month", y = "Monthly Atmospheric Precipitation (mm)") +
  theme(
    plot.title = element_text(size = 14),  # Adjust size of the plot title
    axis.title = element_text(size = 14),  # Adjust size of axis titles
    axis.text = element_text(size = 10)    # Adjust size of axis text
  )

# Plot 'zonalcat' column in the 'cl2test' dataset
plot(cl2test['zonalcat'])

# Use a ColorBrewer palette
palette <- brewer.pal(6, "Paired")

# Plot the data using ggplot2 with a different palette and increased element sizes
compare <- data_long %>%
  ggplot(aes(x = month, y = Rainfall, group = zonalcat, color = zonalcat)) +
  scale_color_manual(values = palette) +
  geom_line(size = 1.5) +  # Increased line thickness
  geom_point(size = 4) +   # Increased point size
  labs(y = "Monthly Atmospheric Precipitation (mm)", x = "Month", colour = "Class", title = "Rainfall classes") +
  theme(
    text = element_text(size = 16),  # Increased base text size
    axis.title = element_text(size = 18),  # Increased axis title size
    axis.text = element_text(size = 14),  # Increased axis text size
    plot.title = element_text(size = 20),  # Increased plot title size
    legend.title = element_text(size = 16),  # Increased legend title size
    legend.text = element_text(size = 14)  # Increased legend text size
  )

# Plot the graph
print(compare)

# Convert 'zonalcat' based on 'Class' values using ifelse and convert to numeric
cl2test$zonal_cat_numeric <- ifelse(cl2test$zonalcat == "Class 1", as.numeric("1"),
                                    ifelse(cl2test$zonalcat == "Class 2", as.numeric("2"),
                                           ifelse(cl2test$zonalcat == "Class 3", as.numeric("3"),
                                                  ifelse(cl2test$zonalcat == "Class 4", as.numeric("4"),
                                                         ifelse(cl2test$zonalcat == "Class 5", as.numeric("5"),
                                                                ifelse(cl2test$zonalcat == "Class 6", as.numeric("6"), NA))))))

# Print the modified dataframe
print(cl2test)

# Extract the column you want to convert to raster
column_to_raster <- cl2test$zonal_cat_numeric

# Create a raster template from the shapefile
raster_template <- raster(extent(cl2test), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(cl2test, raster_template, field = column_to_raster)

# Plot the rasterized column using a color palette
magma_like_palette <- brewer.pal(6, "Paired")

plot(rasterized_column, col = magma_like_palette)
