#设置工作目录
setwd("D:/RScript")

library("maptools")
library(ggplot2)
library(plyr)
china_map = readShapePoly("bou2_4p.shp")       # 读取地图空间数据
#plot(china_map)

#ggplot(china_map,aes(x=long,y=lat,group=group)) +
#  geom_polygon(fill="white",colour="grey") +
#  coord_map("polyconic")

x <- china_map@data          #读取行政信息
xs <- data.frame(x,id=seq(0:924)-1)          #含岛屿共925个形状
china_map1 <- fortify(china_map)           #转化为数据框
china_map_data <- join(china_map1, xs, type = "full")       #合并两个数据框

##读取指标数据，csv格式
mydata <- read.csv("geshengzhibiao.csv")          
china_data <- join(china_map_data, mydata, type="full")          #合并两个数据框

#ggplot(china_data, aes(x = long, y = lat, group = group, fill = X2014年)) +
#  geom_polygon(colour="grey40") +
#  scale_fill_gradient(low="white",high="steelblue") +  #指定渐变填充色，可使用RGB
#  coord_map("polyconic")        #指定投影方式为polyconic，获得常见视角中国地图

##清除背景色、坐标轴、经纬线,图例放左下角.
province_city <- read.csv("chinaprovincecity.csv")  #读取省会城市坐标
pp <- ggplot(china_data, aes(x = long, y = lat, group = group,fill = X2014年)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="steelblue") +  #指定渐变填充色，可使用RGB
  coord_map("polyconic") +       #指定投影方式为polyconic，获得常见视角中国地图
  geom_text(aes(x = jd,y = wd,label = province), data =province_city)+
  theme(               #清除不需要的元素
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.2,0.3)
  )
pp <- pp + ggtitle("2014年全国各省GDP（无香港和台湾数据）单位：亿元")
pp

##添加省名标签
province_city <- read.csv("chinaprovincecity.csv")  #读取省会城市坐标
china_data <- join(china_data, province_city, type="full")          #合并两个数据框

pp <- ggplot(china_data,aes(long,lat))+
  labs(title="2014年全国各省GDP（无香港和台湾数据）单位：亿元")+
  geom_polygon(aes(group=group,fill=X2014年),colour="grey60")+
  scale_fill_gradient(name="2014年GDP",low="white",high="steelblue") +
  coord_map("polyconic") +
  #coord_map(projection = "azequidistant")+
  geom_text(aes(x = jd,y = wd,label = province), data =province_city)+
  theme(
    #panel.grid = element_blank(),
    #panel.background = element_blank(),
    #axis.text = element_blank(),
    #axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.2,0.3)
  )
pp 

pp <- ggplot(china_data,aes(long,lat,group=group))+geom_path()
pp <- pp+labs(title="2014年全国各省GDP（无香港和台湾数据）单位：亿元")
pp <- pp+coord_map("polyconic")
pp <- pp+geom_point(data=china_data,aes(x=jd,y=wd,size=X2014年),color="steelblue")  #aes() mapping the data to the graph
pp <- pp+scale_size_continuous(name="2014年GDP")
#pp <- pp+scale_colour_continuous(low="white",high="black")
pp <- pp+  theme(
  axis.title = element_blank(),
  legend.position = c(0.2,0.3)
)
pp
 
