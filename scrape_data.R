library(rvest)
URL = "https://www.zillow.com/bountiful-ut/sold/?searchQueryState=%7B%22pagination%22%3A%7B%7D%2C%22usersSearchTerm%22%3A%22Bountiful%2C%20UT%22%2C%22mapBounds%22%3A%7B%22west%22%3A-111.98104941381835%2C%22east%22%3A-111.68338858618164%2C%22south%22%3A40.78168501436135%2C%22north%22%3A40.97429909769153%7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A23785%2C%22regionType%22%3A6%7D%5D%2C%22isMapVisible%22%3Atrue%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%2C%22ah%22%3A%7B%22value%22%3Atrue%7D%2C%22rs%22%3A%7B%22value%22%3Atrue%7D%2C%22fsba%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22cmsn%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22fore%22%3A%7B%22value%22%3Afalse%7D%2C%22pmf%22%3A%7B%22value%22%3Afalse%7D%2C%22pf%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A12%7D"
url2 = "https://www.zillow.com/bountiful-ut/sold/2_p/?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A2%7D%2C%22usersSearchTerm%22%3A%22Bountiful%2C%20UT%22%2C%22mapBounds%22%3A%7B%22west%22%3A-111.97383963598632%2C%22east%22%3A-111.69059836401367%2C%22south%22%3A40.78324478798973%2C%22north%22%3A40.972743819477344%7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A23785%2C%22regionType%22%3A6%7D%5D%2C%22isMapVisible%22%3Atrue%2C%22filterState%22%3A%7B%22fsba%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22fore%22%3A%7B%22value%22%3Afalse%7D%2C%22cmsn%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22pmf%22%3A%7B%22value%22%3Afalse%7D%2C%22pf%22%3A%7B%22value%22%3Afalse%7D%2C%22rs%22%3A%7B%22value%22%3Atrue%7D%2C%22ah%22%3A%7B%22value%22%3Atrue%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A12%7D"
s = read_html(url2)
hm = html_nodes(s, ".hiFEPb")
maxpgs = as.integer(strsplit(strsplit(as.character(hm[1]), "</span>")[[1]],
                              "of <!-- -->")[[1]][2])
masterurls = c(URL, url2, rep(NA, maxpgs-2))
for (i in 3:maxpgs) {
  u = gsub("2_p/\\?searchQueryState=", paste0(i,"_p/?searchQueryState="),
           url2)
  u = gsub("22pagination%22%3A%7B%22currentPage%22%3A2", 
           paste0("22pagination%22%3A%7B%22currentPage%22%3A", i),
           u)
  masterurls[i] = u
}

myfunc = function(myurl){
  Sys.sleep(runif(1,3,5))
  s = read_html(myurl)
  s2 = html_nodes(s,"a")
  these = grep("www.zillow.com/homedetails", s2)
  keep = which(these%%2 == 0)
  return(sapply(these[keep], 
                function(i) strsplit(as.character(s2[i]), "\"")[[1]][2]))
}

houseurls = unlist(lapply(masterurls, myfunc))
houseurls = unique(houseurls)

scrape = function(myurl){
  p = read_html(myurl)
  type = html_text(html_nodes(p,".zsg-icon-buildings~.fKxGLN"))
  built = as.integer(html_text(html_nodes(p,".zsg-icon-calendar~.fKxGLN")))
  lotsize = html_text(html_nodes(p, ".zsg-icon-lot~.fKxGLN"))
  p2 = html_text(html_nodes(p, ".ds-bed-bath-living-area span:nth-child(1)"))
  bed = as.integer(p2[1])
  bath = as.integer(p2[2])
  area = p2[3]
  cost = html_text(html_nodes(p, ".zestimate-value"))
  return(list(Type = type, Built = built, Lot = lotsize, Bed = bed, 
              Bath = bath, Area = area, Cost = cost))
}

dta = list()
for(i in 1:length(houseurls)){
  Sys.sleep(runif(1,3,5))
  dta[[i]] = scrape(houseurls[i])
  if(i%%10 == 5){print(i)}
}

b = rep(NA, length(houseurls))
t = rep(NA, length(houseurls))
l = rep(NA, length(houseurls))
bd = rep(NA, length(houseurls))
ba = rep(NA, length(houseurls))
a = rep(NA, length(houseurls))
c = rep(NA, length(houseurls))
for(i in 1:length(houseurls)){
  if(length(dta[[i]]$Built)==1){
    b[i] = dta[[i]]$Built
  }
  if(length(dta[[i]]$Type)==1){
    t[i] = dta[[i]]$Type
  }
  if(length(dta[[i]]$Lot)==1){
    l[i] = dta[[i]]$Lot
  }
  if(length(dta[[i]]$Bed)==1){
    bd[i] = dta[[i]]$Bed
  }
  if(length(dta[[i]]$Bath)==1){
    ba[i] = dta[[i]]$Bath
  }
  if(length(dta[[i]]$Area)==1){
    a[i] = dta[[i]]$Area
  }
  if(length(dta[[i]]$Cost)==1){
    c[i] = dta[[i]]$Cost
  }
}

df = data.frame(Type = t, Built = b, Lot = l, Bed = bd, Bath = ba,
                Area = a, Cost = c, URLaddress = houseurls)
write.csv(df, file = "Bountiful_UT_3-25-2021.csv", row.names = F)

##############################################################
df = read.csv("Bountiful_UT_3-2-2021.csv")
head(df)
table(df$Type, useNA = 'always')

myfunc1 = function(l){
  if(grepl("Acres", l)){
    return(as.numeric(strsplit(l, " ")[[1]][1]))
  }
  if(grepl("sqft", l)){
    sqft = as.numeric(gsub(",", "",strsplit(l, " ")[[1]][1]))
    return(sqft/43560)
  }
  if(is.na(l)){return(NA)}
}
df$Lot = sapply(df$Lot, myfunc1)
df$Area = as.numeric(gsub(",", "", df$Area))
df$Cost = as.numeric(gsub("\\$", "", gsub(",", "", df$Cost)))

head(df[,1:7])
houses = df[df$Type == "SingleFamily",2:7]
houses = houses[complete.cases(houses),]
houses = houses[houses$Cost < 1500000,]
houses = houses[houses$Area > 1000,]

plot(houses$Built, houses$Cost)
plot(houses$Lot, houses$Cost)
plot(houses$Bed, houses$Cost)
plot(houses$Bath, houses$Cost)
plot(houses$Area, jitter(houses$Cost, amount = 10000))

mdl = lm(Cost ~ Built + Lot + Bath + Area + Bed, data = houses)
summary(mdl)

hist(mdl$residuals)

sum(coef(mdl)*c(1, 2000, 0.5, 5, 3500, 3))


