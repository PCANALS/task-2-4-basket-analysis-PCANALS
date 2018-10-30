rm(list = ls())

#### OBJECTIVES ####
## You will be discovering any interesting relationships (or associations) between customers #
      #transactions and the item(s) they have purchased. These associations can then be used to drive
      #sales-oriented initiatives such as recommender systems like the ones used by Amazon and
      #other eCommerce sites. 
## Are there any interesting patterns or item relationships within Electronidex's transactions?
## Would Blackwell benefit from selling any of Electronidex's items?
## In your opinion, should Blackwell acquire Electronidex?
## If Blackwell does acquire Electronidex, do you have any recommendations for Blackwell?
      #(Ex: cross-selling items, sale promotions, should they remove items, etc.)#

#### LOADING LIBRARIES ####
library(arules)
library(arulesViz)

#### SET WORKING DIRECTORY ####

#david directory#

#setwd("C:/Users/David/Google Drive/Github/task-2-4-basket-analysis-PCANALS")

#pilar directory#

setwd("C:/Users/pilar/Documents/Ubiqum/TASK2.4/task-2-4-basket-analysis-PCANALS")



#### LOADING DATASET ####

data = read.transactions("transactions.csv",
                  format = "basket",
                  sep = ",",
                  rm.duplicates = T,
                  encoding = "unknown")

#### LOOKING DATASET ####

dim(data) #Number of Rows and Columns
size(data) #Number of items per transaction
#arules::LIST(data) #Diferent of "inspect". With LIST a list is actually generated
itemLabels(data)

itemFrequencyPlot(x = data, topN = 15) #Top ventas

#### APRIORI ####

rules = apriori(data = data, parameter = list(support = 0.01, confidence = 0.5))
inspect(head(sort(rules, by="lift"),5))

plot(rules, method = "graph")
summary(rules)

image(sample(dat, 2000)) #probado de 100 a 2000 y no se ve un patron#

itemFrequencyPlot(dat, topN=10) #hay que indicarle un top o no puede plotear#

#### Associations with APRIORI####

apriori(data)

RulesName<- apriori(data, parameter = list(supp = 0.01, conf = 0.5)) #probado con muchas variables y las rules siempre son 0#  

RulesName2<- apriori(data, parameter = list(supp = 0.01, conf = 0.0, minlen=2), appearance = list(lhs = "iMac"))

RulesName3<- apriori(data, parameter = list(supp = 0.01, conf = 0.1), appearance = list(rhs="Apple Earpods"))

plot(RulesName3)

inspect(sort(RulesName2, by = "confidence"))

inspect(RulesName)


####EXPLORING RANDOM PATTERNS####

patterns = random.patterns(nItems = 1000);

summary(patterns)# vemos los items mas frecuentes#

trans = random.transactions(nItems = 1000, nTrans = 1000, method = "agrawal",  patterns = patterns);

image(trans) #plot transacctions vs items#

data("data");

bas <- as(data, "transactions");

rules <- apriori(bas, parameter=list(support=0.01, confidence=0.5));

rules
sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE);

subrules = rules[quality(rules)$confidence > 0.8];

subrules
plot(subrules, method="matrix", measure="lift");

plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE));

plot(subrules, method="matrix3D", measure="lift");

plot(subrules, method="matrix3D", measure="lift", control = list(reorder=TRUE));

plot(subrules, method="matrix", measure=c("lift", "confidence"));

plot(subrules, method="matrix", measure=c("lift","confidence"), control = list(reorder=TRUE));

plot(rules, method="grouped");

plot(rules, method="grouped", control=list(k=50));

sel = plot(RulesName2, method="grouped", interactive=TRUE);

sum(is.redundant(rules))


#### ITEM RULES ¿¿¿¿BORRAR???#### 

# ItemRules <- subset(rules, items %in% c("iMac", "HP Laptop", "Lenovo Desktop Computer"))
# inspect(ItemRules)
#### ONLY 1 ITEM ####
# rules with rhs containing "Survived" only
rules.imac <- apriori(data,
                 parameter = list(supp=0.01, conf=0.2),
                 appearance = list(lhs= "iMac"),
                 control = list(verbose=F))
rules.sorted <- sort(rules.imac, by="lift")
inspect(rules.sorted)




#### CAMBIAR NOMBRES DE ITEMS -  LABEL BRAND####

data@itemInfo$brand = data@itemInfo$labels

data@itemInfo$brand[grep(pattern = "^i[A-Z]", x = data@itemInfo$brand)] <- "Apple"
data@itemInfo$brand[grep(pattern = "^Apple", x = data@itemInfo$brand)] <- "Apple"
data@itemInfo$brand[grep(pattern = "^LG", x = data@itemInfo$brand)] <- "LG"
data@itemInfo$brand[grep(pattern = "^Acer", x = data@itemInfo$brand)] <- "Acer"
data@itemInfo$brand[grep(pattern = "^HP", x = data@itemInfo$brand)] <- "HP"
data@itemInfo$brand[grep(pattern = "^ASUS", x = data@itemInfo$brand)] <- "Asus"
data@itemInfo$brand[grep(pattern = "^Dell", x = data@itemInfo$brand)] <- "Dell"
data@itemInfo$brand[grep(pattern = "^Lenovo", x = data@itemInfo$brand)] <- "Lenovo"
data@itemInfo$brand[grep(pattern = "^CYBERPOWER", x = data@itemInfo$brand)] <- "Cyberpower"
data@itemInfo$brand[grep(pattern = "^Samsung", x = data@itemInfo$brand)] <- "Samsung"
data@itemInfo$brand[grep(pattern = "^Logit", x = data@itemInfo$brand)] <- "Logitech"
data@itemInfo$brand[grep(pattern = "^Microsoft", x = data@itemInfo$brand)] <- "Microsoft"
data@itemInfo$brand[grep(pattern = "^Rii", x = data@itemInfo$brand)] <- "Rii"
data@itemInfo$brand[grep(pattern = "^Alienware", x = data@itemInfo$brand)] <- "Alienware"

"%ni%" = Negate("%in%")

data@itemInfo$brand[data@itemInfo$brand %ni% c("Apple", "LG", "Acer", "HP", "Asus", "Dell", "Lenovo", "Cyberpower",
                                               "Samsung", "Logitech", "Microsoft", "Rii", "Alienware")] <- "Others"

data@itemInfo$brand <- as.factor(data@itemInfo$brand)

class(data)

itemLabels(data)

brand.cat <- arules::aggregate(x = data, by = "brand")

rules<-apriori(data = brand.cat,
                #appearance = list(lhs= "Others"),
                parameter = list(support = 0.01, confidence = 0.5))
inspect(head(sort(rules, by="confidence"),15))

###ok###
plot(rules, method = "grouped", control = list(k=10))
plot(rules, method = "matrix", measure = c("lift","confidence"))


###ok###
plot(sort(x = rules, by="confidence")[1:10],method="graph")

#### LABEL CATEGORIES####


data@itemInfo$cate = data@itemInfo$labels


#Laptops# añadiendo las diferentes palabras clave

data@itemInfo$cate[grep(pattern = "Laptop|MacBook|Aspire|Chromebook", x = data@itemInfo$cate)] <- "Laptops" 


#Desktop# # utilizando un vector por la palabra Desktop en diferentes categorias#


desks<-c("Lenovo Desktop Computer","iMac","HP Desktop","ASUS Desktop","Dell Desktop",
         "Intel Desktop","Acer Desktop","CYBERPOWER Gamer Desktop","Dell 2 Desktop")

  
data@itemInfo$cate[grep(paste(pattern = desks, collapse="|"), x = data@itemInfo$cate)] <- "Desktops"



#Monitors# Todos los monitors finalizan en $

data@itemInfo$cate[grep(pattern = "Monitor$", x = data@itemInfo$cate)] <- "Monitors"


#Computer Mice# vector por la diversidad 


mouse<- c("3-Button Mouse","Logitech Wireless Mouse","Microsoft Basic Optical Mouse",
          "Logitech 3-button Mouse","Redragon Gaming Mouse","HP Wireless Mouse","Generic Black 3-Button",
          "Wireless Portable Mouse","Gaming Mouse Professional","Slim Wireless Mouse")

data@itemInfo$cate[grep(paste(pattern = mouse, collapse="|"), x = data@itemInfo$cate)] <- "Computer Mice"


#Keyboard# acabado en Keyboard$

data@itemInfo$cate[grep(pattern = "Keyboard$", x = data@itemInfo$cate)] <- "Keyboard"



#Mouse and Keyboard Combo#
data@itemInfo$cate[grep(pattern = "Combo", x = data@itemInfo$cate)] <- "Mouse and Keyboard Combo"
data@itemInfo$cate[grep(pattern = "Keyboard and Mouse", x = data@itemInfo$cate)] <- "Mouse and Keyboard Combo"
data@itemInfo$cate[grep(pattern = "Keyboard & Mouse", x = data@itemInfo$cate)] <- "Mouse and Keyboard Combo"



#Computer Headphones# #mezcla de generico y especifico por no tener key words#

data@itemInfo$cate[grep(pattern = "Headset", x = data@itemInfo$cate)] <- "Computer Headphones"
data@itemInfo$cate[grep(pattern = "On-Ear", x = data@itemInfo$cate)] <- "Computer Headphones"
data@itemInfo$cate[grep(pattern = "Ailihen", x = data@itemInfo$cate)] <-"Computer Headphones"
data@itemInfo$cate[grep(pattern = "Koss", x = data@itemInfo$cate)] <- "Computer Headphones"
data@itemInfo$cate[grep(pattern = "Kensington Headphones", x = data@itemInfo$cate)] <- "Computer Headphones"



#Active Headphones# #especifico por no existir key words#

data@itemInfo$cate[grep(pattern = "Earpods", x = data@itemInfo$cate)] <- "Active Headphones"
data@itemInfo$cate[grep(pattern = "Bluetooth Headphone", x = data@itemInfo$cate)] <- "Active Headphones"
data@itemInfo$cate[grep(pattern = "Beats", x = data@itemInfo$cate)] <- "Active Headphones"
data@itemInfo$cate[grep(pattern = "In-Ear", x = data@itemInfo$cate)] <- "Active Headphones"
data@itemInfo$cate[grep(pattern = "Earhook", x = data@itemInfo$cate)] <- "Active Headphones"


#Computer Cords# #key word#

data@itemInfo$cate[grep(pattern = "Cable", x = data@itemInfo$cate)] <- "Computer Cords"
data@itemInfo$cate[grep(pattern = "HDMI", x = data@itemInfo$cate)] <- "Computer Cords"


#Accessories#

data@itemInfo$cate[grep(pattern = "Mouse Pad", x = data@itemInfo$cate)] <- "Accesories"
data@itemInfo$cate[grep(pattern = "Computer Game", x = data@itemInfo$cate)] <- "Accesories"
data@itemInfo$cate[grep(pattern = "Home and Student", x = data@itemInfo$cate)] <- "Accesories"



#Speakers# Mezcla key word y especifico#

data@itemInfo$cate[grep(pattern = "Speaker", x = data@itemInfo$cate)] <- "Speakers"
data@itemInfo$cate[grep(pattern = "Sonos", x = data@itemInfo$cate)] <- "Speakers"
data@itemInfo$cate[grep(pattern = "Acoustics", x = data@itemInfo$cate)] <- "Speakers"
data@itemInfo$cate[grep(pattern = "DOSS Touch Wireless Bluetooth", x = data@itemInfo$cate)] <- "Speakers"


#Printers # con vector para evitar confusion con Printer Ink#

printers<- c("Epson Printer","HP Wireless Printer","Canon Office Printer",
             "Brother Printer","DYMO Label Manker")

data@itemInfo$cate[grep(paste(pattern = printers, collapse="|"), x = data@itemInfo$cate)] <- "Printers"


#Printer Ink# key words genericas#

data@itemInfo$cate[grep(pattern = "Ink", x = data@itemInfo$cate)] <- "Printer Ink"
data@itemInfo$cate[grep(pattern = "Toner", x = data@itemInfo$cate)] <- "Printer Ink"
data@itemInfo$cate[grep(pattern = "Tape", x = data@itemInfo$cate)] <- "Printer Ink"


#Computer Stands# key words genericas#

data@itemInfo$cate[grep(pattern = "Stand", x = data@itemInfo$cate)] <- "Computer Stands"
data@itemInfo$cate[grep(pattern = "Mount", x = data@itemInfo$cate)] <- "Computer Stands"

#Computer Tablets# especifico por key words dentro de otras palabras

data@itemInfo$cate[grep(pattern = "iPad", x = data@itemInfo$cate)] <- "Computer Tablets"
data@itemInfo$cate[grep(pattern = "Kindle", x = data@itemInfo$cate)] <- "Computer Tablets"
data@itemInfo$cate[grep(pattern = "Fire HD Tablet", x = data@itemInfo$cate)] <- "Computer Tablets"
data@itemInfo$cate[grep(pattern = "Samsung Galaxy Tab", x = data@itemInfo$cate)] <- "Computer Tablets"


#External Hardrives# mix

data@itemInfo$cate[grep(pattern = "External", x = data@itemInfo$cate)] <- "External Hardrives"
data@itemInfo$cate[grep(pattern = "5TB Desktop Hard Drive", x = data@itemInfo$cate)] <- "External Hardrives"


#Smart Home Devices# mix

data@itemInfo$cate[grep(pattern = "TV", x = data@itemInfo$cate)] <- "Smart Home Devices"
data@itemInfo$cate[grep(pattern = "Google Home", x = data@itemInfo$cate)] <- "Smart Home Devices"
data@itemInfo$cate[grep(pattern = "Smart Light Bulb", x = data@itemInfo$cate)] <- "Smart Home Devices"
data@itemInfo$cate[grep(pattern = "Roku Express", x = data@itemInfo$cate)] <- "Smart Home Devices"


data@itemInfo


data@itemInfo$cate <- as.factor(data@itemInfo$cate)
str(data@itemInfo)

#agregate#

cate.cat <- arules::aggregate(x = data, by = "cate")

rules.cat<-apriori(data = cate.cat,parameter = list(support = 0.01, confidence = 0.5))


inspect(head(sort(rules.cat, by="confidence"),15))

plot(rules.cat, method = "grouped", control = list(k=10)) # muy util para ver los main products Desktop 
#y Laptops  y complementos basicos que son tanto directos a Laptop como a Desktop


plot(rules.cat, method = "matrix", measure = c("lift","confidence")) #no util#

plot(sort(x = rules.cat, by="confidence")[1:5],method="graph") #por mejorar plot para que sea util#



#### LABEL KIND - POR TRABAJAR####
#Dividido en 2 categorias MAIN-productos principales Desktops y labels por lo visto en el anterior paso COMPLE- productos complementarios (posible utilizacion para segmentacion de clientes)


#pendiente de añadir alienware# o agrupar por tipo de label

#main<-c("LG Touchscreen Laptop","Acer Aspire","HP Laptop","ASUS Chromebook","Apple Macbook Pro","Apple MacBook Air","Dell Laptop",
#        "Eluktronics Pro Gaming Laptop","Alienware AW17R4-7345SLV-PUS 17" Laptop",
#"HP Notebook Touchscreen Laptop PC"
"Lenovo Desktop Computer"
"iMac"
"HP Desktop"
"ASUS Desktop"
"Dell Desktop"
"Intel Desktop"
"Acer Desktop"
"CYBERPOWER Gamer Desktop"
"Dell 2 Desktop"
"iPad"
"iPad Pro"
"Fire HD Tablet"
"Samsung Galaxy Tab"
"Kindle"


####~~~notas~~~####

#analizar por categorias los que tienen menor volumen de ventas en Blackwell,vs Electrosu
#rows vacias
##support , amount of times 2 items are together depends of the datasize

#confidence  
#lift how is related

#check with ranges support and confidence