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

rules = apriori(data = brand.cat,
                #appearance = list(lhs= "Others"),
                parameter = list(support = 0.01, confidence = 0.5))
inspect(head(sort(rules, by="confidence"),15))

plot(rules, method = "grouped", control = list(k=10))
plot(rules, method = "matrix", measure = c("lift","confidence"))

#### LABEL CATEGORIES####


data@itemInfo$cate = data@itemInfo$labels


#Laptops#

data@itemInfo$cate[grep(pattern = "Laptop", x = data@itemInfo$cate)] <- "Laptops" 

##error logical al intentar añadir mas de una palabra por patternn ("Laptop"|"MacBook")###

data@itemInfo$cate[grep(pattern = "MacBook", x = data@itemInfo$cate)] <- "Laptops"
data@itemInfo$cate[grep(pattern = "Aspire", x = data@itemInfo$cate)] <- "Laptops"
data@itemInfo$cate[grep(pattern = "Aspire", x = data@itemInfo$cate)] <- "Laptops"
data@itemInfo$cate[grep(pattern = "Chromebook", x = data@itemInfo$cate)] <- "Laptops"

#Desktop#

data@itemInfo$cate[grep(pattern = "iMac", x = data@itemInfo$cate)] <- "Desktops"

#OJO NO FUNCIONA
data@itemInfo$cate[grep(pattern = "Desktop", x = data@itemInfo$cate)] <- "Desktops"


data@itemInfo$cate
#no se ha podido incluir por la palabra Desktops por estar tambien en otras categorias#

#Monitors#

#no se ha podido incluir por la palabra Monitors por estar tambien en otras categorias#


#Computer Mice
#Keyboard
#Mouse and Keyboard Combo


#Computer Headphones#

data@itemInfo$cate[grep(pattern = "Headset", x = data@itemInfo$cate)] <- "Computer Headphones"

#no se ha podido incluir por la palabra Headphones por estar tambien en otras categorias#



#Active Headphones# 
#interferencia con otras categorias#

#Computer Cords#
data@itemInfo$cate[grep(pattern = "Cable", x = data@itemInfo$cate)] <- "Computer Cords"

#Speakers#
data@itemInfo$cate[grep(pattern = "Speaker", x = data@itemInfo$cate)] <- "Speakers"
data@itemInfo$cate[grep(pattern = "Sonos", x = data@itemInfo$cate)] <- "Speakers"

#Computer Stands#

data@itemInfo$cate[grep(pattern = "Stand", x = data@itemInfo$cate)] <- "Computer Stands"
data@itemInfo$cate[grep(pattern = "Mount", x = data@itemInfo$cate)] <- "Computer Stands"

#Computer Tablets#


#External Hardrives#

data@itemInfo$cate[grep(pattern = "External", x = data@itemInfo$cate)] <- "External Hardrives"

#Smart Home Devices#

data@itemInfo$cate[grep(pattern = "TV", x = data@itemInfo$cate)] <- "Smart Home Devices"


data@itemInfo


data@itemInfo$cate <- as.factor(data@itemInfo$cate)
str(data@itemInfo)

####~~~notas~~~####

#analizar por categorias los que tienen menor volumen de ventas en Blackwell,vs Electrosu
#rows vacias
##support , amount of times 2 items are together depends of the datasize

#confidence  
#lift how is related

#check with ranges support and confidence