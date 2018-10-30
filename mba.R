rm(list = ls())

#### OBJECTIVES ####
## You will be discovering any interesting relationships (or associations) between customer’s #
      #transactions and the item(s) they’ve purchased. These associations can then be used to drive
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

#### LOADING DATASET ####

#david directory#
"C:/Users/David/Google Drive/Github/task-2-4-basket-analysis-PCANALS/transactions.csv"

setwd("C:/Users/pilar/Documents/Ubiqum/TASK2.4/task-2-4-basket-analysis-PCANALS")


df = read.csv("transactions.csv",
              header = F,
              stringsAsFactors = F)



dat = read.transactions("transactions.csv",
                  format = "basket",
                  sep = ",",
                  rm.duplicates = T,
                  encoding = "UTF-8")

dim(dat) #Number of Rows and Columns
size(dat) #Number of items per transaction
arules::LIST(dat)
summary(dat)
#list(data)

#inspect(data)


image(dat) # demasiados datos en transacciones por lo que solo se ve una linea, hay que coger un sample#
sample(dat)

image(sample(dat, 2000)) #probado de 100 a 2000 y no se ve un patron#

itemFrequencyPlot(dat, topN=10) #hay que indicarle un top o no puede plotear#

####associations with APRIORI####
apriori(dat)

RulesName<- apriori(dat, parameter = list(supp = 0.01, conf = 0.5)) #probado con muchas variables y las rules siempre son 0#  

RulesName2<- apriori(dat, parameter = list(supp = 0.01, conf = 0.0, minlen=2), appearance = list(lhs = "iMac"))

RulesName3<- apriori(dat, parameter = list(supp = 0.01, conf = 0.1), appearance = list(rhs="Apple Earpods"))

plot(RulesName3)


#

inspect(sort(RulesName2, by = "confidence"))
#support , amount of times 2 items are together depends of the datasize

#confidence  
#lift how is related

#check with ranges support and confidence


#minlen???###

inspect(RulesName)

####RANDOM PATTERNS####
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

####~~~notas~~~####

#analizar por categorias los que tienen menor volumen de ventas en Blackwell,vs Electrosu
#rows vacias
#
