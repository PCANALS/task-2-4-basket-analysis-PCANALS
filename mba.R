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
df = read.csv("C:/Users/David/Google Drive/Github/task-2-4-basket-analysis-PCANALS/transactions.csv",
              header = F,
              stringsAsFactors = F)

data = read.transactions("C:/Users/David/Google Drive/Github/task-2-4-basket-analysis-PCANALS/transactions.csv",
                  format = "basket",
                  sep = ",",
                  rm.duplicates = T,
                  encoding = "UTF-8")

dim(data) #Number of Rows and Columns
size(data) #Number of items per transaction
arules::LIST(data)
inspect(data)


list(((1,2,3,4), ("Hi", "car")))

