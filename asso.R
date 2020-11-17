library(arules)

datasets<-read.csv("Groceries_dataset.csv")
newMat<-as.data.frame(matrix(F,ncol=length(unique(datasets$itemDescription)),nrow = length(unique(datasets$Member_number))))
colnames(newMat)<-unique(datasets$itemDescription)
rownames(newMat)<-unique(datasets$Member_number)
for (i in 1:nrow(datasets)) {
  variable<-datasets[i,]
  newMat[as.character(variable$Member_number),as.character(variable$itemDescription)]<-T
}

items2.df = data.frame(item1=c(T,F,F,T,T,F,T,T,T), item2=c(T,T,T,T,F,T,F,T,T), item3=c(F,F,T,F,T,T,T,T,T),item4=c(F,T,F,T,F,F,F,F,F), item5=c(T,F,F,F,F,F,F,T,F))
items.df = data.frame(newMat)
str(items.df)
rules <- apriori(items.df,parameter = list(minlen=2, supp=0.005, conf=0.8), control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted,rules.sorted)
subset.matrix
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
subset.matrix
redundant <- apply(subset.matrix, 2, any)
redundant
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


itemsets <- eclat(items.df, parameter = list(supp = 0.1, maxlen = 15))
itemsets.sorted <- sort(itemsets) 
inspect(itemsets.sorted[1:5]) 
head(as(items(itemsets.sorted), "list"))


itemsets <- weclat(items.df, parameter = list(supp = 0.1, maxlen = 15))
itemsets.sorted <- sort(itemsets) 
inspect(itemsets.sorted[1:5]) 
head(as(items(itemsets.sorted), "list"))

is.subset(rules.sorted,rules.sorted)
liner<-as.data.frame(matrix(nrow = nrow(items.df),ncol = 2))
colnames(liner)<-c("client","panier")
for (i in 1:nrow(items.df)) {
  line<-rownames(items.df[i,])
  liner[i,]$client<-line
  liner[i,]$panier<-paste((colnames(items.df[line,!items.df[line, ]==FALSE])), collapse = ',')
}


