library("readxl")
data1_1 <- read_excel("data1_1.xls", sheet = "Sheet1")

str(data1_1)

data1_1 <- data.frame(read_excel("data1_1.xls", sheet = "Sheet1", col_types = c("text","numeric","numeric","numeric")))

colnames(data1_1) <- c('název','výkon','spotřeba','délka')
rownames(data1_1) <- data1_1$název
n <- nrow(data1_1)

cor(data1_1[c(2:4)], use = "pairwise.complete.obs")

# Hledat další chyby v celé datové tabulce by bylo trochu zdlouhavé, byť některé jsou patrné na první pohled
print(data1_1)

#problematické hodnoty pravděpodobně budou mezi k=5 největšími a nejmenšími
k <- 5
head(arrange(data1_1, výkon), k)
tail(arrange(data1_1, výkon), k)

head(arrange(data1_1, spotřeba), k)
tail(arrange(data1_1, spotřeba), k)

head(arrange(data1_1, délka), k)
tail(arrange(data1_1, délka), k)

# Toto jsou již dříve vyřazené nenumerické hodnoty (pro připomenutí uložíme celý soubor jako text):
data1_1text <- data.frame(read_excel("data1_1.xls", sheet = "Sheet1", col_types = c("text","text","text","text")))
colnames(data1_1text) <- c('název','výkon','spotřeba','délka')
rownames(data1_1text) <- data1_1text$název
data1_1text["Citroen C3 Picasso","délka"]
data1_1text["VW Jetta","délka"]

# A toto je datum uložené jako hodnota, kterou také odstraníme
data1_1["Nissan Pixo","spotřeba"] 
data1_1["Nissan Pixo","spotřeba"] <- NA


# Následující čtyři hodnoty jsou opět jasné chyby, které odstraníme:
data1_1["Renault Mégane Grandtour","výkon"]
data1_1["Nissan Tiida","výkon"]
data1_1["Citroen C1","délka"]
data1_1["KIA Soul","délka"]

data1_1["Renault Mégane Grandtour","výkon"] <- NA
data1_1["Nissan Tiida","výkon"] <- NA
data1_1["Citroen C1","délka"] <- NA
data1_1["KIA Soul","délka"] <- NA

# Pokud se podíváme na graf s hodnotami dvojic proměnných, je zřejmé, že spotřeba Volva S80 nebude reálná"
pairs(data1_1[c(2:4)])
data1_1["Volvo S80",]
data1_1["Volvo S80","spotřeba"] <- NA

# Po odstranění všech osmi chybných hodnot už vycházejí korelační koeficienty tak, jak bychom očekávali
cor(data1_1[c(2:4)],use = "pairwise.complete.obs")
