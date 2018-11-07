# Lade alle Pakete die benötigt werden für die Berechnungen
library(rstudioapi) # Paket zum setzen des "Working-Directories"
library(psych) # Paket für Cronbachs Alpha
library(ggplot2) # Paket für Grafiken
library(reshape2) # Paket für Datentransformation

#sessionInfo() # get the version of the packages attached

# Setze das "Working Directory" auf den Ordner wo das .R-File und die Rohdaten liegen
# FAlls der folgende Befehl nicht funktioniert kann man das "Working Directory" auch 
# händisch setzen mittels CTRL + STRG + H
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Einlesen der Rohdaten (Datei muss im "Working Directory" liegen - siehe oben)
d.raw <- read.csv2("export_690239_2018_04_07.csv", header = TRUE, sep = ";", comment.char = "")


################################## Clear the data ##################################

# Übernehme nur die Daten, welche für die spätere Analyse notwendig sind

#### Produktpalette 1:
# Produktpalette 1 zuerst dem Probanden gezeigt (PP11) 
# Produktpalette 1 als 2. dem Probanden gezeigt (PP12)
PP <- c("PP11", "PP12")
# alle möglichen Produktabkürzungen für Produktpalette 1 (siehe Word-Dokument)
products <- c("Tg", "Pg", "Jb", "TuZ", "Rs", "Hh")
# Für jedes Produkt wurden 4 Fragen gestellt
questions <- 1:4
# Erzeuge alle möglichen Kombinationen der Produktpräsentation. (erzeuge alle Spaltenüberschriften
# für die Produktpalette 1). Die Kombinationen haben die Form: [PP11/PP12]_[Tg/Pg/Jb/TuZ/Rs/Hh]_[1/2/3/4]
PP1_columns <- apply(expand.grid(PP, products, questions), 1, function(x) paste(x[1], x[2], x[3], sep="_"))

#### Produktpalette 2
# Produktpalette 2 zuerst dem Probanden gezeigt (PP21) 
# Produktpalette 2 als 2. dem Probanden gezeigt (PP22)
PP <- c("PP21", "PP22")
# alle möglichen Produktabkürzungen für Produktpalette 2 (siehe Word-Dokument)
products <- c("Tw", "Pst", "Js", "TuK", "Rg", "Hb")
# Für jedes Produkt wurden 4 Fragen gestellt
questions <- 1:4
# Erzeuge alle möglichen Kombinationen der Produktpräsentation. (erzeuge alle Spaltenüberschriften
# für die Produktpalette 2). Die Kombinationen haben die Form: [PP21/PP22]_[Tw/Pst/Js/TuK/Rg/Hb]_[1/2/3/4]
PP2_columns <- apply(expand.grid(PP, products, questions), 1, function(x) paste(x[1], x[2], x[3], sep="_"))

#### Impulskauftendenz Rook
rook_columns <- paste0("rook_", 1:9)
#### Impulskauftendenz Verplanken
verp_columns <- paste0("verp_", 1:20)

#### Attraktivität des Befragungsortes 
# Raumatmosphäre & Lageratmosphäre
Ort <- c("VAtm", "LAtm")
# Für jeden Ort wurden 3 Fragen gestellt
questions <- 1:3
# Erzeuge alle möglichen Kombinationen der für die Atmosphäre - sowohl für Verkaufsraum
# als auch für Lager. Die möglichen Kombinationen sind: [VAtm,LAtm]_[1/2/3]
Ort_columns <- apply(expand.grid(Ort, questions), 1, function(x) paste(x[1], x[2], sep="_"))

# übernehme aus den Rohdaten nur die Spalten für die wir oben definiert haben 
# Spaltenüberschriften generiert haben. Weiters wissen wir dass alle Zeilen mit
# dispcode 31 beendete Fragebögen sind.
d.cleared <- d.raw[d.raw$dispcode == 31, c(PP1_columns, PP2_columns, "Ort_PP1", Ort_columns, rook_columns, verp_columns, "ort_1.bewertung")]

# Fehlende Werte werden mit -77 gekennzeichnet. Ersetze -77 mit NA (not available)
d.cleared[d.cleared == -77] <- NA

# Problem: Wurde Produktpalette 1 als erstes gezeigt so haben wir Werte für die Variablen
# PP11_.... Die entsprechenden Werte für die Variablen PP12 sind daher NA gesetzt. Für die
# spätere Analyse fassen wir die Werte zusammen: d.h. wir überschreiben alle NA - Werte in 
# PP11_... mit den entsprechenden Werten aus PP12_...
tmp <- d.cleared[, grepl("PP11", names(d.cleared))]
tmp2 <- d.cleared[, grepl("PP12", names(d.cleared))]
tmp[is.na(tmp)] <- tmp2[is.na(tmp)]
d.tmp <- tmp
# Analaoge Zusammenführung der Daten wie oben
tmp <- d.cleared[, grepl("PP21", names(d.cleared))]
tmp2 <- d.cleared[, grepl("PP22", names(d.cleared))]
tmp[is.na(tmp)] <- tmp2[is.na(tmp)]

# Zusammenführen der bereinigten Daten und Anreicherung mit Ort_PP1 sowie Atmosphären-Fragen.
# Aus dem Ort_PP1 kann abgeleitet werden ob eine Bewertung im Verkaufsraum oder im Lager stattgefunden hat.
d.cleared <- cbind(d.tmp, tmp , d.cleared[, c("Ort_PP1", Ort_columns, rook_columns, verp_columns, "ort_1.bewertung")])

# Die ersten beiden Zeilen im Datenset enthalten nur Testdaten und müssen für die spätere Analyse ausgeschlossen werden
d.cleared <- d.cleared[-(1:2),]

# Der letzte Buchstabe aus Ort_PP1 gibt an ob Produktpalette 1 im Verkaufsraum (=v) oder im Lager (=h) ausgestellt war.
location_string <- substr(as.character(d.cleared$Ort_PP1), nchar(as.character(d.cleared$Ort_PP1)), nchar(as.character(d.cleared$Ort_PP1))) 
d.cleared$PP1_location <- ifelse(location_string == "v", "Store", "Warehouse")

# Wurde PP1 im Verkaufsraum ausgestellt so ist klar dass Produktpalette 2 im Lager ausgestellt wurde und vice versa. 
d.cleared$PP2_location <- ifelse(d.cleared$PP1_location == "Store", "Warehouse", "Store")


#################################### Datenkorrektur ############################################################
# Bei Lageratmosphäre (LAtm1) ist es in Unipark zu dem Problem gekommen, dass eine 8 in den Daten eigentlich eine 6 sein sollte.
# Bei Verkaufsraumatmosphäre (VAtm1) ist es in Unipark zu dem Problem gekommen, dass eine 9 in den Daten eigentlich eine 6 sein sollte.
# siehe Codebuch (Seite 38 bzw. 36) für weitere Informationen.

d.cleared[d.cleared$LAtm_1 == 8, "LAtm_1"] <- 6
d.cleared[d.cleared$VAtm_1 == 9, "VAtm_1"] <- 6

# Umkodieren von Item 8 rook
d.cleared$rook_8 <- 6 - d.cleared$rook_8

# Umkodieren von Items 1, 3, 7, 8, 9, 11, 13, 15
d.cleared[,paste0("verp_",c(1, 3, 7, 8, 9, 11, 13, 15))] <- sapply(c(1, 3, 7, 8, 9, 11, 13, 15),
                                                                   function(x){d.cleared[,paste0("verp_",x)] <- 8 - d.cleared[,paste0("verp_",x)]})

########################################## Cronbachs alpha Produkte ################################################
# Beginnen von hier befinden sind in d.cleared alle relevanten Daten für die Analyse
# Beachte, dass es ab hier nur noch PP11 und PP21 gibt da die Werte von PP12 zu PP11 und jene von PP22 zu PP21 hinzugefügt wurden.

### Hier wird das Cronbach Alpha für jedes Produkt ausgerechnet.
### Möchte man das Cronbach Alpha für ein bestimmtes Produkt berechnen, so muss
### im Befehl unten die entsprechende Produktabkürzung eingefügt werden.
### Mögliche Werte für die verschiedenen Produkte sind: Tg, Pg, Jb, TuZ, Rs, Hh, Tw, Pst, Js, TuK, Rg, Hb
product_columns <- grepl("TuK", names(d.cleared))
psych::alpha(d.cleared[,product_columns])


########################################## Cronbachs alpha Ort ################################################
### Hier wird das Cronbach Alpha für jeden Bewertungsort ausgerechnet.
### Möchte man das Cronbach Alpha für einen bestimmtes Ort (Lager oder Verkausraum) berechnen, so muss
### im Befehl unten die entsprechende Ortabkürzung (LAtm bzw. VAtm) eingeführt werden. 
### LAtm = Lageratmosphäre
### VAtm = Verkaufsraumatmosphäre
Ort_columns <- grepl("VAtm", names(d.cleared))
psych::alpha(d.cleared[,Ort_columns])

########################################## Cronbachs Rook-Skala ################################################
rook_columns <- grepl("rook", names(d.cleared))
psych::alpha(d.cleared[, rook_columns])

########################################## Cronbachs Verplanken-Skala ################################################
verp_columns <- grepl("verp", names(d.cleared))
psych::alpha(d.cleared[, verp_columns])

########################################## Mittelwerte pro Produkt ################################################
# alle Saplten für die Fragen 1-3 in eine lange Liste untereinander schreiben 
df.product_1_sumed <- melt(cbind(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))], PP1_location = d.cleared$PP1_location),
                id.vars = "PP1_location",
                variable.name = "Produkt_Frage",
                value.name = "Rating")
df.product_1_sumed$Produkt_Frage <- sub("_[1-4]", "", df.product_1_sumed$Produkt_Frage) # lösche für jede Variable die Kennung um welche Frage es sich handelt.
# Mittelwert pro Produkt bilden mit Unterscheidung zwischen Store und Warehouse
df.product_1_total_means <- aggregate(Rating ~ PP1_location + Produkt_Frage, data = df.product_1_sumed, mean)
# total means of products
df.total_means_PP1 <- aggregate(Rating ~ Produkt_Frage, data = df.product_1_sumed, mean)
# total sd of products
df.total_sd_PP1 <- aggregate(Rating ~ Produkt_Frage, data = df.product_1_sumed, sd)

names(df.product_1_total_means)[names(df.product_1_total_means) == 'PP1_location'] <- 'Location'

df.product_2_sumed <- melt(cbind(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))], PP2_location = d.cleared$PP2_location),
                           id.vars = "PP2_location",
                           variable.name = "Produkt_Frage",
                           value.name = "Rating")
df.product_2_sumed$Produkt_Frage <- sub("_[1-4]", "", df.product_2_sumed$Produkt_Frage) # lösche für jede Variable die Kennung um welche Frage es sich handelt.
df.product_2_total_means <- aggregate(Rating ~ PP2_location + Produkt_Frage, data = df.product_2_sumed, mean)
# total means of products
df.total_means_PP2 <- aggregate(Rating ~ Produkt_Frage, data = df.product_2_sumed, mean)
# total sd of products
df.total_sd_PP2 <- aggregate(Rating ~ Produkt_Frage, data = df.product_2_sumed, sd)
names(df.product_2_total_means)[names(df.product_2_total_means) == 'PP2_location'] <- 'Location'

# Mittwerte von PP1 und PP2 in einer Tabelle zusammenfassen (untereinander schreiben)
df.all_means <- rbind(df.product_1_total_means, df.product_2_total_means)

# Definiere neue x-Achsen-Beschriftung für die Grafik
labels_x_axis <- c("Trousers_light", "Jacket_blue", "Jumper_grey", "Skirt_black", "T-Shirt_grey", "Scarf_zigzag",
                   "Trousers_blue", "Jacket_black", "Jumper_striped", "Skirt_grey", "Scarf_checked", "T-Shirt_white")

ggplot(df.all_means, aes(Produkt_Frage, Rating)) +
  geom_point(aes(colour = Location), size = 4) + # Datenpunkte je nach location färben und Größe ändern
  labs(color = "Location") + # Legende beschriften
  scale_x_discrete(labels = labels_x_axis) + # lösche PP11_ aus Variablennamen
  scale_color_manual(labels = c("Salesroom", "Warehouse"), values = c("#F8766D", "#00BA38")) +
  labs(x = "Products") +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Beschriftung x-Achse um 90° drehen


########################################## Dichte der Atmosphäre ################################################
# Für jeden Probanden berechnen wir den Mittelwert von VAtm (Verkaufsraumatmosphäre) sowie von LAtm (Lageratmosphäre)
d.means_VAtm <- data.frame(Store = rowMeans(d.cleared[, grepl("VAtm_[1-3]", names(d.cleared))]))
d.means_LAtm <- data.frame(Warehouse = rowMeans(d.cleared[, grepl("LAtm_[1-3]", names(d.cleared))]))


# Mittelwerte von VAtm und LAtm in einer langen Liste zusammenfassen (untereinander schreiben)
d.atmosphere <- melt(cbind(d.means_VAtm, d.means_LAtm),
                     variable.name = "Location",
                     value.name = "Roomattractiveness")
# Mittwerte von VAtm und LAtm ermitteln (wichtig für die Grafik)
d.means <- aggregate(d.atmosphere$Roomattractiveness, list(d.atmosphere$Location), mean)
# SD von VAtm und LAtm ermitteln (wichtig für die Grafik)
d.sd <- aggregate(d.atmosphere$Roomattractiveness, list(d.atmosphere$Location), sd)

# Dichte für LAtm und VAtm sowie deren Mittelwert zeichnen
ggplot(d.atmosphere, aes(x = Roomattractiveness, color = Location)) +
  geom_density() +
  geom_vline(data = d.means, aes(xintercept = x, color = Group.1), linetype="dashed")

# Dichte für LAtm und VAtm sowie deren Mittelwert zeichnen
ggplot(d.atmosphere, aes(x = Roomattractiveness)) +
  geom_density(aes(group=Location, fill = Location), alpha=0.3) +
  geom_vline(data = d.means, aes(xintercept = x), linetype="dashed", size=1) +
  scale_fill_grey() + theme_classic(base_size = 17)#theme(legend.text=element_text(size=20),
  #                                              legend.title=element_text(size=20),
  #                                              axis.title=element_text(size=20),
  #                                              axis.text = element_text(size=20))
  
ggplot(d.atmosphere, aes(x = Roomattractiveness)) +
  geom_density(aes(group=Location, colour=Location, fill=Location), alpha=0.3) +
  geom_vline(data = d.means, aes(xintercept = x, color = Group.1), linetype="dashed", size=1) +
  geom_histogram(aes(y=..density.., fill = Location), alpha=0.5, position="identity", binwidth=.7)

########################################## t-test Raumattraktivität ################################################
t.test(d.means_VAtm$Store, d.means_LAtm$Warehouse, paired=TRUE)
# t(74) = 8.249 ---> p < .001
mean(d.means_VAtm$Store)
mean(d.means_LAtm$Warehouse)

########################################## t-test Produktbewertung ################################################
# Alle Produktbewertungen von PP1 und PP2 in einer langen Liste
# (Achtung: Im Abschnitt ###Mittelwerte pro Produkt### wird festgelegt of Fragen 1-3 oder Fragen 1-4 pro Produkt verwendet werden)
# Derzeit sind nur Fragen 1-3 pro Produkt inkludiert.
names(df.product_1_sumed)[names(df.product_1_sumed) == 'PP1_location'] <- 'Location'
names(df.product_2_sumed)[names(df.product_2_sumed) == 'PP2_location'] <- 'Location'
df.all_product_questions <- rbind(df.product_1_sumed, df.product_2_sumed)

# Test ob es einen Berwertungsunterschied zwischen Lager und Verkaufsraum gibt.
t.test(df.all_product_questions[df.all_product_questions$Location == "Warehouse", "Rating"],
       df.all_product_questions[df.all_product_questions$Location == "Store", "Rating"], paired = TRUE)


########################################## Korrelation Impulskauftendenzskalen mit Produktbewertung ################################################
# bereits z-transformiert
df.means_rook <- data.frame(rook = rowMeans(scale(d.cleared[, grepl("rook", names(d.cleared))])))
df.means_verp <- data.frame(verp = rowMeans(scale(d.cleared[, grepl("verp", names(d.cleared))])))
cor(cbind(df.means_rook, df.means_verp), method = "pearson")

df.means_impuls_skala <- data.frame(impulse_skala = rowMeans(cbind(df.means_rook, df.means_verp)))

# für jede Person nehmen wir den Mittelwert über alle Prdouktbewertungen die im Verkaufsraum abgegeben worden sind und subtrahieren davon
# alle Prdouktbewertungen die im Lager abgegeben worden sind.
d.diff_store_warehouse <- ifelse(d.cleared$PP1_location == "Store",
       rowMeans(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))]) - rowMeans(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))]),
       rowMeans(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))]) - rowMeans(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))]))

cor(cbind(df.means_impuls_skala, d.diff_store_warehouse), method = "pearson")
cor.test(df.means_impuls_skala$impulse_skala, d.diff_store_warehouse, method = "pearson")


########################################## Regression ################################################
df.means_products <- data.frame(total_product_rating = rowMeans(d.cleared[, grepl("[PP11|PP21]_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))]))
df.fit_location <- cbind(df.means_products, data.frame(total_location_rating = rowMeans(cbind(d.means_VAtm, d.means_LAtm))))

fit.location <- lm(total_product_rating ~ total_location_rating, data = df.fit_location)
summary(fit.location)
cor.test(df.fit_location$total_product_rating, df.fit_location$total_location_rating, method = "pearson")

########################################## t-Test ################################################
# unterscheiden sich die Bewertungen von PP1 und PP2 

PP1_total_mean <- rowMeans(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])
PP2_total_mean <- rowMeans(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])

names(PP1_total_mean) <- 1:length(PP1_total_mean)
names(PP2_total_mean) <- 1:length(PP2_total_mean)

t.test(PP1_total_mean, PP2_total_mean, paired = TRUE)
# Mittelwert und SD Abweichung von PP1 und PP2
mean(PP1_total_mean)
sd(PP1_total_mean)
mean(PP2_total_mean)
sd(PP2_total_mean)
