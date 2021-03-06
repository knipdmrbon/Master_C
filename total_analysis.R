#############################################################################################
#############################################################################################
#
######################
# Variable Description
######################
# d.raw -------------------> raw data; read directly from Unipark-file and not modified
# PP1_columns -------------> column names of all product ratings from product range 1
# PP2_columns -------------> column names of all product ratings from product range 2
# rook_columns ------------> column names of all questions related to Rook & Fisher scale (questions 1 to 9)
# verp_columns ------------> column names of all questions related to Verplanken & Herabadi scale (questions 1 to 20)
# feelings_columns --------> column names of all feelings; that means questions 1 to 4 related to 
#                              warehouse and store respectively
# attr_columns -------------> column names of room-attractiveness; questions 1 to 3 related to 
#                              warehouse and store respectively
# d.cleared ---------------> raw data: rows cleaned by surveys not finished or interrupted due to technical issues
#                             columns: 
#                               loc_PP1, ---> location of product group 1 (either store or warehouse)
#                               attr_columns, feelings_columns, rook_columns, verp_columns,
#                               loc_first rating, ---> start location of product rating (either store or warehouse)
#                               vpn ---> unique number for each participant
#                               age --> age of the participant
# d.cleared$VAtm_mean ------> mean score of attractiveness of the sales room (3 questions per participant)
# d.cleared$LAtm_mean ------> mean score of attractiveness of the warehouse (3 questions per participant)
# d.cleared$rook_mean ------> mean score of Rook & Fisher scale (questions 1 to 9)
# d.cleared$verp_mean ------> mean score of Verplanken & Herabadi scale (questions 1 to 20)
# d.cleared$impuls_mean-----> due to high correlation between the scales of Rook & Fisher and Verplanken & Herabadi
#                             mean score-> creating a new variable of impulse buying tendency
# d.cleared$PP1_means ------> mean score of all product ratings of product range 1
# d.cleared$pp2_means ------> mean score of all product ratings of product range 2
# d.cleared$PP1_location  --> location of product rating of product range 1 (either store or warehouse)
# d.cleared$pp2_location ---> location of product rating of product range 2 (either store or warehouse)
# d.GLM_input --------------> per participant two rows, divided by product ratings made in the sales room and the warehouse
# fit.all_comp -------------> GLM Model including variables: Y_mean of product rating,
#                             Location (sales room or warehouse), room attractiveness, feelings (1 to 4)
#                             impuls_mean, location_first rating
# fit.loc_impuls -----------> Model with interaction of location and impuls_mean
# fit.attr_impuls ----------> Model with interaction of room attractiveness and impuls_mean
#
# fit.store ----------------> Linear regression model between product_rating and room-attractiveness of sales room
# fit.warehouse ------------> Linear regression model between product_rating and room-attractiveness of warehouse
# fit.moderation -----------> Linear regression model between product_rating and room_attractiveness
#                             and mean of feelings with interaction
#############################################################################################
#############################################################################################


# Lade alle Pakete die ben�tigt werden f�r die Berechnungen
library(rstudioapi) # Paket zum setzen des "Working-Directories"
library(psych) # Paket f�r Cronbachs Alpha
library(ggplot2) # Paket f�r Grafiken
library(reshape2) # Paket f�r Datentransformation
library(cowplot) # make ggplots in a grid

# Setze das "Working Directory" auf den Ordner wo das .R-File und die Rohdaten liegen
# FAlls der folgende Befehl nicht funktioniert kann man das "Working Directory" auch 
# h�ndisch setzen mittels CTRL + STRG + H
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Einlesen der Rohdaten (Datei muss im "Working Directory" liegen - siehe oben)
d.raw <- read.csv2("export_690239_2018_04_07.csv", header = TRUE, sep = ";", comment.char = "")


################################## Clear the data ##################################

# �bernehme nur die Daten, welche f�r die sp�tere Analyse notwendig sind

#### Produktpalette 1:
# Produktpalette 1 zuerst dem Probanden gezeigt (PP11) 
# Produktpalette 1 als 2. dem Probanden gezeigt (PP12)
PP <- c("PP11", "PP12")
# alle m�glichen Produktabk�rzungen f�r Produktpalette 1 (siehe Word-Dokument)
products <- c("Tg", "Pg", "Jb", "TuZ", "Rs", "Hh")
# F�r jedes Produkt wurden 4 Fragen gestellt
questions <- 1:4
# Erzeuge alle m�glichen Kombinationen der Produktpr�sentation. (erzeuge alle Spalten�berschriften
# f�r die Produktpalette 1). Die Kombinationen haben die Form: [PP11/PP12]_[Tg/Pg/Jb/TuZ/Rs/Hh]_[1/2/3/4]
PP1_columns <- apply(expand.grid(PP, products, questions), 1, function(x) paste(x[1], x[2], x[3], sep="_"))

#### Produktpalette 2
# Produktpalette 2 zuerst dem Probanden gezeigt (PP21) 
# Produktpalette 2 als 2. dem Probanden gezeigt (PP22)
PP <- c("PP21", "PP22")
# alle m�glichen Produktabk�rzungen f�r Produktpalette 2 (siehe Word-Dokument)
products <- c("Tw", "Pst", "Js", "TuK", "Rg", "Hb")
# F�r jedes Produkt wurden 4 Fragen gestellt
questions <- 1:4
# Erzeuge alle m�glichen Kombinationen der Produktpr�sentation. (erzeuge alle Spalten�berschriften
# f�r die Produktpalette 2). Die Kombinationen haben die Form: [PP21/PP22]_[Tw/Pst/Js/TuK/Rg/Hb]_[1/2/3/4]
PP2_columns <- apply(expand.grid(PP, products, questions), 1, function(x) paste(x[1], x[2], x[3], sep="_"))

#### Impulskauftendenz Rook
rook_columns <- paste0("rook_", 1:9)
#### Impulskauftendenz Verplanken
verp_columns <- paste0("verp_", 1:20)

#### Spalten�berschriften der Gef�hle
feelings_columns <- as.vector(sapply(c("GefV","GefL"),function(x) {paste(x, 1:4, sep="_")}))

#### Attraktivit�t des Befragungsortes 
# Raumatmosph�re & Lageratmosph�re
attractiveness <- c("VAtm", "LAtm")
# F�r jeden Ort wurden 3 Fragen gestellt
questions <- 1:3
# Erzeuge alle m�glichen Kombinationen der f�r die Atmosph�re - sowohl f�r Verkaufsraum
# als auch f�r Lager. Die m�glichen Kombinationen sind: [VAtm,LAtm]_[1/2/3]
attr_columns <- apply(expand.grid(attractiveness, questions), 1, function(x) paste(x[1], x[2], sep="_"))

# �bernehme aus den Rohdaten nur die Spalten f�r die wir oben definiert haben 
# Spalten�berschriften generiert haben. Weiters wissen wir dass alle Zeilen mit
# dispcode 31 beendete Frageb�gen sind.
d.cleared <- d.raw[d.raw$dispcode == 31, c(PP1_columns, PP2_columns, "Ort_PP1", attr_columns, feelings_columns, rook_columns, verp_columns, "ort_1.bewertung", "vpn", "age")]

# Fehlende Werte werden mit -77 gekennzeichnet. Ersetze -77 mit NA (not available)
d.cleared[d.cleared == -77] <- NA

# Problem: Wurde Produktpalette 1 als erstes gezeigt so haben wir Werte f�r die Variablen
# PP11_.... Die entsprechenden Werte f�r die Variablen PP12 sind daher NA gesetzt. F�r die
# sp�tere Analyse fassen wir die Werte zusammen: d.h. wir �berschreiben alle NA - Werte in 
# PP11_... mit den entsprechenden Werten aus PP12_...
tmp <- d.cleared[, grepl("PP11", names(d.cleared))]
tmp2 <- d.cleared[, grepl("PP12", names(d.cleared))]
tmp[is.na(tmp)] <- tmp2[is.na(tmp)]
d.tmp <- tmp
# Analaoge Zusammenf�hrung der Daten wie oben
tmp <- d.cleared[, grepl("PP21", names(d.cleared))]
tmp2 <- d.cleared[, grepl("PP22", names(d.cleared))]
tmp[is.na(tmp)] <- tmp2[is.na(tmp)]

# Zusammenf�hren der bereinigten Daten und Anreicherung mit Ort_PP1 sowie Atmosph�ren-Fragen.
# Aus dem Ort_PP1 kann abgeleitet werden ob eine Bewertung im Verkaufsraum oder im Lager stattgefunden hat.
d.cleared <- cbind(d.tmp, tmp , d.cleared[, c("Ort_PP1", attr_columns, feelings_columns, rook_columns, verp_columns, "ort_1.bewertung", "vpn", "age")])

# Die ersten beiden Zeilen im Datenset enthalten nur Testdaten und m�ssen f�r die sp�tere Analyse ausgeschlossen werden
d.cleared <- d.cleared[-(1:2),]

# Der letzte Buchstabe aus Ort_PP1 gibt an ob Produktpalette 1 im Verkaufsraum (=v) oder im Lager (=h) ausgestellt war.
location_string <- substr(as.character(d.cleared$Ort_PP1), nchar(as.character(d.cleared$Ort_PP1)), nchar(as.character(d.cleared$Ort_PP1))) 
d.cleared$PP1_location <- ifelse(location_string == "v", "Store", "Warehouse")

# Wurde PP1 im Verkaufsraum ausgestellt so ist klar dass Produktpalette 2 im Lager ausgestellt wurde und vice versa. 
d.cleared$PP2_location <- ifelse(d.cleared$PP1_location == "Store", "Warehouse", "Store")


#################################### Datenkorrektur ############################################################
# Bei Lageratmosph�re (LAtm1) ist es in Unipark zu dem Problem gekommen, dass eine 8 in den Daten eigentlich eine 6 sein sollte.
# Bei Verkaufsraumatmosph�re (VAtm1) ist es in Unipark zu dem Problem gekommen, dass eine 9 in den Daten eigentlich eine 6 sein sollte.
# siehe Codebuch (Seite 38 bzw. 36) f�r weitere Informationen.

d.cleared[d.cleared$LAtm_1 == 8, "LAtm_1"] <- 6
d.cleared[d.cleared$VAtm_1 == 9, "VAtm_1"] <- 6

# Umkodieren von Item 8 rook
d.cleared$rook_8 <- 6 - d.cleared$rook_8

# Umkodieren von Items 1, 3, 7, 8, 9, 11, 13, 15
d.cleared[,paste0("verp_",c(1, 3, 7, 8, 9, 11, 13, 15))] <- sapply(c(1, 3, 7, 8, 9, 11, 13, 15),
                                                                   function(x){d.cleared[,paste0("verp_",x)] <- 8 - d.cleared[,paste0("verp_",x)]})


############################################ average and SD of age of participants #################################################
mean(d.cleared$age)
sd(d.cleared$age)

########################################## Cronbachs alpha Produkte ################################################
# Beginnen von hier befinden sind in d.cleared alle relevanten Daten f�r die Analyse
# Beachte, dass es ab hier nur noch PP11 und PP21 gibt da die Werte von PP12 zu PP11 und jene von PP22 zu PP21 hinzugef�gt wurden.

### Hier wird das Cronbach Alpha f�r jedes Produkt ausgerechnet.
### M�chte man das Cronbach Alpha f�r ein bestimmtes Produkt berechnen, so muss
### im Befehl unten die entsprechende Produktabk�rzung eingef�gt werden.
### M�gliche Werte f�r die verschiedenen Produkte sind: Tg, Pg, Jb, TuZ, Rs, Hh, Tw, Pst, Js, TuK, Rg, Hb
product_columns <- grepl("TuK", names(d.cleared))
psych::alpha(d.cleared[,product_columns])

########################################## Cronbachs alpha Lager/Verkaufsraum ################################################
### Hier wird das Cronbach Alpha f�r jeden Bewertungsort ausgerechnet.
### M�chte man das Cronbach Alpha f�r einen bestimmtes Ort (Lager oder Verkausraum) berechnen, so muss
### im Befehl unten die entsprechende Ortabk�rzung (LAtm bzw. VAtm) eingef�hrt werden. 
### LAtm = Lageratmosph�re
### VAtm = Verkaufsraumatmosph�re
Ort_columns <- grepl("VAtm", names(d.cleared))
psych::alpha(d.cleared[,Ort_columns])

########################################## Cronbachs Rook-Skala ################################################
rook_columns <- grepl("rook", names(d.cleared))
psych::alpha(d.cleared[, rook_columns])

########################################## Cronbachs Verplanken-Skala ################################################
verp_columns <- grepl("verp", names(d.cleared))
psych::alpha(d.cleared[, verp_columns])

########################################## Mittelwerte pro Produkt ################################################
# alle Saplten f�r die Fragen 1-3 in eine lange Liste untereinander schreiben (Produktpalette 1)
df.product_1_sumed <- melt(cbind(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))], PP1_location = d.cleared$PP1_location),
                           id.vars = "PP1_location",
                           variable.name = "Produkt_Frage",
                           value.name = "Rating")
df.product_1_sumed$Produkt_Frage <- sub("_[1-4]", "", df.product_1_sumed$Produkt_Frage) # l�sche f�r jede Variable die Kennung um welche Frage es sich handelt.
# Mittelwert pro Produkt bilden mit Unterscheidung zwischen Store und Warehouse
df.product_1_total_means <- aggregate(Rating ~ PP1_location + Produkt_Frage, data = df.product_1_sumed, mean)
# total means of products
df.total_means_PP1 <- aggregate(Rating ~ Produkt_Frage, data = df.product_1_sumed, mean)
# total sd of products
df.total_sd_PP1 <- aggregate(Rating ~ Produkt_Frage, data = df.product_1_sumed, sd)

names(df.product_1_total_means)[names(df.product_1_total_means) == 'PP1_location'] <- 'Location'
# alle Saplten f�r die Fragen 1-3 in eine lange Liste untereinander schreiben (Produktpalette 2)
df.product_2_sumed <- melt(cbind(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))], PP2_location = d.cleared$PP2_location),
                           id.vars = "PP2_location",
                           variable.name = "Produkt_Frage",
                           value.name = "Rating")
df.product_2_sumed$Produkt_Frage <- sub("_[1-4]", "", df.product_2_sumed$Produkt_Frage) # l�sche f�r jede Variable die Kennung um welche Frage es sich handelt.
df.product_2_total_means <- aggregate(Rating ~ PP2_location + Produkt_Frage, data = df.product_2_sumed, mean)
# total means of products
df.total_means_PP2 <- aggregate(Rating ~ Produkt_Frage, data = df.product_2_sumed, mean)
# total sd of products
df.total_sd_PP2 <- aggregate(Rating ~ Produkt_Frage, data = df.product_2_sumed, sd)
names(df.product_2_total_means)[names(df.product_2_total_means) == 'PP2_location'] <- 'Location'

# Mittwerte von PP1 und PP2 in einer Tabelle zusammenfassen (untereinander schreiben)
df.all_means <- rbind(df.product_1_total_means, df.product_2_total_means)

# Definiere neue x-Achsen-Beschriftung f�r die Grafik
labels_x_axis <- c("Trousers_light", "Jacket_blue", "Jumper_grey", "Skirt_black", "T-Shirt_grey", "Scarf_zigzag",
                   "Trousers_blue", "Jacket_black", "Jumper_striped", "Skirt_grey", "Scarf_checked", "T-Shirt_white")

ggplot(df.all_means, aes(Produkt_Frage, Rating)) +
  geom_point(aes(colour = Location), size = 4) + # Datenpunkte je nach location f�rben und Gr��e �ndern
  labs(color = "Location") + # Legende beschriften
  scale_x_discrete(labels = labels_x_axis) + # l�sche PP11_ aus Variablennamen
  scale_color_manual(labels = c("Salesroom", "Warehouse"), values = c("#F8766D", "#00BA38")) +
  labs(x = "Products") +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Beschriftung x-Achse um 90� drehen


########################################## Dichte der Atmosph�re ################################################
# F�r jeden Probanden berechnen wir den Mittelwert von VAtm (Verkaufsraumatmosph�re) sowie von LAtm (Lageratmosph�re)
d.means_VAtm <- data.frame(Store = rowMeans(d.cleared[, grepl("VAtm_[1-3]", names(d.cleared))]))
d.means_LAtm <- data.frame(Warehouse = rowMeans(d.cleared[, grepl("LAtm_[1-3]", names(d.cleared))]))


# Mittelwerte von VAtm und LAtm in einer langen Liste zusammenfassen (untereinander schreiben)
d.atmosphere <- melt(cbind(d.means_VAtm, d.means_LAtm),
                     variable.name = "Location",
                     value.name = "Roomattractiveness")
# Mittwerte von VAtm und LAtm ermitteln (wichtig f�r die Grafik)
d.means <- aggregate(d.atmosphere$Roomattractiveness, list(d.atmosphere$Location), mean)
# SD von VAtm und LAtm ermitteln (wichtig f�r die Grafik)
d.sd <- aggregate(d.atmosphere$Roomattractiveness, list(d.atmosphere$Location), sd)

# Dichte f�r LAtm und VAtm sowie deren Mittelwert zeichnen
ggplot(d.atmosphere, aes(x = Roomattractiveness, color = Location)) +
  geom_density() +
  geom_vline(data = d.means, aes(xintercept = x, color = Group.1), linetype="dashed")

# Dichte f�r LAtm und VAtm sowie deren Mittelwert zeichnen
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


########################################## t-test Raumattraktivit�t ################################################
t.test(d.means_VAtm$Store, d.means_LAtm$Warehouse, paired=TRUE)
# t(74) = 8.249 ---> p < .001
mean(d.means_VAtm$Store)
mean(d.means_LAtm$Warehouse)


############################################ prepare the data for the LM/GLM - model #################################################
d.cleared$VAtm_mean <- rowMeans(d.cleared[, grepl("VAtm_[1-3]", names(d.cleared))])
d.cleared$LAtm_mean <- rowMeans(d.cleared[, grepl("LAtm_[1-3]", names(d.cleared))])
d.cleared$rook_mean <- rowMeans(d.cleared[, grepl("rook_[1-9]", names(d.cleared))])
d.cleared$verp_mean <- rowMeans(d.cleared[, grepl("verp_([0-9]|[1-2][0-9])", names(d.cleared))])
d.cleared$impuls_mean <- rowMeans(d.cleared[, c("rook_mean", "verp_mean")])
d.cleared$PP1_means <- rowMeans(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])
d.cleared$PP2_means <- rowMeans(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])
d.cleared$PP1_location <- as.factor(d.cleared$PP1_location)
d.cleared$PP2_location <- as.factor(d.cleared$PP2_location)

# define columns so that we know which location corresponds to which product group
Columns_Verkaufsraum_PP1 <- c("vpn", "PP1_means", "PP1_location", "VAtm_mean", "GefV_1", "GefV_2", "GefV_3", "GefV_4", "impuls_mean", "ort_1.bewertung")
Columns_Lager_PP1 <- c("vpn", "PP1_means", "PP1_location", "LAtm_mean", "GefL_1", "GefL_2", "GefL_3", "GefL_4", "impuls_mean", "ort_1.bewertung")
Columns_Verkaufsraum_PP2 <- c("vpn", "PP2_means", "PP2_location", "VAtm_mean", "GefV_1", "GefV_2", "GefV_3", "GefV_4", "impuls_mean", "ort_1.bewertung")
Columns_Lager_PP2 <- c("vpn", "PP2_means", "PP2_location", "LAtm_mean", "GefL_1", "GefL_2", "GefL_3", "GefL_4", "impuls_mean", "ort_1.bewertung")

# pick correspoding columns depending on location --> for each participant we get two rows
# one for prouctrating in salesroom and one for product rating in warehouse
d.Columns_Verkaufsraum_PP1 <- d.cleared[d.cleared$PP1_location == "Store", Columns_Verkaufsraum_PP1]
d.Columns_Lager_PP1 <- d.cleared[d.cleared$PP1_location == "Warehouse", Columns_Lager_PP1]
d.Columns_Verkaufsraum_PP2 <- d.cleared[d.cleared$PP2_location == "Store", Columns_Verkaufsraum_PP2]
d.Columns_Lager_PP2 <- d.cleared[d.cleared$PP2_location == "Warehouse", Columns_Lager_PP2]

# define generailzed header for the location / product group combinations                                        
columns_names <- c("vpn", "Y_Produktbewertung_gemittelt", "Location", "Raumattraktivit�t", "Gef�hl_1", "Gef�hl_2", "Gef�hl_3", "Gef�hl_4", "impuls_mean", "ort_1.bewertung")

# apply generalized header 
names(d.Columns_Verkaufsraum_PP1) <- columns_names
names(d.Columns_Lager_PP1) <- columns_names
names(d.Columns_Verkaufsraum_PP2) <- columns_names
names(d.Columns_Lager_PP2) <- columns_names

# bind everything rowwise together  
d.GLM_input <- rbind(d.Columns_Verkaufsraum_PP1,d.Columns_Lager_PP1, d.Columns_Verkaufsraum_PP2,d.Columns_Lager_PP2 )
# calculate mean-fealings for every row
d.GLM_input$Gef�hl_mean <- rowMeans(d.GLM_input[,paste0("Gef�hl_",1:4)])
# define the loaction of the first product rating as a factor (needed for GLM)
d.GLM_input$ort_1.bewertung <- as.factor(d.GLM_input$ort_1.bewertung)


############################################ Regression/Moderation  #################################################

# Regression for Store
fit.store <- lm(Y_Produktbewertung_gemittelt ~ Raumattraktivit�t, data = d.GLM_input[d.GLM_input$Location == "Store",])
summary(fit.store)

# Is the same as the p-value of the coefficient used in the regression
cor.test(d.GLM_input[d.GLM_input$Location == "Store", "Y_Produktbewertung_gemittelt"],
         d.GLM_input[d.GLM_input$Location == "Store", "Raumattraktivit�t"],
         method = "pearson")

# regression-plot for Produktbewertung VS Raumattraktivit�t
regStore <- ggplot(d.GLM_input[d.GLM_input$Location == "Store",],
                   aes(x = Raumattraktivit�t, y = Y_Produktbewertung_gemittelt)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm) + # Add linear regression line (by default includes 95% confidence region)
  labs(title = "Salesroom", x = "Roomattractiveness", y = "Product Rating") +
  scale_fill_grey() + theme_classic(base_size = 17)

# Regression for Warehouse
fit.warehouse <- lm(Y_Produktbewertung_gemittelt ~ Raumattraktivit�t, data = d.GLM_input[d.GLM_input$Location == "Warehouse",])
summary(fit.warehouse)

# Is the same as the p-value of the coefficient used in the regression
cor.test(d.GLM_input[d.GLM_input$Location == "Warehouse", "Y_Produktbewertung_gemittelt"],
         d.GLM_input[d.GLM_input$Location == "Warehouse", "Raumattraktivit�t"],
         method = "pearson")

# regression-plot for Produktbewertung VS Raumattraktivit�t
regWare <- ggplot(d.GLM_input[d.GLM_input$Location == "Warehouse",],
                  aes(x = Raumattraktivit�t, y = Y_Produktbewertung_gemittelt)) + 
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm) + # Add linear regression line (by default includes 95% confidence region)
  labs(title = "Warehouse", x = "Roomattractiveness", y = "Product Rating") +
  scale_fill_grey() + theme_classic(base_size = 17)

# arrange regression plots side by side
cowplot::plot_grid(regWare, regStore)

# calculate total mean-product rating 
df.means_products <- data.frame(total_product_rating = rowMeans(d.cleared[, grepl("[PP11|PP21]_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))]))
# calculate total room-attractiveness rating
df.fit_location <- cbind(df.means_products, data.frame(total_location_rating = rowMeans(cbind(d.means_VAtm, d.means_LAtm))))

# regression for total mean-product rating and total room-attractiveness rating
fit.location <- lm(total_product_rating ~ total_location_rating, data = df.fit_location)
summary(fit.location)
cor.test(df.fit_location$total_product_rating, df.fit_location$total_location_rating, method = "pearson")


############################################ GLM  #################################################
# GLM - Model with all components
fit.all_comp <- glm(Y_Produktbewertung_gemittelt ~ Location+Raumattraktivit�t+Gef�hl_mean+impuls_mean+ort_1.bewertung,data=d.GLM_input,family=gaussian)
summary.lm(fit.all_comp)

# GLM - Model Impulskauftendenz with Interaction
fit.loc_impuls <- glm(Y_Produktbewertung_gemittelt ~ Location*impuls_mean,data=d.GLM_input,family=gaussian)
summary.lm(fit.loc_impuls)

# GLM - Model Impulskauftendenz with Interaction
fit.attr_impuls <- glm(Y_Produktbewertung_gemittelt ~ Raumattraktivit�t*impuls_mean,data=d.GLM_input,family=gaussian)
summary.lm(fit.attr_impuls)


#####################################################################################################################
############################################# Explorative analysis ##################################################
#####################################################################################################################

############################################ Correlation of Gef�hle #################################################
cor(d.GLM_input[,paste0("Gef�hl_", 1:4)])

############################################ Correlation of Gef�hle 1 -4 #################################################
cor(d.GLM_input[,paste0("Gef�hl_", 1:4)])
cor(d.GLM_input[d.GLM_input$Location == "Store",paste0("Gef�hl_", 1:4)])
cor(d.GLM_input[d.GLM_input$Location == "Warehouse",paste0("Gef�hl_", 1:4)])

############################################ Correlation of Gef�hle_mean & Produktbewertung #################################################
cor(d.GLM_input$Gef�hl_mean, d.GLM_input$Y_Produktbewertung_gemittelt)
cor(d.GLM_input[d.GLM_input$Location == "Store", "Gef�hl_mean"],
    d.GLM_input[d.GLM_input$Location == "Store", "Y_Produktbewertung_gemittelt"])
cor(d.GLM_input[d.GLM_input$Location == "Warehouse", "Gef�hl_mean"],
    d.GLM_input[d.GLM_input$Location == "Warehouse", "Y_Produktbewertung_gemittelt"])
############################################ Correlation of Gef�hle_mean & Attraktivit�t #################################################
cor(d.GLM_input$Gef�hl_mean, d.GLM_input$Raumattraktivit�t)
cor(d.GLM_input[d.GLM_input$Location == "Store", "Gef�hl_mean"],
    d.GLM_input[d.GLM_input$Location == "Store", "Raumattraktivit�t"])
cor(d.GLM_input[d.GLM_input$Location == "Warehouse", "Gef�hl_mean"],
    d.GLM_input[d.GLM_input$Location == "Warehouse", "Raumattraktivit�t"])

############################################ Correlation of impuls_mean & Raumattraktivit�t #################################################
cor(d.GLM_input$impuls_mean, d.GLM_input$Raumattraktivit�t)
cor.test(d.GLM_input$impuls_mean, d.GLM_input$Raumattraktivit�t)

# Regression with Moderation
fit.moderation <- lm(Y_Produktbewertung_gemittelt ~ Raumattraktivit�t * Gef�hl_mean, data = d.GLM_input)
summary(fit.moderation)

