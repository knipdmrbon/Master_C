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

############################################ prepare the data for the GLM - model #################################################

d.cleared$VAtm_mean <- rowMeans(d.cleared[, grepl("VAtm_[1-3]", names(d.cleared))])
d.cleared$LAtm_mean <- rowMeans(d.cleared[, grepl("LAtm_[1-3]", names(d.cleared))])
d.cleared$rook_mean <- rowMeans(d.cleared[, grepl("rook_[1-9]", names(d.cleared))])
d.cleared$verp_mean <- rowMeans(d.cleared[, grepl("verp_([0-9]|[1-2][0-9])", names(d.cleared))])
d.cleared$impuls_mean <- rowMeans(d.cleared[, c("rook_mean", "verp_mean")])
d.cleared$PP1_means <- rowMeans(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])
d.cleared$PP2_means <- rowMeans(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])
d.cleared$PP1_location <- as.factor(d.cleared$PP1_location)
d.cleared$PP2_location <- as.factor(d.cleared$PP2_location)

Columns_Verkaufsraum_PP1 <- c("vpn", "PP1_means", "PP1_location", "VAtm_mean", "GefV_1", "GefV_2", "GefV_3", "GefV_4", "impuls_mean", "ort_1.bewertung")
Columns_Lager_PP1 <- c("vpn", "PP1_means", "PP1_location", "LAtm_mean", "GefL_1", "GefL_2", "GefL_3", "GefL_4", "impuls_mean", "ort_1.bewertung")
Columns_Verkaufsraum_PP2 <- c("vpn", "PP2_means", "PP2_location", "VAtm_mean", "GefV_1", "GefV_2", "GefV_3", "GefV_4", "impuls_mean", "ort_1.bewertung")
Columns_Lager_PP2 <- c("vpn", "PP2_means", "PP2_location", "LAtm_mean", "GefL_1", "GefL_2", "GefL_3", "GefL_4", "impuls_mean", "ort_1.bewertung")

d.Columns_Verkaufsraum_PP1 <- d.cleared[d.cleared$PP1_location == "Store", Columns_Verkaufsraum_PP1]
d.Columns_Lager_PP1 <- d.cleared[d.cleared$PP1_location == "Warehouse", Columns_Lager_PP1]
d.Columns_Verkaufsraum_PP2 <- d.cleared[d.cleared$PP2_location == "Store", Columns_Verkaufsraum_PP2]
d.Columns_Lager_PP2 <- d.cleared[d.cleared$PP2_location == "Warehouse", Columns_Lager_PP2]
                                        
columns_names <- c("vpn", "Y_Produktbewertung_gemittelt", "Location", "Raumattraktivit�t", "Gef�hl_1", "Gef�hl_2", "Gef�hl_3", "Gef�hl_4", "impuls_mean", "ort_1.bewertung")

names(d.Columns_Verkaufsraum_PP1) <- columns_names
names(d.Columns_Lager_PP1) <- columns_names
names(d.Columns_Verkaufsraum_PP2) <- columns_names
names(d.Columns_Lager_PP2) <- columns_names

d.GLM_input <- rbind(d.Columns_Verkaufsraum_PP1,d.Columns_Lager_PP1, d.Columns_Verkaufsraum_PP2,d.Columns_Lager_PP2 )
d.GLM_input$Gef�hl_mean <- rowMeans(d.GLM_input[,paste0("Gef�hl_",1:4)])


d.GLM_input$ort_1.bewertung <- as.factor(d.GLM_input$ort_1.bewertung)

# GLM - Model with all components
fit.all_comp <- glm(Y_Produktbewertung_gemittelt ~ Location+Raumattraktivit�t+Gef�hl_mean+impuls_mean+ort_1.bewertung,data=d.GLM_input,family=gaussian)
summary.lm(fit.all_comp)

# GLM - Model Impulskauftendenz with Interaction
fit.loc_impuls <- glm(Y_Produktbewertung_gemittelt ~ Location*impuls_mean,data=d.GLM_input,family=gaussian)
summary.lm(fit.loc_impuls)

# GLM - Model Impulskauftendenz with Interaction
fit.attr_impuls <- glm(Y_Produktbewertung_gemittelt ~ Raumattraktivit�t*impuls_mean,data=d.GLM_input,family=gaussian)
summary.lm(fit.attr_impuls)

write.csv(d.GLM_input, file = "GLM_input.csv", row.names = FALSE)

############################################ END GLM - model #################################################

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

cowplot::plot_grid(regWare, regStore)

# Regression with Moderation
fit.moderation <- lm(Y_Produktbewertung_gemittelt ~ Raumattraktivit�t * Gef�hl_mean, data = d.GLM_input)
summary(fit.moderation)

############################################ END Regression #################################################

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

############################################ average and SD of age of participants #################################################
mean(d.cleared$age)
sd(d.cleared$age)


