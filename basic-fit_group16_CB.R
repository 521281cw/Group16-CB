basicfit <- BasicFit_WebScraping
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

#TEXT CLEANING#

# check if we do not miss any values
mean(BasicFit_WebScraping$"rating")
summary(BasicFit_WebScraping)

# apparently rating is seen as character values
# convert ratings to numeric values
as.numeric(basicfit$rating)
basicfit$rating_numeric <- as.numeric(basicfit$rating)
summary(basicfit)


#LEXICONS#
# see appendix B for an English overview

personeel_positief_lexicon <- c("vriendelijk", "vrolijk", "behulpzaam", 
                                "professioneel", "lief", "goed geholpen", 
                                "hartelijk", "zorgvuldig", "enthousiast", 
                                "leuk", "aangenaam", "attent", "klantgericht", 
                                "beleefd", "goed personeel") 

personeel_negatief_lexicon <- c("onaardig", "niemand aanwezig", "geen personeel", 
                                "geen medewerker", "geen begeleiding", 
                                "onbehulpzaam", "onprofessioneel", "onbeleefd", 
                                "klantonvriendelijk", "onattent", "ongepast", 
                                "onzorgvuldig", "respectloos", "onervaren", 
                                "onverantwoordelijk")

klantenservice_positief_lexicon <- c("vlot geholpen", "snel antwoord", 
                                     "probleem oplossend", "klanttevredenheid",
                                     "snelle klantenservice", "duidelijke klantenservice",
                                     "top klantenservice", "goede klantenservice", 
                                     "uitstekende service", "professionele klantenservice",
                                     "klantgericht", "behulpzame klantenservice",
                                     "efficiënte klantenservice", 
                                     "vriendelijke klantenservice", 
                                     "klantvriendelijke klantenservice")

klantenservice_negatief_lexicon <- c("helpt niet", "slecht", "onbereikbaar",
                                     "chatbot", "misinformatie", 
                                     "trage klantenservice", "langzame klantenservice", 
                                     "slechte klantenservice", "schande", 
                                     "onvriendelijk", "onvriendelijke", 
                                     "vervelende klantenservice", 
                                     "waardeloze klantenservice", 
                                     "klantenservice onbereikbaar", 
                                     "nutteloze klantenservice")

hygiëne_positief_lexicon <- c("schoon", "hygiënisch", "netjes",
                              "verzorgd", "opgeruimd", "fris", 
                              "brandschoon", "glanzend", "steriel", 
                              "netheid", "keurig", "proper", 
                              "sanitair", "vlekkeloos",
                              "gedesinfecteerd")

hygiëne_negatief_lexicon <- c("onhygiënisch", "vies", "rommelig", "slordig",
                              "smoezelig", "vuil", "muf", "stinkt", 
                              "geen hygiëne", "hygiene zou beter kunnen", 
                              "smerig", "slordig", "onfris", "verwaarloosd",
                              "niet fris")

inschrijving_positief_lexicon <- c("rondleiding", "duidelijke uitleg", 
                                   "makkelijk", "gemakkelijk", 
                                   "vlotte registratie", "simpel inschrijven",
                                   "snelle aanmelding", "inschrijven duidelijk",
                                   "duidelijk", "vlot", "eenvoudig",
                                   "persoonlijk filmpje", "snel inschrijven",
                                   "makkelijk inschrijven", "meteen sporten")

inschrijving_negatief_lexicon <- c("moeilijk", "lastig", 
                                   "duurt lang", "onduidelijk", 
                                   "inschrijven moeilijk", 
                                   "niet gelukt", "veel werk", 
                                   "niet duidelijk", "niet ingediend", 
                                   "inschrijven moeizaam", "bevestigingsmail laat", 
                                   "late bevestigingsmail", "inschrijven problematisch", 
                                   "mail niet ontvangen", "aanmelden onhandig")

app_positief_lexicon <- c("gebruiksvriendelijk", "overzichtelijk", 
                          "toegankelijk", "mobielvriendelijk", "functioneel",
                          "praktisch", "handige app", "betrouwbare app",
                          "duidelijke app", "schema", "simpele app",
                          "goeie app", "efficiënte app", "stabiele app",
                          "effectieve app")


app_negatief_lexicon <- c("bug", "bugs", "geboortedatum", 
                          "onduidelijke app", "loopt vast",
                          "onhandige bediening", "trage app", 
                          "slechte app", "foutmelding app", 
                          "error app", "app gegooid", "onveilige app",
                          "vastlopende app", "appt werkt niet", 
                          "verwarrende app")

website_positief_lexicon <- c("handige website", "duidelijke website", 
                              "betrouwbare website", "overzichtelijke website", 
                              "propere site", "toegankelijke wesite", 
                              "website is erg duidelijk", "makkelijke website", "gemakkelijke website", 
                              "efficiënte website", "gebruiksvriendelijke website", "stabiele website", "eenvoudige website", 
                              "prima website", "snel ladende website")

website_negatief_lexicon <- c("onduidelijke website", "onoverzichtelijke website", 
                              "verwarrende website", "trage website", 
                              "moelijke site", "slechte wesite", 
                              "problemen inloggen", "storing", "problemen website", 
                              "informatie onduidelijk", "foutmelding website", "onstabiele website", "error website", 
                              "onbetrouwbare website", "informatie onvindbaar")

qrcode_positief_lexicon <- c("scanbaar", "handige code", 
                             "handige qr code", "handige qr-code", 
                             "snel inchecken", "efficiënte code", 
                             "eenvoudige code", "praktische code", "snelle toegang", 
                             "gebruiksvriendelijke code", "snel inchecken", "probleemloos inchecken", "snel scannen", 
                             "makkelijk scannen", "betrouwbaar inchecken")

qrcode_negatief_lexicon <- c("werkt niet", "scant niet", 
                             "problemen code", "probleem code", 
                             "onhandige code", "onpraktische code", 
                             "code doet het niet", "slechte code", "foutmelding code", 
                             "storing", "geen code", "geen qr code", "geen qr-code", 
                             "niet gescant", "geweigerd")

sporten_positief_lexicon <- c("begeleiding", "apparatuur", 
                              "onderhoud", "modern", 
                              "kwaliteit", "goede toestellen", 
                              "variatie", "relaxed", "relaxt", 
                              "nooit druk", "super fitness", "toffe sfeer", "toegangelijk", 
                              "uitdagend", "goed aanbod")

sporten_negatief_lexicon <- c("verouderd", "versleten", 
                              "defect", "druk", 
                              "niet relaxed", "niet relaxt", 
                              "ouderwetse apparatuur", "weinig apparaten", "niet opruimen", 
                              "slechte begeleiding", "onvoldoende toestellen", "onveilig", "weinig begeleiding", 
                              "beperkte keuze", "slecht onderhouden")

abonnement_positief_lexicon <- c("goede prijs-kwaliteit", "makkelijk opzeggen", 
                                 "duidelijke voorwaarden", "flexibel abonnement", 
                                 "gemakkelijk opzeggen", "eenvoudig abonnement", 
                                 "flexibele opties", "voordelige abonnementen", "gunstige tarieven", 
                                 "soepele opzegging", "geen verborgen kosten", "transparantie", "keuzevrijheid", 
                                 "veel keuze", "veel mogelijkheden")

abonnement_negatief_lexicon <- c("opzegtermijn", "misleidend", 
                                 "storing", "technische storing", 
                                 "opzeggen ingewikkeld", "lange opzegtermijn", 
                                 "moeilijk opzeggen", "beperkt abonnement", "verwarring abonnement", 
                                 "slechte voorwaarden", "hoge inschrijfkosten", "verkeerd bedrag", "hoog bedrag", 
                                 "aanmaning", "geen reactie")

prijs_positief_lexicon <- c("goedkoop", "betaalbaar", 
                            "voordelig", "voordelige", 
                            "aantrekkelijk", "deal", 
                            "budget", "korting", "overpriced", 
                            "redelijke prijs", "beste prijs/kwaliteit", "goede prijs/kwaliteit", "goede prijs", 
                            "faire prijs", "prijs is goed")

prijs_negatief_lexicon <- c("duur", "duurder", 
                            "prijzig", "onbetaalbaar", 
                            "verborgen kosten", "valse reclame", 
                            "pittige maandprijs", "hoge kosten", "overdreven prijzen", 
                            "onbetaalbaar", "onaantrekkelijk", "onvoordelig", "geldklopperij", 
                            "absurde prijzen", "niet waard")

#LEXICONS CATEGORIES#

categorie_personeel_lexicon <- c("personeel", "medewerker", "medewerkers")
categorie_klantenservice_lexicon <- c("klantenservice", "chatbot")
categorie_hygiëne_lexicon <- c("hygiene", "hygiëne", "schoon", "proper", 
                               "vies")
categorie_inschrijving_lexicon <- c("inschrijving", "inschrijven", "aanmelden",
                                    "aanmelding")
categorie_app_lexicon <- c("app")
categorie_website_lexicon <- c("website")
categorie_qrcode_lexicon <- c("qr code", "qr-code", "QR code", "QR-code", "QR", 
                              "qr", "code")
categorie_sporten_lexicon <- c("sporten", "toestellen", "apparaten", 
                               "apparatuur", "begeleiding")
categorie_abonnement_lexicon <- c("abonnement", "opzeggen", "opzegtermijn", 
                                  "abonnementen", "inschrijfkosten")
categorie_prijs_lexicon <- c("prijs", "betaling", "deal", "goedkoop", 
                             "duur", "geld", "kosten")

  
#nb_words preparation#
text_reviews <- c(basicfit$text)
nb_words <- function(lexicon,corpus){
  rowSums(sapply(lexicon, function(x) grepl(x, corpus)))}


#WORD COUNT CATGORIES#
basicfit$nb_words_categorie_personeel <- nb_words(lexicon=categorie_personeel_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_personeel)

basicfit$nb_words_categorie_klantenservice <- nb_words(lexicon=categorie_klantenservice_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_klantenservice)

basicfit$nb_words_categorie_hygiëne <- nb_words(lexicon=categorie_hygiëne_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_hygiëne)

basicfit$nb_words_categorie_inschrijving <- nb_words(lexicon=categorie_inschrijving_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_inschrijving)

basicfit$nb_words_categorie_app <- nb_words(lexicon=categorie_app_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_app)

basicfit$nb_words_categorie_website <- nb_words(lexicon=categorie_website_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_website)

basicfit$nb_words_categorie_qrcode <- nb_words(lexicon=categorie_qrcode_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_qrcode)

basicfit$nb_words_categorie_sporten <- nb_words(lexicon=categorie_sporten_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_sporten)

basicfit$nb_words_categorie_abonnement <- nb_words(lexicon=categorie_abonnement_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_abonnement)

basicfit$nb_words_categorie_prijs <- nb_words(lexicon=categorie_prijs_lexicon, corpus=text_reviews)
table(basicfit$nb_words_categorie_prijs)

###   REGRESSIONS   ###


#EMPLOYEES#
#positive#
basicfit$nb_words_personeel_positief <- nb_words(lexicon=personeel_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_personeel_positief)


reg_personeel_positief <- lm(rating_numeric~nb_words_personeel_positief,basicfit)
summary(reg_personeel_positief)

word_counts <- sapply(basicfit$nb_words_personeel_positief, function(word) {
  sum(grepl(word, text_reviews, ignore.case = TRUE))
})
word_counts

#negative#
basicfit$nb_words_personeel_negatief <- nb_words(lexicon=personeel_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_personeel_negatief)

reg_personeel_negatief <- lm(rating_numeric~nb_words_personeel_negatief,basicfit)
summary(reg_personeel_negatief)

word_counts <- sapply(nb_words_personeel_positief, function(word) {
  sum(grepl(word, text_reviews, ignore.case = TRUE))
})
word_counts

#CUSTOMER SERVICE#
#positive#
basicfit$nb_words_klantenservice_positief <- nb_words(lexicon=klantenservice_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_klantenservice_positief)

reg_klantenservice_positief <- lm(rating_numeric~nb_words_klantenservice_positief,basicfit)
summary(reg_klantenservice_positief)

#negative#
basicfit$nb_words_klantenservice_negatief <- nb_words(lexicon=klantenservice_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_klantenservice_negatief)

reg_klantenservice_negatief <- lm(rating_numeric~nb_words_klantenservice_negatief,basicfit)
summary(reg_klantenservice_negatief)

#HYGIENE#
#positive#
basicfit$nb_words_hygiene_positief <- nb_words(lexicon=hygiëne_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_hygiene_positief)

reg_hygiene_positief <- lm(rating_numeric~nb_words_hygiene_positief,basicfit)
summary(reg_hygiene_positief)

#negative#
basicfit$nb_words_hygiene_negatief <- nb_words(lexicon=hygiëne_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_hygiene_negatief)

reg_hygiene_negatief <- lm(rating_numeric~nb_words_hygiene_negatief,basicfit)
summary(reg_hygiene_negatief)

#REGISTRATION PROCESS#
#positive#
basicfit$nb_words_inschrijving_positief <- nb_words(lexicon=inschrijving_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_inschrijving_positief)

reg_inschrijving_positief <- lm(rating_numeric~nb_words_inschrijving_positief,basicfit)
summary(reg_inschrijving_positief)

#negative#
basicfit$nb_words_inschrijving_negatief <- nb_words(lexicon=inschrijving_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_inschrijving_negatief)

reg_inschrijving_negatief <- lm(rating_numeric~nb_words_inschrijving_negatief,basicfit)
summary(reg_inschrijving_negatief)

#APP#
#positive#
basicfit$nb_words_app_positief <- nb_words(lexicon=app_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_app_positief)

reg_app_positief <- lm(rating_numeric~nb_words_app_positief,basicfit)
summary(reg_app_positief)

#negative#
basicfit$nb_words_app_negatief <- nb_words(lexicon=app_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_app_negatief)

reg_app_negatief <- lm(rating_numeric~nb_words_app_negatief,basicfit)
summary(reg_app_negatief)

#WEBSITE#
#positive#
basicfit$nb_words_website_positief <- nb_words(lexicon=website_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_website_positief)

reg_website_positief <- lm(rating_numeric~nb_words_website_positief,basicfit)
summary(reg_website_positief)

#negative#
basicfit$nb_words_website_negatief <- nb_words(lexicon=website_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_website_negatief)

reg_website_negatief <- lm(rating_numeric~nb_words_website_negatief,basicfit)
summary(reg_website_negatief)

#QRCODE#
#positive#
basicfit$nb_words_qrcode_positief <- nb_words(lexicon=qrcode_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_qrcode_positief)

reg_qrcode_positief <- lm(rating_numeric~nb_words_qrcode_positief,basicfit)
summary(reg_qrcode_positief)

#negative#
basicfit$nb_words_qrcode_negatief <- nb_words(lexicon=qrcode_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_qrcode_negatief)

reg_qrcode_negatief <- lm(rating_numeric~nb_words_qrcode_negatief,basicfit)
summary(reg_qrcode_negatief)

#WORKING OUT EXPERIENCE#
#positive#
basicfit$nb_words_sporten_positief <- nb_words(lexicon=sporten_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_sporten_positief)

reg_sporten_positief <- lm(rating_numeric~nb_words_sporten_positief,basicfit)
summary(reg_sporten_positief)

#negative#
basicfit$nb_words_sporten_negatief <- nb_words(lexicon=sporten_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_sporten_negatief)

reg_sporten_negatief <- lm(rating_numeric~nb_words_sporten_negatief,basicfit)
summary(reg_sporten_negatief)

#MEMBERSHIPS#
#positive#
basicfit$nb_words_abonnement_positief <- nb_words(lexicon=abonnement_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_abonnement_positief)

reg_abonnement_positief <- lm(rating_numeric~nb_words_abonnement_positief,basicfit)
summary(reg_abonnement_positief)

#negative#
basicfit$nb_words_abonnement_negatief <- nb_words(lexicon=abonnement_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_abonnement_negatief)

reg_abonnement_negatief <- lm(rating_numeric~nb_words_abonnement_negatief,basicfit)
summary(reg_abonnement_negatief)

#PRICING/BILLING#
#positive#
basicfit$nb_words_prijs_positief <- nb_words(lexicon=prijs_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_prijs_positief)

reg_prijs_positief <- lm(rating_numeric~nb_words_prijs_positief,basicfit)
summary(reg_prijs_positief)

#negative#
basicfit$nb_words_prijs_negatief <- nb_words(lexicon=prijs_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_prijs_negatief)

reg_prijs_negatief <- lm(rating_numeric~nb_words_prijs_negatief,basicfit)
summary(reg_prijs_negatief)


###  MOST PROMINENT WORDS IN LEXICON   ###

# nb_words preparation
text_reviews <- c(basicfit$text)
nb_words <- function(lexicon,corpus){(sapply(lexicon, function(x) grepl(x, corpus)))}

#EMPLOYEES#
#positive#
basicfit$nb_words_personeel_positief <- nb_words(lexicon=personeel_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_personeel_positief)
table(basicfit$nb_words_personeel_positief)

#negative#
basicfit$nb_words_personeel_negatief <- nb_words(lexicon=personeel_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_personeel_negatief)
table(basicfit$nb_words_personeel_negatief)

#CUSTOMER SERVICE#
#positive#
basicfit$nb_words_klantenservice_positief <- nb_words(lexicon=klantenservice_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_klantenservice_positief)
table(basicfit$nb_words_klantenservice_positief)

#negative#
basicfit$nb_words_klantenservice_negatief <- nb_words(lexicon=klantenservice_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_klantenservice_negatief)
table(basicfit$nb_words_klantenservice_negatief)

#HYGIENE#
#positive#
basicfit$nb_words_hygiene_positief <- nb_words(lexicon=hygiëne_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_hygiene_positief)
table(basicfit$nb_words_hygiene_positief)

#negative#
basicfit$nb_words_hygiene_negatief <- nb_words(lexicon=hygiëne_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_hygiene_negatief)
table(basicfit$nb_words_hygiene_negatief)

#REGISTRATION PROCESS#
#positive#
basicfit$nb_words_inschrijving_positief <- nb_words(lexicon=inschrijving_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_inschrijving_positief)
table(basicfit$nb_words_inschrijving_positief)

#negative#
basicfit$nb_words_inschrijving_negatief <- nb_words(lexicon=inschrijving_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_inschrijving_negatief)
table(basicfit$nb_words_inschrijving_negatief)

#APP#
#positive#
basicfit$nb_words_app_positief <- nb_words(lexicon=app_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_app_positief)
table(basicfit$nb_words_app_positief)

#negative#
basicfit$nb_words_app_negatief <- nb_words(lexicon=app_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_app_negatief)
table(basicfit$nb_words_app_negatief)

#WEBSITE#
#positive#
basicfit$nb_words_website_positief <- nb_words(lexicon=website_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_website_positief)
table(basicfit$nb_words_website_positief)

#negative#
basicfit$nb_words_website_negatief <- nb_words(lexicon=website_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_website_negatief)
table(basicfit$nb_words_website_negatief)

#QRCODE#
#positive#
basicfit$nb_words_qrcode_positief <- nb_words(lexicon=qrcode_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_qrcode_positief)
table(basicfit$nb_words_qrcode_positief)

#negative#
basicfit$nb_words_qrcode_negatief <- nb_words(lexicon=qrcode_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_qrcode_negatief)
table(basicfit$nb_words_qrcode_negatief)

#WORKING OUT EXPERIENCE#
#positive#
basicfit$nb_words_sporten_positief <- nb_words(lexicon=sporten_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_sporten_positief)
table(basicfit$nb_words_sporten_positief)

#negative#
basicfit$nb_words_sporten_negatief <- nb_words(lexicon=sporten_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_sporten_negatief)
table(basicfit$nb_words_sporten_negatief)

#MEMBERSHIPS#
#positive#
basicfit$nb_words_abonnement_positief <- nb_words(lexicon=abonnement_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_abonnement_positief)
table(basicfit$nb_words_abonnement_positief)

#negative#
basicfit$nb_words_abonnement_negatief <- nb_words(lexicon=abonnement_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_abonnement_negatief)
table(basicfit$nb_words_abonnement_negatief)


#PRICING/BILLING#
#positive#
basicfit$nb_words_prijs_positief <- nb_words(lexicon=prijs_positief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_prijs_positief)
table(basicfit$nb_words_prijs_positief)

#negative#
basicfit$nb_words_prijs_negatief <- nb_words(lexicon=prijs_negatief_lexicon, corpus=text_reviews)
summary(basicfit$nb_words_prijs_negatief)
table(basicfit$nb_words_prijs_negatief)

mean(basicfit$rating_numeric)