###############################################################################
# BASICS ZUM UMGANG MIT R
###############################################################################

# In R arbeitet man häufig mit Packages, die vorher installiert und geladen werden 
# müssen. Welche man braucht ist unterschiedlich.

# tidyverse ist eine Sammlung von mehreren Packages und eine Art R-Code zu schreiben. 
# Dieses Package braucht man eigentlich immer
install.packages("tidyverse") # Package installieren: das muss nur ein Mal gemacht werden
library(tidyverse) # Package laden: das mzss jedes Mal gemacht werden

# readxl brauchen wir, um Datensätze aus Excel zu importieren
install.packages("readxl")
library(readxl)

### DATENSÄTZE ####

# Datensätze laden
NAVCO1_2 <-read_excel("daten/NAVCO 1.2 Updated.xlsx") 
  # mit <- weise ich den Datensatz einem neuen Objekt zu, das ich NAVCO1_2 genannt habe
  # der Datensatz sollte jetzt oben rechts erscheinen. Ich kann draufklicken und ihn
  # anschauen.

# häufig sind Datensätze im CSV-Format. Die würde man mit der Funktion read.csv 
# (wenn Dezimalzeichen ein . ist) bzw. read.csv2 (wenn Dezimalzeichen , ist) einlesen

# Namen aller Variablen im Datensatz anschauen
names(NAVCO1_2)

# Struktur des Datensatzes anschauen:
str(NAVCO1_2)
  # numeric: numerisch, d.h. die Variable ist eine Zahl
  # character: character, d.h. die Variable ist Text
  # factor: Faktor, d.h. die Variable hat verschiedene Kategorien
  # logical: logisch, d.h. die Variable zwigt TRUE oder FALSE

# nachschauen wie viele Zeilen (d.h. Beobachtungen) der Datensatz hat
nrow(NAVCO1_2)
  
# Eine einzelne Variable / Spalte auswählen. Hier gibt es zwei Möglichkeiten: 
NAVCO1_2$CAMPAIGN # das ist base-R-Syntax

NAVCO1_2 |> 
  select(CAMPAIGN) # das ist tidyverse-Syntax

#### TIDYVERSE-SYNTAX ####
# Tidyverse arbeitet mit dem Pipe-Operator: |> , damit können mehrere Arbeitsschritte
# miteinander verbunden werden. Man kann sich |> ungefähr so vorstellen: "Nimm das 
# Ergebnis von links und mache damit rechts weiter."

# zum Beispiel:
NAVCO1_2 |> # "Nimm den Datensatz NAVCO1_2"
  select(CAMPAIGN, PARTICIPATION) |> # "Wähle daraus die Variablen CAMPAIGN und PARTCIPATION"
  filter(PARTICIPATION > 10000) |> # "Wähle nur campaigns, die eine höhere PARTICIPATION als 10000 haben"
  arrange(PARTICIPATION) # "Sortiere aufsteigend nach PARTICIPATION"

# Wichtig: wenn ich mit den eben ausgewählten, gefilterten und sortierten Daten weiterarbeiten
# möchte (und das möchte ich meistens), muss ich sie mit <- einem Objekt zuweisen. 
largepartip <- NAVCO1_2|> # Ich gebe dem neuen Objekt den Namen largepartip
  select(CAMPAIGN, PARTICIPATION) |> 
  filter(PARTICIPATION > 10000) |> 
  arrange(PARTICIPATION) 

# Wichtige Tidyverse-Funktionen: 

# filter(): Damit können wir bestimmte Fälle (d.h. Zeilen) auswählen
NAVCO1_2 |> filter (PARTICIPATION > 10000) # nur Fälle mit Participation > 10 000
NAVCO1_2 |> filter (LOCATION == "Afghanistan") # nur Fälle in Afganistan
NAVCO1_2 |> filter (LOCATION == "Afganistan" | LOCATION == "Algeria") # nur Fälle in Afganistan oder Algerien
NAVCO1_2 |> filter (LOCATION == "Algeria" & PARTICIPATION > 5000) # nur Fälle in Algerien mit > 5000 participants

# select(): Damit können wir bestimmte Variablen (d.h. Spalten) auswählen
NAVCO1_2 |> select(LOCATION, CAMPAIGN, PARTICIPATION) 

# arrange(): Damit können wir aufsteigend/absteigend sortieren
NAVCO1_2 |> arrange(PARTICIPATION) # sortiere aufsteigend nach Participation
NAVCO1_2 |> arrange(desc(PARTICIPATION)) # sortiere absteigend nach Participation

# mutate(): Damit können wir neue Variablen erstellen
NAVCO1_2 |> mutate(duration_years = EYEAR - BYEAR) 
  # ich nenne die neue Variable duration_years und berechne das End-Jahr der Campaign minus das
  # Beginn-Jahr der Campaign und bekomme so die Dauer in Jahren

# summarize(): Einfache deskriptive Kennzahlen berechnen
NAVCO1_2 |> summarize(
  mean_participation = mean(PARTICIPATION, na.rm = TRUE), # Falls es NAs gibt, werden die bei der Berechnung ignoriert.
  max_participation = max(PARTICIPATION, na.rm = TRUE),
  min_participation = min(PARTICIPATION, na.rm = TRUE), 
  sd_participation = sd(PARTICIPATION, na.rm = TRUE),
  median_participation = median(PARTICIPATION, na.rm = TRUE)
)

# group_by() und ungroup(): Damit können wir gruppieren und dann innerhalb der Gruppen arbeiten
NAVCO1_2 |> 
  group_by(LOCATION) |> #hier gruppiere ich pro Land
  summarize(
    number_campaigns = n(), # Zahl der Kampagnen pro Land
    mean_participation = mean(PARTICIPATION, na.rm = TRUE)) |> 
    # ich berechne den Durschnitt der Participation nicht wie oben allgemein sondern pro Land
  arrange (mean_participation) |> #ich sortiere die Länder nach ihrer durschnittlichen participation
  ungroup () # wichtig: Gruppierung weider aufheben
