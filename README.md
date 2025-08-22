# Biosimilarity Analysis - Analýza podobnosti účastníků

Moderní R skript pro komplexní analýzu podobnosti účastníků na základě jejich odpovědí v dotazníku. Skript využívá pokročilé statistické metody pro identifikaci skupin podobných účastníků a vytváří přehledné vizualizace výsledků.

## Popis skriptu

Tento skript provádí kompletní pipeline analýzy biosimilarity, která zahrnuje:

### Použité metody a algoritmy

1. **Clusterové algoritmy**:
   - **K-means clustering** s automatickou optimalizací počtu startů
   - **Hierarchické shlukování** s evaluací různých metod (average, single, complete, ward)
   - **AGNES** (Agglomerative Nesting) - bottom-up přístup
   - **DIANA** (Divisive Analysis) - top-down přístup

2. **Analýza vzdáleností**:
   - **Euklidovské vzdálenosti** mezi všemi páry účastníků
   - Symetrická matice vzdáleností pro vizualizaci

3. **Dimensionality reduction**:
   - **t-SNE** (t-distributed Stochastic Neighbor Embedding) pro 2D vizualizaci
   - Automatická optimalizace perplexity na základě velikosti vzorku

4. **Vizualizační techniky**:
   - **Dendrogramy** hierarchického shlukování
   - **Heatmapy** s barevným kódováním vzdáleností
   - **Force-directed grafy** pro síťovou vizualizaci
   - **t-SNE scatter ploty** s optimalizovaným umístěním textů

### Části skriptu

- **Načítání a preprocessing dat** - čištění CSV souboru, odstranění duplicit, konverze na numerická data
- **Výpočet vzdáleností** - matice Euklidovských vzdáleností mezi účastníky
- **Clusterová analýza** - aplikace různých algoritmů shlukování
- **t-SNE analýza** - redukce dimensionality pro vizualizaci
- **Generování reportů** - vytvoření PDF dokumentů s vizualizacemi
- **Export zkratek** - legendy pro zkrácená jména účastníků

### Výstupní soubory

Skript automaticky vytváří složku `./results/` a generuje:

- `biosimilarity_analysis.pdf` - hlavní PDF report s 5 stránkami analýz
- `detailed_heatmap.pdf` - detailní heatmapa ve vysokém rozlišení
- `participant_abbreviations.csv` - legenda zkratek jmen účastníků

## Vstupní CSV soubor

### Formát souboru

Vstupní CSV soubor musí mít následující vlastnosti:

- **Oddělovač**: středník (`;`)
- **Kódování**: UTF-8 (pro správné zobrazení českých znaků)
- **Hlavička**: první řádek obsahuje názvy sloupců

### Požadovaná struktura

```csv
Jméno a příjmení:;Otázka 1;Otázka 2;Otázka 3;...
Jan Novák;odpověď 1;odpověď 2;odpověď 3;...
Marie Svobodová;odpověď 1;odpověď 2;odpověď 3;...
```

### Důležité požadavky

1. **První sloupec** musí obsahovat jména účastníků (sloupec začínající "Jméno")
2. **Ostatní sloupce** obsahují odpovědi na otázky dotazníku
3. **Všechny odpovědi** budou automaticky převedeny na numerické hodnoty
4. **Duplicitní účastníci** jsou automaticky odstraněni (na základě jména)
5. **Sloupce s časovými značkami** jsou automaticky odstraněny

### Příklad vstupního souboru

```csv
Jméno a příjmení:;Banán loupu od:;Cereálie s mlékem servíruji tak, že:;Ananas na pizzu:
Dominika Adamová;stopky;nejprve nasypu cereálie;patří
Lucie Babušková;bubáka;nejprve naliji mléko;nepatří
Jakub Brabec;stopky;nejprve nasypu cereálie;nepatří
```

## Použití skriptu

### Základní spuštění

```r
# Spuštění s výchozím souborem
source("skript.R")

# Nebo přímo volání hlavní funkce
analysis_results <- run_biosimilarity_analysis("./data/2025.csv")
```

### Konfigurace parametrů

```r
# Změna počtu clusterů
CLUSTERING_GROUPS <- 4

# Změna maximálního počtu iterací t-SNE
TSNE_MAX_ITER <- 1500

# Spuštění analýzy
analysis_results <- run_biosimilarity_analysis("./data/muj_soubor.csv")
```

## Požadované R knihovny

```r
library(dplyr)        # Manipulace s daty
library(cluster)      # Algoritmy shlukování
library(pheatmap)     # Rozšířené heatmapy
library(qgraph)       # Síťové vizualizace
library(Rtsne)        # t-SNE algoritmus
```

## Instalace knihoven

```r
install.packages(c("dplyr", "cluster", "pheatmap", "qgraph", "Rtsne"))
```

## Doporučení pro použití

- **Skupiny do 20 lidí**: funguje bez problémů
- **Větší skupiny**: může být potřeba upravit velikosti grafů pro čitelnost
- **Menší skupiny**: může být problém s K-means shlukováním (doporučuje se snížit `CLUSTERING_GROUPS`)

## Webová verze

Pro rychlé použití bez instalace R je k dispozici [webová verze](https://dreryos.shinyapps.io/podobnostbio/) pro nahrání dotazníku.

## Příklad výstupu

![sample_pheatmap](sample_pheatmap.png)
