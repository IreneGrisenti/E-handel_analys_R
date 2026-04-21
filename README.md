# E-handel analys R

Detta projekt syftar till att analysera e-handelsdata med hjälp av R för att identifiera mönster i försäljning, kundbeteende och returer.\
Genom statistiska analyser och visualiseringar tar vi fram insikter som kan stödja datadrivna beslut i verksamheten.

## Rollfördelning

| Huvudansvar                   | Namn            |
|-------------------------------|-----------------|
| Dataförståelse                | Henri           |
| Datastädning och förberedelse | Irene           |
| Statistiska sammanfattningar  | Kevin           |
| Visualisering                 | Anarkoli        |
| Tolkning och slutsatser       | Anarkoli, Irene |

## Valda frågeställningar

Gruppen har valt att fokusera på följande frågeställningar:

1)  **Hur skiljer sig returgrad mellan olika kategorier, regioner eller kundtyper?**
2)  **Finns det tecken på att längre leveranstid hänger ihop med fler returer?**
3)  **Hur påverkar rabatter returgraden?**

## Hur man kör projektet

### Projekts struktur

project\
├── data\
│ └── ecommerce_orders.csv\
│\
├── scripts\
│ ├── 01_data_understanding.R\
│ ├── 02_data_cleaning_and_prep.R\
│ ├── 03_stat_summary.R\
│ └── 04_visualization.R\
│\
├── report\
│ ├── interactive_report.qmd\
│ └── report.pdf\
│\
├── output\
│ └── (plots and figures)\
│\
├── run_analysis.R\
└── README.md

### Köra analysen

1.  Öppna `run_analysis.R` i RStudio
2.  Kör hela skriptet: det kör automatiskt alla filer i mappen `scripts/` i rätt ordning

### Se resultaten

-   **Interaktiv rapport:** Öppna `reports/interactive_report`, kör alla kodblock
-   **PDF-rapport:** Öppna `reports/` och läs PDF-filen direkt

### Nödvändiga paket

Projektet kräver följande R-paket:\
- `tidyvers`\
- `ggplot2`\
- `dplyr`\
- `viridis`\
- `ggrepel`
