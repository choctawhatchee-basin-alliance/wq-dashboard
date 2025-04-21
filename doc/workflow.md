
# Workflow Overview: R Script for Water Quality Data

<https://github.com/choctawhatchee-basin-alliance/wq-dashboard/blob/main/R/dat_proc.R>

```mermaid
flowchart TD
  A["1 Start: Load Libraries"] --> B["Authenticate with Google Sheets (read-only)"]
  B --> C["2 Load Station Locations Sheet"]
  C --> D[Clean & Rename Columns]
  D --> E["Convert to Spatial Data (sf)"]
  E --> F[Save as stas.RData]

  B --> G["3 Load Physical Data Sheet"]
  G --> H[Clean & Rename Columns]
  H --> I[Convert Date and Clean Values]
  I --> J[Standardize County & Waterbody Names]
  J --> K[Drop Redundant Columns]
  K --> L[Save as cbadat.RData]

  B --> M["4 Load Nutrient Data Sheet"]
  M --> N[Clean & Rename Columns]
  N --> O["Convert Units (TP, TN, Conductivity)"]
  O --> P[Parse & Flag Secchi Data]
  P --> Q[Reorder Columns]
  Q --> R[Save as lkwdat.RData]

  style A fill:#DDEEFF,stroke:#000,stroke-width:1px
  style F fill:#DFFFD6,stroke:#000,stroke-width:1px
  style L fill:#DFFFD6,stroke:#000,stroke-width:1px
  style R fill:#DFFFD6,stroke:#000,stroke-width:1px
```

---

## 🔧 1. Setup
- Load required libraries:  
  `tidyverse`, `googlesheets4`, `googledrive`, `sf`, `here`, `janitor`
- Authenticate Google Sheets access (read-only scope)
- Reference to shared Google Drive folder:  
  https://drive.google.com/drive/u/1/folders/1x51X6p60KOKpC3UEStIkuAWRhOH-8FHS

---

## 📍 2. Load and Process Station Location Data

### **Data Source**
- Google Sheet ID: `13ob5pYoKnYMTMn-jqKFFT6e0QyrDPXmBK9QtcB0gnrw`

### **Steps**
- Read raw data from Google Sheet
- Swap Latitude and Longitude columns (they were mislabeled)
- Rename relevant columns (e.g., `CBA Waterbody Name` → `waterbody`)
- Convert to spatial data with WGS84 CRS
- Save processed station data to: `data/stas.RData`

---

## 💧 3. Load and Process CBA Data (Discrete Physical)

#### **Data Source**
- Google Sheet ID: `16_B7XLMDDgL-4RDz4UaFE4Gk569tYi2xaf1f96mAauY`

#### **Steps**
- Read raw data
- Clean column names with `janitor::clean_names()`
- Rename key columns for clarity (e.g., `temperature_surface_f` → `temp_surf_f`)
- Convert date strings to `Date` objects
- Standardize names in `county` and `waterbody` columns using `case_when()` and `gsub()`
- Drop unneeded date/time columns
- Save processed data as `cbadat` to: `data/cbadat.RData`

---

## 🧪 4. Load and Process LakeWatch Data (Nutrients)

### **Data Source**
- Google Sheet ID: `1h4yvi9AnISVFbH_AvBw7wDx7s5-4VIOdqD-VToExmvg`

### **Steps**
- Read raw data
- Clean column names
- Rename columns for clarity (e.g., `tp_mg_l` → `tp_mgl`)
- Convert total phosphorus (TP) and total nitrogen (TN) units from µg/L to mg/L
- Harmonize conductivity values (µS/cm → mS/cm)
- Parse and extract `secchi_ft` values from notes where necessary
- Flag if Secchi depth hit bottom or weeds
- Reorder columns for readability
- Save processed data as `lkwdat` to: `data/lkwdat.RData`

---

## ❓ 5.  Questions

- Station location for Lakewatch data
- Waterbody and station as unique identifier
