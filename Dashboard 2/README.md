# Toronto Mayoral Dashboard

This folder is a cleaned, submission-ready copy of the Toronto mayoral dashboard project. It is organized so the app can be unzipped and run from this folder without changing any paths.

## Folder structure

```text
Toronto_Mayoral_Dashboard_Submission/
├── app.R
├── README.md
├── dashboard_report.html
├── data/
│   ├── 2023 Office of the Mayor.xlsx
│   ├── 2023-WardProfiles-2011-2021-CensusData.xlsx
│   ├── 2023-WardProfiles-GeographicAreas.xlsx
│   └── 25-ward-model-december-2018-wgs84-latitude-longitude/
├── scripts/
│   └── install_packages.R
└── www/
    ├── dashboard_screenshot.png
    └── styles.css
```

## What is required to run the app

- `app.R`
- the full `data/` folder
- the full `www/` folder

Do not separate these files. Keep the folder structure unchanged after unzipping.

## How to run the app

1. Open this folder in RStudio.
2. If needed, install packages:

```r
source("scripts/install_packages.R")
```

3. Run the app with either:

```r
shiny::runApp()
```

or open `app.R` in RStudio and click **Run App**.

## Portability notes

- The app now uses **relative paths only**.
- All required data files are stored inside the local `data/` folder.
- The app no longer depends on any personal computer path.
- You can optionally set `TORONTO_DATA_DIR` to another folder, but this is not required for submission.

## Package note

This submission does **not** include `renv.lock` because `renv` was not available in the local environment during packaging. The included `scripts/install_packages.R` script is the practical fallback for installing the required R packages on another computer.

One package, `sf`, may require system libraries on some computers. If package installation fails on another machine, install `sf` first using that computer's normal R setup instructions, then rerun `source("scripts/install_packages.R")`.

## HTML version

`dashboard_report.html` is a static HTML summary page that can be opened directly in a browser. It includes:

- a short project overview
- a dashboard screenshot
- local run instructions
- a note about included files

Because this is a Shiny app, the full interactive dashboard still needs to be run locally in R.

## Submission guidance

To submit:

1. Keep this folder exactly as it is.
2. Compress the entire `Toronto_Mayoral_Dashboard_Submission` folder into one `.zip` file.
3. Submit that zip file.

If your instructor wants both a runnable app and a browser-openable file, include this whole folder so they get both `app.R` and `dashboard_report.html`.
