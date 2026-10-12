# GDP Contraction Risk and Value Stock Returns across Profitability Groups

R replication materials for the paper by Junda Wu, Liyuan Luan, Jiajing Sun, and Haoyuan Feng.

The paper examines whether revisions in professional forecasts of next-year gross domestic product contraction are associated with different subsequent growth-minus-value returns among low- and high-profitability stocks. The United States evidence places much of the estimated difference within value stocks. Portfolio weights and the composition of the high-profitability benchmark matter, particularly among large stocks. European comparisons assess the portability of this pattern.

## Reproduce the analysis

All statistical analysis, data reconstruction and figure generation in this repository use **R**. Python is not required. Run commands from the repository root.

```r
install.packages(c("readxl", "jsonlite", "digest", "MASS", "data.table", "ggplot2", "ragg"))
```

```sh
Rscript run_all.R
```

The tested environment is R 4.5.1. Exact package versions are recorded in `results/session_info.txt`. The scripts use 9,999 dependent-wild-bootstrap draws for empirical inference, preserve calendar gaps between survey observations, and set the random-number generator and seeds explicitly in `inference.R`. The separate calibration experiment uses 1,000 simulated samples and 1,999 inner draws, as in the supplement.

The downloader creates `raw_data/`, saves source files, and checks SHA-256 hashes against `data/source_manifest.csv`. An existing matching file is reused. If a provider replaces an archive with a different vintage, the downloader saves it but stops instead of silently changing the study sample. A hash mismatch is not evidence of an error in the provider's data. The saved manuscript-vintage research panels and model designs remain available for reproducing the reported estimates without substituting a newer source vintage.

To reproduce the statistical results and figures directly from the included research data, without downloading raw provider archives:

```sh
Rscript 02_estimate.R
Rscript 04_country_estimates.R
Rscript 05_country_coverage.R
Rscript 06_figures.R
Rscript 07_inference_calibration.R
Rscript 08_validate.R
```

## Files and directories

The layout has one directory level. R scripts and documentation are at the root.

| File or directory | Contents |
|---|---|
| `00_download.R` | Download, save and verify the 118 source files in the manifest. |
| `01_prepare_data.R` | Reconstruct United States and European survey revisions from individual responses in R. |
| `01b_prepare_portfolios.R` | Reconstruct the main monthly-to-quarterly portfolio returns and the four constituent portfolios. |
| `02_estimate.R` | Re-estimate 251 archived model specifications, their confidence intervals and family-specific adjusted probabilities. |
| `03_country_data.R` | Rebuild country-level comparison samples from the 96 country portfolio archives. |
| `04_country_estimates.R`, `05_country_coverage.R` | Country comparisons, alternative weights, leave-one-country-out estimates and coverage sensitivity. |
| `06_figures.R` | Generate all three English manuscript figures in R. Statistical figures read the recomputed estimates rather than manually entered coefficients. |
| `07_inference_calibration.R` | Reproduce the inference calibration experiment. |
| `08_validate.R` | Compare reconstructed data and estimates with the manuscript-vintage references. |
| `inference.R`, `survey_helpers.R`, `portfolio_helpers.R` | Shared estimation, survey-bin and portfolio functions. |
| `data/` | Research panels, numeric model designs, source manifest, and attributed European Central Bank and Global Factor Data source archives. |
| `reference_results/` | Fixed numeric comparisons from the manuscript version preceding this R consolidation. These are comparison targets, not substitutes for re-estimation. |
| `results/` | Recomputed estimates, data comparisons, figure data and software information. |
| `figures/` | The three manuscript figures. |
| `raw_data/` | Local source-download cache; excluded from Git. |

## Data sources and coverage

1. **Federal Reserve Bank of Philadelphia, Survey of Professional Forecasters.** Individual probability distributions for annual real gross domestic product growth, point forecasts, corporate-profit forecasts and survey dates. The United States analysis has 102 survey origins from 1992Q2 to 2025Q4. Adjacent surveys must concern the same target calendar year. Reporting IDs may represent institutions or individuals; they are not necessarily permanent personal identifiers.
2. **European Central Bank, Survey of Professional Forecasters.** Individual growth distributions and point forecasts. The European analysis has 81 origins from 1999Q2 to 2025Q4. It forecasts euro-area output; the European stock portfolios have broader country coverage.
3. **Kenneth R. French Data Library.** Valuation, profitability and size portfolios, factors, industry returns and daily portfolios. Portfolio returns include dividends. The main outcome is the following calendar quarter. United States portfolios draw on NYSE, AMEX and NASDAQ stocks. The European regional portfolios cover 16 markets. Provider archives are downloaded to the local cache; the repository does not mirror the full French source archives.
4. **Jensen–Kelly–Pedersen Global Factor Data.** Country-level valuation and operating-profitability portfolio sorts, with value, capped-value and equal weights. These are separate characteristic sorts; they do not reconstruct a joint country-by-valuation-by-profitability portfolio. The 16 countries are Austria, Belgium, Denmark, Finland, France, Germany, Greece, Ireland, Italy, the Netherlands, Norway, Portugal, Spain, Sweden, Switzerland and the United Kingdom. The primary common sample has 63 survey origins; the coverage sensitivity excluding Ireland has 71.

Source links, filenames, download dates where recorded, byte counts and SHA-256 hashes appear in `data/source_manifest.csv`. A date-only entry records the archived snapshot date, not a claimed precise download time. Source data were obtained in October 2026; estimation is restricted to the manuscript's stated survey and return windows.

## What is reconstructed from raw sources

The R preparation scripts rebuild the matched survey probability and central-expectation revisions, their point-forecast comparisons, the main United States and European portfolio outcomes, and the country-level samples. They check these against the saved panels before proceeding.

For the wider set of historical robustness specifications, `data/model_designs.json` stores the exact dated numeric design matrix, outcome vector, variable names and specification metadata. `02_estimate.R` fits every model again from those observations. It does not treat stored coefficients as new estimates. Ancillary controls in these archived designs are preserved at their original vintage rather than downloaded and silently refreshed. Thus the statistical replication covers all 251 supplied specifications; raw-source reconstruction is explicitly scoped to the preparation scripts above.

The 251 specifications include repeated baselines and exploratory robustness checks. They are not 251 independent findings. Multiplicity adjustments are specific to each stated hypothesis family. The primary profitability comparison uses the five profitability slopes and the low-minus-high contrast, giving an adjusted probability of 0.1591. Other families can give different adjustments for the same marginal coefficient.

## Reproduction findings

The R consolidation preserves the reported results:

- Core four-portfolio slope: approximately 7.788 return percentage points per 10 probability percentage points.
- Unadjusted dependent-wild-bootstrap probability: 0.0541.
- Primary-family adjusted probability: 0.1591.
- One-standard-deviation signal change: approximately 4.24 return percentage points.

`results/replication_summary.json` and `results/replication_checks.csv` contain the comparisons. No numerical manuscript revision was required by the R conversion. These checks establish computational agreement, not causal identification or a unique economic mechanism.

## Attribution and data rights

The European Central Bank is the source of its survey records; transformations here include probability normalization, matching and revisions. See its [reuse conditions](https://www.ecb.europa.eu/services/using-our-site/disclaimer/html/index.en.html).

Global Factor Data are provided by Jensen, Kelly and Pedersen under [Creative Commons Attribution–NonCommercial 4.0](https://creativecommons.org/licenses/by-nc/4.0/), as stated on [their website](https://jkpfactors.com/). Included country archives are unmodified; quarterly research panels are transformations described by the R scripts. Cite Jensen, T. I., Kelly, B., and Pedersen, L. H. (2023), “Is There a Replication Crisis in Finance?”, *The Journal of Finance*, 78(5), 2465–2518.

Please also acknowledge the [Philadelphia Fed survey](https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/survey-of-professional-forecasters) and [Kenneth R. French Data Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html). The repository does not grant additional rights to third-party data or distribute licensed individual-stock CRSP, Compustat or Bloomberg records.
