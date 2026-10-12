# Data Dictionary

| Variable | Meaning and unit |
|---|---|
| `quarter` | Survey origin, in year–quarter notation. |
| `matched_x` or `x` in reconstructed revision tables | Average same-ID, same-target revision in contraction probability, divided by ten. A value of one is ten probability percentage points. |
| `matched_mu` | Matched central-growth-expectation revision, in growth percentage points; the United States baseline uses a histogram-based mean and the European baseline uses point forecasts. |
| `matched_width` | Average change in within-respondent distribution standard deviation, in growth percentage points. |
| `matched_disagree` | Change in the cross-respondent sample standard deviation of distribution means among the same matched respondents. |
| `all_VW`, `all_EW` | Low-minus-high-profitability difference of growth-minus-value quarterly returns, without a size split; value and equal stock weights. |
| `small_VW`, `small_EW`, `big_VW`, `big_EW` | Corresponding United States size-conditioned contrasts. |
| `S1_contrast`, `S2_contrast` | European small- and large-stock four-portfolio contrasts. |
| `low_growth`, `low_value`, `high_growth`, `high_value` | Four constituent portfolio returns in the quarter after the survey, in percent. “Low” and “high” refer to operating profitability. |
| `be_me` in country columns | Low-book-to-market minus high-book-to-market return spread. |
| `ope_be` in country columns | Low-operating-profitability minus high-operating-profitability spread. |
| `vw`, `vw_cap`, `ew` | Value weighting, capped value weighting and equal weighting within country portfolios. |
| `ea`, `ne`, `all`, `diff` | Common country weights within the fixed euro-area group, non-euro group, all countries, or euro-minus-non-euro comparison. |
| `coef`, `se_hac` | Estimated focal slope and heteroskedasticity-and-autocorrelation-consistent standard error. |
| `p_dwb`, `p_rw_family` | Marginal dependent-wild-bootstrap probability and family-specific Romano–Wolf adjusted probability. |
| `ci95_low`, `ci95_high`, `ci95_set` | Limits and full set from inversion of the marginal 5% bootstrap test. |
| `lags`, `bw` | Calendar-quarter autocovariance lag and dependent multiplier bandwidth. |

The United States baseline sorts valuation and profitability using NYSE reference breakpoints. The size split uses the contemporaneous NYSE median market capitalization. European large stocks comprise the top 90% of regional cumulative capitalization; small stocks comprise the remaining 10%. These are different construction rules, not universal currency-value thresholds.

Quarterly constituent portfolio returns compound monthly total returns before the four-way contrast is formed. Country single-sort diagnostic returns are monthly fixed-notional long–short profit-and-loss sums; they are not compounded as if a zero-investment spread were an individual asset.

`model_designs.json` contains one object per specification: `quarter`, `columns`, `X`, `y`, and reference metadata in `row`. The first matrix column is an intercept. The original focal regressor, outcome, control names, lag and bandwidth are retained. Units of the focal coefficient follow that named regressor, so a non-probability diagnostic should not automatically be interpreted per ten probability points.
