# Results & Interpretation

## Model Specification

To estimate mean Willingness to Pay (WTP), we use a logit regression model following Hanemann (1984). The model estimates a bid function that explains how WTP varies with the presented price (tax increase) and respondent characteristics.

The mean WTP is derived from:

$$WTP = \frac{-(\beta_0 + \sum \bar{X}_k \beta_k)}{\beta_{bid}}$$

Where:
- $\beta_0$ = intercept coefficient
- $\beta_k$ = control variable coefficients
- $\bar{X}_k$ = mean values of control variables
- $\beta_{bid}$ = bid (price) coefficient

---

## Logit Regression — Full Sample

**Reference group:** Low income (< $50,000)

See `analysis.R` section 9 for regression table output.

---

## Marginal Effects — Full Sample

The marginal effects describe the change in probability of accepting the bid for a one-unit increase in each variable (or moving from 0 to 1 for binary variables), holding all else constant.

| Variable | Marginal Effect |
|---|---|
| Bid ($) | −1.5 pp per additional dollar |
| Income: Middle | +27.6 pp |
| Income: High | +8.8 pp |
| Lives in Oregon full-time | +20.1 pp |
| Traveled in South-Central Oregon | +19.8 pp |
| Visits rivers/lakes/parks | +32.9 pp |

> **Note:** None of these results are statistically significant, likely due to the small sample size (n = 31).

---

## Mean WTP — Full Sample

> **Estimated mean WTP: ~$38.50 per year in additional state taxes**

---

## Protest Zero Adjustment

Protest zeros are "No" responses where the respondent may have a non-zero WTP, but rejected the scenario for non-economic reasons (Carson, 2000). Including them biases the WTP estimate downward.

The following responses were classified as protest zeros:

1. "You cannot afford to pay more taxes"
2. "You do not support the program"
3. "i don't want to pay more taxes, especially when I don't know wtf this is"

### Marginal Effects — Protest Zeros Removed

| Variable | Marginal Effect |
|---|---|
| Bid ($) | −1.1 pp per additional dollar |
| Income: Middle | +20.5 pp |
| Income: High | −10.8 pp |
| Lives in Oregon full-time | +11.6 pp |
| Traveled in South-Central Oregon | −5.2 pp |
| Visits rivers/lakes/parks | +52.4 pp |

> **Note:** Results remain statistically insignificant after removing protest zeros.

---

## Mean WTP — Protest Zeros Removed

> **Estimated mean WTP: ~$48.29 per year in additional state taxes**

---

## Summary

| Model | Mean WTP |
|---|---|
| Full sample | ~$38.50/year |
| Protest zeros removed | ~$48.29/year |

The higher WTP estimate after removing protest zeros suggests that non-economic objections were pulling the estimate downward. Both estimates are limited by the small, non-random sample and should not be generalized to the broader Oregon population.
