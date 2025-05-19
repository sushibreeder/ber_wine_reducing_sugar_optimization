# Ber Wine Reducing Sugar Optimization

This project applies a 3×3×3 fixed-effects factorial ANOVA to optimize pretreatment conditions (Acid, Alkali, Enzyme), concentrations (0.5%, 1.0%, 1.5%), and incubation times (4h, 8h, 12h) for maximizing reducing sugar yield in Ber (Ziziphus mauritiana) wine production.

## Objectives

- Evaluate impact of pretreatment, concentration, and time on reducing sugar levels.
- Identify significant main effects and interaction effects.
- Use Hsu’s procedure to determine the best treatment combination.

## Experimental Design

- Full factorial: 3 (Pretreatment) × 3 (Concentration) × 3 (Time)
- Response variable: Reducing sugars (mg)
- 3 replications per combination (N = 81)
- Factors: All fixed effects

## Statistical Methods

- 3-way ANOVA with all 2-way and 3-way interactions
- Shapiro-Wilk, Brown-Forsythe tests for assumptions
- Post-hoc grouping with Hsu’s method (α-adjusted)

## Key Result

- Best treatment: Enzyme × 1.0% × 12h incubation
- Pretreatment explained the most variation
- All main and interaction effects were significant (p < 0.0001)

## Repository Contents

- `scripts/`: R script for model fitting, diagnostics, and post-hoc comparisons
- `reports/`: Final project presentation (PDF)
- `figures/`: Optional plots (interaction, residual)
- `data/`: Description of experiment structure and variable layout

## License

MIT License
