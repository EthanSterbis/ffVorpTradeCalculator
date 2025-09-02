# Sterb’s Fantasy Football VORP Trade Calculator

**Live app:** https://ethansterbis.shinyapps.io/ffVorpTradeCalculator/

An interactive **Shiny** app that evaluates fantasy football trades using **Value Over Replacement Player (VORP)**. Adjust league size, roster/bench composition, and scoring; choose seasons and week windows; and compare trades with a balance meter based on aggregated VORP.

---

## Features

- **Trade Calculator** — search players, build two packages, and see a live **VORP meter** and totals.  
- **Leaderboard** — top players by **AVG VORP** (or **Expected VORP**) over a custom week span.  
- **Replacement** — positional replacement values implied by your league settings.  
- **Player Weeks** — the week-level inputs behind the VORP calculations.  
- **League & Scoring controls** — league size, QB/RB/WR/TE/FLEX/SuperFLEX/Bench, PPR, pass TD points, TE PPR bonus.  
- **Dark UI** designed for readability; tables are center-aligned.

---

## How it works

- **Data**: pulled via `{nflreadr}` (nflverse) from the “opportunity” dataset for **2021–2024, Weeks 1–18**.  
- **Fantasy points**: computed from opportunity stats using your scoring. Both **actual** and **expected** totals are produced.  
- **Replacement**: per **position × week**, a replacement FP is estimated from your league size, roster slots, and bench.  
  Default replacement ranks (you can override them in the UI):

  - QB: `round(L*((Startable + BEN*(Startable/TotalStart)) - 0.25)) + 1`  
  - RB: `round(L*((Startable + BEN*(Startable/TotalStart)) - 2.00)) + 1`  
  - WR: `floor(L*((Startable + BEN*(Startable/TotalStart)) - 0.50)) + 1`  
  - TE: `round(L*((Startable + BEN*(Startable/TotalStart)) - 0.50)) + 1`

  *TE “startable” counts TE-only spots; FLEX/SuperFLEX are handled by the formula.*

- **Weekly VORP**: `VORP_weekly = player_week_FP – position_week_replacement_FP`  
- **Averaging**: VORP is averaged across your selected week window (respecting **Minimum Weeks Played**).

---

## Requirements

- **R ≥ 4.2** (tested on 4.4)  
- Packages: `shiny`, `dplyr`, `nflreadr`, `DT`, `rlang`, `bslib`