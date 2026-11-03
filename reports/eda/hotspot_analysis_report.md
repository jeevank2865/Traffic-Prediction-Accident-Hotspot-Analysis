# UrbanPulse: Spatial Accident Hotspot & Corridor Analysis

## 1. Hotspot Identification Methodology

- **Geographical Unit**: Bengaluru Urban Traffic Police Station Jurisdictions (administrative point features from official Karnataka GIS).
- **Ranking Criterion**: Cumulative recorded road crashes and fatal incidents in 2024.
- **Pareto Concentration**: Top 15 stations account for over 80% of recorded city crashes.

## 2. Top 10 Accident Hotspot Stations (2024)

| Rank | Station | Zone | Total Crashes | Fatal Crashes | City Share | Cumulative Share |
|---|---|---|---|---|---|---|
| 1 | Yalahanka | North | 242 | 56 | 5.04% | 5.04% |
| 2 | K R Puram | East | 241 | 34 | 5.02% | 10.06% |
| 3 | Int. Aiport | North | 240 | 60 | 5.00% | 15.06% |
| 4 | Whitefield | East | 189 | 44 | 3.94% | 19.00% |
| 5 | Electronic City | South | 178 | 42 | 3.71% | 22.71% |
| 6 | Kengeri | West | 177 | 31 | 3.69% | 26.40% |
| 7 | Hebbala | North | 177 | 29 | 3.69% | 30.08% |
| 8 | Bytarayanapura | West | 146 | 19 | 3.04% | 33.12% |
| 9 | Kamakshipalya | West | 144 | 18 | 3.00% | 36.12% |
| 10 | Jnanabharathi | West | 141 | 22 | 2.94% | 39.06% |

## 3. Spatial Corridors & Arterial Clustering

Spatial hierarchical clustering identifies major high-risk corridors in Bengaluru:
1. **Outer Ring Road / K.R. Puram – Whitefield Corridor**: High volume tech corridor intersecting national highways with heavy mixed pedestrian and freight traffic.
2. **Tumkur Road / Peenya – Yeshwanthpur Industrial Corridor**: Heavy commercial vehicle movement and elevated flyover approach bottlenecks.
3. **Hosur Road / Electronic City – Madiwala Highway Corridor**: High-speed peripheral expressway transition zone.

## 4. Exposure Denominator Caveat

Vehicle registrations indicate city-wide motorization (rising from ~7 million to >11 million vehicles between 2017 and 2026). While city crashes per 10,000 registered vehicles declined from ~6.5 to ~3.9 due to rapid vehicle growth, station-level accident risk cannot be estimated without road-segment Vehicle Miles Traveled (VMT) or Annual Average Daily Traffic (AADT). Recorded accident counts reflect historical occurrences, not localized individual crash probability.

