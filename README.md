# Forced Turnover: Evaluating Pressing Effectiveness in Soccer

Analysis of pressing effectiveness in soccer using SkillCorner tracking data from the 2023 MLS season.

## Authors
- **David Almona** - Centre College ([almonadavid@gmail.com](mailto:almonadavid@gmail.com)) ([website](https://almonadavid.github.io/))
- **Natalie Rayce** - Carnegie Mellon University ([nrayce@andrew.cmu.edu](mailto:nrayce@andrew.cmu.edu))

## Overview
This project analyzes MLS matches to predict forced turnovers from pressing sequences using machine learning. We implement an oval pressure zone detection algorithm and extract features to predict whether pressing leads to a turnover within 5 seconds.

## Project Structure
```
├── Almona/
│   ├── pressing_functions.R          # Pressing detection functions
│   ├── single_game_pressing_data_preparation.R
│   ├── multi_game_pressing_data_preparation.R
│   └── pressing_modeling.R           # ML models
├── data/
│   └── skillcorner/                  # Data (not included)
│       ├── tracking/                 # JSON tracking files
│       ├── match_data/               # JSON match information
│       └── dynamic_events/           # CSV event data
├── results/                          # Output directory for analysis
├── Presentations/
│   └── Final Presentation.qmd
└── Capstone_Report.qmd
```

## Data provided by SkillCorner
- XY Tracking data (10 Hz)
- Match information JSON files
- Dynamic events CSV files

*Note: Data files not included in repository*

## Acknowledgments
Special thanks to [Daniel Wicker](https://www.linkedin.com/in/daniel-wicker/) (Charlotte FC), [Dr. Ron Yurko](https://www.linkedin.com/in/ron-yurko-stats/), [Quang Nguyen](https://www.linkedin.com/in/qntkhvn/), the CMSACamp TAs, and Carnegie Mellon University.

## Resources
[Slides](https://www.stat.cmu.edu/cmsac/sure/2025/showcase/soccer_tracking/slides.html) | [Poster](https://www.stat.cmu.edu/cmsac/sure/2025/showcase/soccer_tracking/poster.pdf) | [Report](https://www.stat.cmu.edu/cmsac/sure/2025/showcase/soccer_tracking/report.html)
