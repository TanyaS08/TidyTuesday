#' ------------------------------------------------------------------#
#'  CANADIAN HOCKEY PLAYER BIRTH MONTHS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-01-09/readme.md)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
#devtools::install_github("coolbutuseless/ggblur")
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(extrafont)
library(ggtext)
library(ggblur)
library(mapproj) # For the polar orthographic map projection
library(ggthemes) # For theme_map()
library(ggtext)
library(ggrepel)
library(ggridges)
library(cowplot)
library(mdthemes)
library(gt)
library(sysfonts)
library(showtext)

#todays prompts
options(prompt = "\U1F3D2",
        continue = "\U1F1E8, \U1F1E6")

### >> d) Data import ----