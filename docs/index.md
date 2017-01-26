---
title: "Trumpworld Network Analysis"
date: "2017-01-25 19:39:04"
author: Benjamin Chan (https://github.com/benjamin-chan/trumpworld)
output:
  html_document:
    toc: true
    theme: simplex
---

---

# Preamble

Set working directory.


```r
setwd("~/Projects/trumpworld/scripts")
```

Load libraries.


```r
library(igraph)
library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
library(networkD3)
```

Reproducibility steps.


```r
sessionInfo()
```

```
## R version 3.3.2 (2016-10-31)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows >= 8 x64 (build 9200)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] rmarkdown_1.3     networkD3_0.2.13  knitr_1.15.1      ggplot2_2.2.1    
## [5] readr_1.0.0       dplyr_0.5.0       magrittr_1.5      igraph_1.0.1     
## [9] checkpoint_0.3.18
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.8      munsell_0.4.3    colorspace_1.3-2 R6_2.2.0        
##  [5] stringr_1.1.0    highr_0.6        plyr_1.8.4       tools_3.3.2     
##  [9] grid_3.3.2       gtable_0.2.0     DBI_0.5-1        htmltools_0.3.5 
## [13] yaml_2.1.14      lazyeval_0.2.0   assertthat_0.1   digest_0.6.10   
## [17] rprojroot_1.1    tibble_1.2       htmlwidgets_0.8  evaluate_0.10   
## [21] stringi_1.1.2    backports_1.0.4  scales_0.4.1     jsonlite_1.2
```

```r
set.seed(as.integer(as.Date("2017-01-20")))
```

Source user-defined functions.


```r
sapply(list.files("../lib", full.names = TRUE), source)
```

---

# Read data

See http://r.prevos.net/trumpworld-analysis/

Read the `.graphml` file.


```r
f <- "../data/trumpworld.graphml"
G <- f %>% read_graph(format = "graphml")
df <- igraph::as_data_frame(G, what = "both")
df$vertices <-
  df$vertices %>% 
  mutate(node = 1:nrow(.))
names(df)
str(df)
```

---

# Centrality

What entities are the top 5% connected nodes (in terms of 
[*degree*](https://en.wikipedia.org/wiki/Centrality#Degree_centrality))?


```r
threshold <- 0.05
degree <- G %>% degree
lim <- degree %>% quantile(probs = 1 - threshold)
top <-
  data.frame(node = V(G)$id, degree = degree) %>% 
  filter(degree >= lim) %>% 
  arrange(-degree)
top %>% kable
```



|node                                                | degree|
|:---------------------------------------------------|------:|
|DONALD J. TRUMP                                     |    618|
|WILBUR ROSS                                         |    129|
|STEVEN MNUCHIN                                      |    116|
|THRIVE CAPITAL                                      |     86|
|BETSY DEVOS                                         |     48|
|40 WALL STREET LLC                                  |     41|
|ELAINE CHAO                                         |     38|
|DJT HOLDINGS LLC                                    |     36|
|KUSHNER COMPANIES                                   |     32|
|TRUMP HOTELS & CASINO RESORTS, INC.                 |     29|
|JARED KUSHNER                                       |     28|
|TRUMP ORGANIZATION LLC                              |     27|
|JOSHUA KUSHNER                                      |     24|
|DONALD TRUMP JR.                                    |     24|
|REX TILLERSON                                       |     22|
|TRUMP TOWER COMMERCIAL LLC                          |     22|
|THE TRUMP ORGANIZATION, INC.                        |     22|
|PAUL MANAFORT                                       |     18|
|THE ERIC TRUMP FOUNDATION                           |     17|
|KELLYANNE CONWAY                                    |     17|
|BEN CARSON                                          |     15|
|STEPHEN BANNON                                      |     14|
|OMAROSÉ ONEE MANIGAULT                             |     13|
|TOM PRICE                                           |     12|
|THOMAS J. BARRACK JR.                               |     12|
|GARY COHN                                           |     11|
|IVANKA TRUMP                                        |     11|
|GOLDMAN SACHS                                       |     10|
|MICK MULVANEY                                       |      9|
|TAG AIR, INC.                                       |      9|
|ANDY PUZDER                                         |      9|
|COLONY NORTHSTAR, INC.                              |      9|
|CHARLES KUSHNER                                     |      8|
|TTTT VENTURE LLC                                    |      8|
|NIKKI HALEY                                         |      8|
|MELANIA TRUMP                                       |      8|
|BAYROCK GROUP                                       |      8|
|MNC GROUP                                           |      8|
|TRUMP ENTERTAINMENT RESORTS, INC.                   |      7|
|MICHAEL POMPEO                                      |      7|
|BLACK, MANAFORT, STONE AND KELLY                    |      7|
|DAN COATS                                           |      7|
|DON MCGAHN                                          |      7|
|TOM BOSSERT                                         |      7|
|LINDA MCMAHON                                       |      7|
|RICK PERRY                                          |      7|
|ANDREW BREMBERG                                     |      7|
|CADRE                                               |      7|
|RYAN ZINKE                                          |      7|
|HEALTHCARE EQUITY PARTNERS LP                       |      6|
|THE TRUMP ENTREPRENEUR INITIATIVE LLC (NY DOMESTIC) |      6|
|ERIC TRUMP                                          |      6|
|BAYROCK/SAPIR ORGANIZATION LLC                      |      6|
|VIKTOR KHRAPUNOV                                    |      6|
|TRUMP PRODUCTIONS LLC                               |      6|
|DEUTSCHE BANK                                       |      6|
|MARK BURNETT                                        |      6|
|REINCE PRIEBUS                                      |      6|
|TITAN ATLAS MANUFACTURING                           |      6|
|401 MEZZ VENTURE LLC                                |      6|
|401 NORTH WABASH VENTURE LLC                        |      6|
|CARLISLE MANAGEMENT                                 |      6|
|TRUMP RUFFIN LLC                                    |      5|
|DT MARKS PUNE LLC                                   |      5|
|TRUMP PARK AVENUE LLC                               |      5|
|SYLVANUS PARTNERS LLC                               |      5|
|TRUMP SOHO MEMBER LLC                               |      5|
|FELIPE YARYURA                                      |      5|
|JAMES MATTIS                                        |      5|
|SCOTT PRUITT                                        |      5|
|KATHLEEN TROIA MCFARLAND                            |      5|
|THE DONALD J. TRUMP FOUNDATION                      |      5|
|MICHAEL FLYNN                                       |      5|
|TRUMP INTERNATIONAL HOTELS MANAGEMENT LLC           |      5|
|DRINKS AMERICAS                                     |      4|
|MADISON AVENUE DIAMONDS                             |      4|
|TRUMP MARKS PUNTA DEL ESTE LLC                      |      4|
|SJ PETRO PUMP INVESTMENT LLC                        |      4|
|TRUMP VINEYARD ESTATES LLC                          |      4|
|COUNCIL FOR NATIONAL POLICY                         |      4|
|MEHMET ALI YALCINDAG                                |      4|
|DOGAN GROUP                                         |      4|
|RICHARD MARVIN "DICK" DEVOS JR.                     |      4|
|RHONA GRAFF RICCIO                                  |      4|
|HUDSON WATERFRONT ASSOCIATES IV, L.P.               |      4|
|TRUMP BRIARCLIFF MANOR DEVELOPMENT LLC              |      4|
|MIKE PENCE                                          |      4|
|MARTIN SORRELL                                      |      4|
|HUDSON WATERFRONT ASSOCIATES V, L.P.                |      4|
|GOLF RECREATION SCOTLAND LIMITED                    |      4|
|HUDSON WATERFRONT ASSOCIATES I, L.P.                |      4|
|TW VENTURE II LLC                                   |      4|
|TRUMP NATIONAL DORAL                                |      4|
|OPO HOTEL MANAGER LLC                               |      4|
|RUSSIAN-AMERICAN CHAMBER OF COMMERCE IN THE USA     |      4|
|TRUMP NATIONAL GOLF CLUB WASHINGTON DC LLC          |      4|
|SERGEI MILLIAN                                      |      4|
|THE TRUMP-EQUITABLE FIFTH AVENUE COMPANY            |      4|
|SEAN SPICER                                         |      4|
|HUDSON WATERFRONT ASSOCIATES III, L.P.              |      4|
|TRUMP MARKS HOLDINGS, L.P.                          |      4|
|1290 AVENUE OF THE AMERICAS, A TENANCY-IN-COMMON    |      4|
|TURNBERRY SCOTLAND LLC                              |      4|
|THC VENTURE III LLC                                 |      4|
|OSCAR INSURANCE CORPORATION                         |      4|
|LFB ACQUISITION LLC                                 |      4|
|TRUMP LAS VEGAS MEMBER LLC                          |      4|
|THE DONALD J. TRUMP REVOCABLE TRUST                 |      4|
|KUSHNER PROPERTIES                                  |      4|
|TRUMP LAS VEGAS MANAGING MEMBER LLC                 |      4|

Not surprisingly,  is the most connected.

What entities are the top 5% connected nodes (in terms of 
[*closeness*](https://en.wikipedia.org/wiki/Centrality#Closeness_centrality))?

Closeness is a measure of how many steps are required to access every other node.
It's a measure of how close a node is to all the action.
A node with high closeness, however, doesn't necessarily have to have very many connections or be in between relationships.


```r
closeness <- G %>% closeness
lim <- closeness %>% quantile(probs = 1 - threshold)
top <-
  data.frame(node = V(G)$id, closeness = closeness) %>% 
  filter(closeness >= lim) %>% 
  arrange(-closeness)
top %>% kable
```



|node                                       | closeness|
|:------------------------------------------|---------:|
|DONALD J. TRUMP                            | 0.0002925|
|STEVEN MNUCHIN                             | 0.0002145|
|IVANKA TRUMP                               | 0.0002100|
|DINA POWELL                                | 0.0002064|
|STEPHEN BANNON                             | 0.0002057|
|GARY COHN                                  | 0.0002051|
|WILBUR ROSS                                | 0.0002046|
|FELIPE YARYURA                             | 0.0002033|
|THE DONALD J. TRUMP FOUNDATION             | 0.0002032|
|DONALD TRUMP JR.                           | 0.0002017|
|MEHMET ALI YALCINDAG                       | 0.0002017|
|TRUMP ORGANIZATION LLC                     | 0.0002003|
|DJT HOLDINGS LLC                           | 0.0001993|
|40 WALL STREET LLC                         | 0.0001987|
|BETSY DEVOS                                | 0.0001981|
|THE TRUMP ORGANIZATION, INC.               | 0.0001979|
|401 NORTH WABASH VENTURE LLC               | 0.0001977|
|ELAINE CHAO                                | 0.0001970|
|TRUMP HOTELS & CASINO RESORTS, INC.        | 0.0001968|
|JASON D. GREENBLATT                        | 0.0001965|
|ERIC TRUMP                                 | 0.0001965|
|PAUL MANAFORT                              | 0.0001963|
|PANCHSHIL REALTY                           | 0.0001963|
|D B PACE ACQUISITION, LLC                  | 0.0001961|
|REX TILLERSON                              | 0.0001960|
|TRUMP VINEYARD ESTATES LLC                 | 0.0001960|
|TRUMP BRIARCLIFF MANOR DEVELOPMENT LLC     | 0.0001960|
|TW VENTURE II LLC                          | 0.0001960|
|TRUMP NATIONAL GOLF CLUB WASHINGTON DC LLC | 0.0001960|
|TURNBERRY SCOTLAND LLC                     | 0.0001960|
|LFB ACQUISITION LLC                        | 0.0001960|
|TRUMP LAS VEGAS MEMBER LLC                 | 0.0001960|
|TRUMP LAS VEGAS MANAGING MEMBER LLC        | 0.0001960|
|OPO HOTEL MANAGER LLC                      | 0.0001959|
|THE DONALD J. TRUMP REVOCABLE TRUST        | 0.0001959|
|DJT HOLDINGS MANAGING MEMBER LLC           | 0.0001959|
|TRUMP OLD POST OFFICE LLC                  | 0.0001959|
|PINE HILL DEVELOPMENT LLC                  | 0.0001959|
|TRUMP CHICAGO MANAGING MEMBER LLC          | 0.0001959|
|809 NORTH CANON LLC                        | 0.0001959|
|THC VENTURE I LLC                          | 0.0001959|
|SEVEN SPRINGS LLC                          | 0.0001959|
|TIHT CHICAGO MEMBER ACQUISITION LLC        | 0.0001959|
|TRUMP WINE MARKS LLC                       | 0.0001959|
|TRUMP SCOTSBOROUGH SQUARE LLC              | 0.0001959|
|TRUMP CHICAGO MEMBER LLC                   | 0.0001959|
|DT CONNECT II LLC                          | 0.0001959|
|WHITE COURSE LLC                           | 0.0001959|
|4 SHADOW TREE LANE LLC                     | 0.0001959|
|TNGC JUPITER MANAGEMENT LLC                | 0.0001959|
|TRUMP TOWER COMMERCIAL LLC                 | 0.0001959|
|TRUMP MARKS ASIA LLC                       | 0.0001959|
|TW VENTURE I LLC                           | 0.0001959|
|TRUMP VIRGINIA LOT 5 LLC                   | 0.0001959|
|40 WALL DEVELOPMENT ASSOCIATES LLC         | 0.0001958|
|40 WALL STREET MEMBER CORP.                | 0.0001958|
|40 WALL STREET COMMERCIAL LLC              | 0.0001958|
|401 MEZZ VENTURE LLC                       | 0.0001958|
|THOMAS J. BARRACK JR.                      | 0.0001957|
|MICHAEL POMPEO                             | 0.0001957|
|TRUMP ENDEAVOR 12 MANAGER CORP.            | 0.0001957|
|TRUMP ENDEAVOR 12 LLC                      | 0.0001957|
|CHICAGO UNIT ACQUISITION LLC               | 0.0001956|
|TRUMP COMMERCIAL CHICAGO LLC               | 0.0001956|
|TRUMP PAYROLL CHICAGO LLC                  | 0.0001956|
|KELLYANNE CONWAY                           | 0.0001955|
|RHONA GRAFF RICCIO                         | 0.0001954|
|DAN COATS                                  | 0.0001953|
|BEN CARSON                                 | 0.0001953|
|OMAROSÉ ONEE MANIGAULT                    | 0.0001953|
|TRUMP SOHO MEMBER LLC                      | 0.0001952|
|THE TRUMP-EQUITABLE FIFTH AVENUE COMPANY   | 0.0001952|
|BRIAN BAUDREAU                             | 0.0001951|
|TOM PRICE                                  | 0.0001950|
|TRUMP TOWER MANAGING MEMBER, INC.          | 0.0001950|
|TRUMP PRODUCTIONS LLC                      | 0.0001949|
|SERGEI MILLIAN                             | 0.0001949|
|MICK MULVANEY                              | 0.0001948|
|ANDY PUZDER                                | 0.0001948|
|TAG AIR, INC.                              | 0.0001948|
|DON MCGAHN                                 | 0.0001947|
|TOM BOSSERT                                | 0.0001947|
|NIKKI HALEY                                | 0.0001947|
|ANDREW BREMBERG                            | 0.0001947|
|MELANIA TRUMP                              | 0.0001947|
|ROGER J. STONE, JR.                        | 0.0001947|
|LINDA MCMAHON                              | 0.0001947|
|RICK PERRY                                 | 0.0001947|
|RYAN ZINKE                                 | 0.0001947|

What entities are the top 5% connected nodes (in terms of 
[**betweenness**](https://en.wikipedia.org/wiki/Centrality#Betweenness_centrality))?

Betweenness is a measure of how often a node is in the pathway between two other nodes.
I.e., a node with high betweenness can be a key player in introducing a large group of nodes to another large group of nodes.
Such a node doesn't necessarily have to have a large number of connections themselves.
But they could be in a unique position of influence in the network.


```r
betweenness <- G %>% betweenness
lim <- betweenness %>% quantile(probs = 1 - threshold)
top <-
  data.frame(node = V(G)$id, betweenness = betweenness) %>% 
  filter(betweenness >= lim) %>% 
  arrange(-betweenness)
top %>% kable
```



|node                                            | betweenness|
|:-----------------------------------------------|-----------:|
|DONALD J. TRUMP                                 | 1398943.117|
|WILBUR ROSS                                     |  212875.744|
|STEVEN MNUCHIN                                  |  206921.375|
|IVANKA TRUMP                                    |  177540.412|
|JARED KUSHNER                                   |  174586.605|
|THRIVE CAPITAL                                  |  119901.080|
|GOLDMAN SACHS                                   |   91812.755|
|BETSY DEVOS                                     |   85265.333|
|TRUMP ORGANIZATION LLC                          |   82114.593|
|CADRE                                           |   67483.810|
|KUSHNER COMPANIES                               |   62228.952|
|40 WALL STREET LLC                              |   61580.025|
|ELAINE CHAO                                     |   59043.703|
|TRUMP HOTELS & CASINO RESORTS, INC.             |   56528.000|
|JOSHUA KUSHNER                                  |   45952.925|
|DONALD TRUMP JR.                                |   45720.492|
|PAUL MANAFORT                                   |   41306.440|
|STEPHEN BANNON                                  |   40682.928|
|REX TILLERSON                                   |   33498.601|
|THOMAS J. BARRACK JR.                           |   32776.929|
|MICHAEL POMPEO                                  |   32662.000|
|GARY COHN                                       |   31392.743|
|THE TRUMP ORGANIZATION, INC.                    |   30639.720|
|KELLYANNE CONWAY                                |   27937.030|
|TRUMP TOWER COMMERCIAL LLC                      |   27328.989|
|BEN CARSON                                      |   24129.000|
|OMAROSÉ ONEE MANIGAULT                         |   24127.000|
|DAN COATS                                       |   24111.000|
|THE ERIC TRUMP FOUNDATION                       |   22493.358|
|TOM PRICE                                       |   18975.000|
|DEUTSCHE BANK                                   |   17744.987|
|DINA POWELL                                     |   16561.707|
|MNC GROUP                                       |   15531.000|
|TRUMP PRODUCTIONS LLC                           |   15514.500|
|ERIC TRUMP                                      |   14313.419|
|MICK MULVANEY                                   |   13812.000|
|ANDY PUZDER                                     |   13812.000|
|TRUMP SOHO MEMBER LLC                           |   13160.646|
|DJT HOLDINGS LLC                                |   12898.365|
|COLONY NORTHSTAR, INC.                          |   12630.856|
|MELANIA TRUMP                                   |   12089.000|
|NIKKI HALEY                                     |   12089.000|
|DON MCGAHN                                      |   12088.000|
|TOM BOSSERT                                     |   12088.000|
|DRINKS AMERICAS                                 |   12084.000|
|LINDA MCMAHON                                   |   10365.000|
|RICK PERRY                                      |   10365.000|
|RYAN ZINKE                                      |   10365.000|
|SUSAN POMPEO                                    |   10350.000|
|BAYROCK GROUP                                   |   10268.955|
|TITAN ATLAS MANUFACTURING                       |   10062.777|
|ANDREW BREMBERG                                 |   10012.928|
|KICKSTARTER                                     |    9845.974|
|42FLOORS                                        |    9845.974|
|HONEST BUILDINGS                                |    9845.974|
|JIBE                                            |    9845.974|
|HOT POTATO                                      |    9845.974|
|CHARLES KUSHNER                                 |    9626.008|
|TAG AIR, INC.                                   |    9120.907|
|TRUMP ENTERTAINMENT RESORTS, INC.               |    8668.000|
|HEALTHCARE EQUITY PARTNERS LP                   |    8640.000|
|CARLISLE MANAGEMENT                             |    8640.000|
|MICHAEL FLYNN                                   |    8639.000|
|DAVIS MANAFORT, INC.                            |    8634.000|
|SERGEI MILLIAN                                  |    8330.149|
|BLACK, MANAFORT, STONE AND KELLY                |    7562.541|
|REINCE PRIEBUS                                  |    7480.504|
|SYLVANUS PARTNERS LLC                           |    6914.000|
|JAMES MATTIS                                    |    6914.000|
|SCOTT PRUITT                                    |    6914.000|
|KATHLEEN TROIA MCFARLAND                        |    6914.000|
|MADISON AVENUE DIAMONDS                         |    6913.000|
|HUDSON WATERFRONT ASSOCIATES I, L.P.            |    6913.000|
|TRUMP CENTRAL PARK WEST CORP.                   |    6908.000|
|401 NORTH WABASH VENTURE LLC                    |    6903.808|
|VIKTOR KHRAPUNOV                                |    6564.550|
|HUDSON WATERFRONT ASSOCIATES IV, L.P.           |    5343.785|
|HUDSON WATERFRONT ASSOCIATES V, L.P.            |    5343.785|
|HUDSON WATERFRONT ASSOCIATES III, L.P.          |    5343.785|
|RUSSIAN-AMERICAN CHAMBER OF COMMERCE IN THE USA |    5224.923|
|SJ PETRO PUMP INVESTMENT LLC                    |    5187.000|
|MIKE PENCE                                      |    5187.000|
|MARTIN SORRELL                                  |    5187.000|
|KUSHNER PROPERTIES                              |    5187.000|
|BANNON & CO.                                    |    5186.000|
|ONE CENTRAL PARK WEST ASSOCIATES                |    5186.000|
|PERICLES EMERGING MARKET MANAGERS L.P.          |    5184.000|

Find communities using the edge betweenness algorithm.


```r
C <- edge.betweenness.community(G)
sizes(C)[order(sizes(C), decreasing=TRUE)]
```

```
## Community sizes
##   1  16   3   8  12  14   4   6  13  19  11   2  15  54  10  55  17  18 
## 501 143 128 115  96  65  51  50  45  45  43  37  37  21  20  17  15  15 
##  20  62  53  23  49  21  44   7  24  28  38  75   5  34  56  70  64  25 
##  15  13  12  10  10   9   9   8   8   8   8   8   7   7   7   7   6   5 
##  29  31  45  47  57  66  72  22  30  42  51  63  67   9  32  35  40  41 
##   5   5   5   5   5   5   5   4   4   4   4   4   4   3   3   3   3   3 
##  58  60  61  68  69  71  73  74  77  26  27  33  36  37  39  43  46  48 
##   3   3   3   3   3   3   3   3   3   2   2   2   2   2   2   2   2   2 
##  50  52  59  65  76  78 
##   2   2   2   2   2   2
```

Wow.
That's one big community there with 501 nodes.
Let's call this community the **primary community**.

---

# Visualize

Produce a zoomable, interactive network plots of

* The **entire** network
  * [HTML](bignet.html)
* The **primary community** network
  * [HTML](primenet.html)


```r
edges <-
  G %>% 
  as_edgelist %>% 
  data.frame %>% 
  rename(source = X1,
         target = X2) %>% 
  mutate(source = source - 1,
         target = target - 1,
         value = 1)
vertices <- 
  G %>% 
  vertex_attr("id") %>% 
  data.frame %>% 
  mutate(group = 1)
names(vertices) <- c("id", "group")
N <- forceNetwork(Links = edges, 
                  Nodes = vertices,
                  Source = "source", 
                  Target = "target", 
                  # Value = "value",
                  NodeID = "id", 
                  Group = "group",
                  opacity = 1/2,
                  fontSize = 16, fontFamily = "sans-serif", zoom = TRUE)
saveNetwork(N, "../docs/bignet.html")
```


```r
communityID <- names(sizes(C))[sizes(C) == max(sizes(C))]
nodesInPrimeNet <- (1:nrow(df$vertices))[C$membership == communityID]
G1 <-
  G %>% 
  induced_subgraph(df$vertices$node[nodesInPrimeNet])
edges <-
  G1 %>% 
  as_edgelist %>% 
  data.frame %>% 
  rename(source = X1,
         target = X2) %>% 
  mutate(source = source - 1,
         target = target - 1,
         value = 1)
vertices <- 
  G1 %>% 
  vertex_attr("id") %>% 
  data.frame %>% 
  mutate(group = 1)
names(vertices) <- c("id", "group")
N <- forceNetwork(Links = edges, 
                  Nodes = vertices,
                  Source = "source", 
                  Target = "target", 
                  # Value = "value",
                  NodeID = "id", 
                  Group = "group",
                  opacity = 1/2,
                  fontSize = 16, fontFamily = "sans-serif", zoom = TRUE)
saveNetwork(N, "../docs/primenet.html")
```
