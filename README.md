# Digital_twin

<b>Section 1. System requirements</b>

All software dependencies and operating systems (including version numbers): Users must install R and RStudio in order to run the codes provided in the GitHub and Zenodo repository. R and RStudio run on both Windows and MacOS. R must be installed, then RStudio can be installed. See Section 2 (Installation Guide) for installing R and RStudio.

Versions the software has been tested on: R version 4.3.1.

Any required non-standard hardware: None 

<b>Section 2. Installation guide</b>

Instructions: See here. R must be installed, then RStudio can be installed. All R packages can be installed in RStudio. See here for installing packages. No installation is needed for the agricultural DT demo.

Typical install time on a “normal” desktop computer: Installing R and RStudio should take less than 5 minutes each. (It may depend on internet speed.) Installing a package in RStudio usually takes less than one minute (or a few seconds) depending on size and dependencies.

<b>Section 3. Demo</b>

Instructions to run on data: Agricultural DT demo can be used as follows.

(1)	Use the first dropdown menu to select an orchard.
(2)	Click on the Submit button below the dropdown menu. The second dropdown menu will be updated (trees in the selected orchard).
(3)	Use the second dropdown menu to select a tree.
(4)	Click on the Submit button below the dropdown menu. After a few seconds (less than 3 seconds), results will show in the panels on the right side.

Expected output: When an orchard is selected, the most recent information is presented in the following tabs:

(1)	Map: This tab shows the map of Jeju Island and the location of an orchard selected
(2)	Soil: This tab shows the distribution of observed soil chemical property values (available phosphorus, exchangeable potassium, calcium, and magnesium, acidity, organic matter, and electrical conductivity) in Jeju Island with the percentile (at the selected orchard) and the RDA recommendation (shaded in gray).
(3)	Weather: This tab shows the distribution of observed temperature, humidity, and air pressure in orchards in Jeju Island with the percentile (at the selected orchard).
(4)	Agricultural Practice: This tab shows the frequency of agricultural practices (fertilization, mulching, spraying, pruning, and thinning) at the selected orchard and compares with orchards in Jeju Island.
(5)	Sugar Content Distribution: This tab shows the inter-orchard and intra-orchard distributions of sugar content (°Bx). It is based on the recent week in the data. The inter-orchard distribution represents mandarins by orchards in the week, and the intra-orchard distribution represents mandarins by trees in the selected orchard in the week.
(6)	Fruit Size Distribution: This tab shows the inter-orchard and intra-orchard distributions of fruit size (mm). It is based on the recent week in the data. The inter-orchard distribution represents mandarins by orchards in the week, and the intra-orchard distribution represents mandarins by trees in the selected orchard in the week.
(7)	Sugar Content History: This tab shows longitudinal patterns of the weekly average sugar content (°Bx). The orchard-level figure shows the selected orchard and all other orchards in Jeju Island. The tree-level figure shows the selected tree and all other trees in the selected orchard.
(8)	Fruit Size History: This tab shows longitudinal patterns of the weekly average fruit size (mm) at orchard-level and tree-level in the selected orchard. The orchard-level figure shows the selected orchard and all other orchards in Jeju Island. The tree-level figure shows the selected tree and all other trees in the selected orchard.

Expected run time for demo on a “normal” desktop computer: A few seconds (less than 3 seconds)

<b>Section 4. Instructions for use</b>

How to run the software on your data: See Section 3 (Demo).

Reproduction instructions: For reproducing figures and results presented in the manuscript, all source codes are provided in the GitHub and Zenodo repository. Make sure RStudio and relevant packages are installed before attempt.
