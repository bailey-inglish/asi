# ASI Data Projects
If you're interested in the final products behind our work on voter representation and civic engagement, check out [our website]([url](https://straussinstitute.moody.utexas.edu/tcvr-landing-page))! This repository contains the code that works behind the scenes to pull this off.

## Setup
Because GitHub doesn't like it when you try and upload large files, the `.gitignore` file includes a variety of different directories throughout the project that tend to make Git angry. That being said, it also means that most of the datasets I use are not synced to this repo. For each project, the configuration/dependency instructions are below.

### cps_cleaning
Create a folder named `raw_data` in the directory and then add a second folder named `final_data`. IPUMS restricts redistribution, so you will need to register for a free account with them and then submit a request for a CPS extract with the following samples and variables:

![image](https://github.com/user-attachments/assets/f220673b-a1e2-4a02-8712-f3d629d4e738)

Download the XML codebook and the .dat file and save both to `raw_data` so that your directory looks like this:

![image](https://github.com/user-attachments/assets/9f18d8e7-3b7b-4808-ba0d-a56c7c9d62f0)

If the numbers at the end of the data files are different, change to match the image above so that the R scripts can read them in.

Additionally, download the datsets from [this Box folder]([url](https://utexas.box.com/s/411ibdctl8txf0t8v27t0q09yt8rakeu)) and put them in their respective directories.

_Sources: IPUMS, UF Election Lab_

**The ASI logo is included as a download for convenience, however, it is not to be reproduced without permission.**

### disparities
Create a folder named `raw_data` in the directory and then add a second folder named `final_data`. Download the CES dataset from [the Harvard Dataverse]([url](https://dataverse.harvard.edu/file.xhtml?fileId=10275515&version=9.0)) and put it in `raw_data`. IPUMS restricts redistribution, so you will need to register for a free account with them and then submit a request for a CPS extract with the following samples and variables:

![image](https://github.com/user-attachments/assets/ae38be13-167e-40b0-9893-c5232be52704)

If the numbers at the end of the data files are different, change to match the image above so that the R scripts can read them in.

Additionally, download the datsets from [this Box folder]([url](https://utexas.box.com/s/3m1m85c23ggz29ktri0rakg7ypypi3do)) and put them in their respective directories.

**Note:** `selected_plots.r` is dependent on `asi\cps_cleaning\cps_ipums_tidy.r` having been run to produce `cps_cleaning/final_data/cps_clean_ipums_2008-2022.csv`.

_Sources: IPUMS, Cost of Voting Index, NCSL, UF Election Lab, CES_

### midnight_sun
_Note that this project is unfinished and not stable for release._ The cast vote record (CVR) for Alaska is available in a massive ZIP file [here]([url](https://www.elections.alaska.gov/results/24GENR/CVR_Export_20241130154411.zip)). Unzip it (you may need to use 7zip or a similar tool) into a directory called `cvr` in the respective year. Move the manifest files over to a separate directory aptly named `manifests`. Then create a third empty folder named `products`. **This script takes forever to run and may time out on lower end machines. You've been warned.**

![image](https://github.com/user-attachments/assets/4f6f1386-efda-4fb4-b240-0bda2c5a1d8b)

### precincts
All of the datasets for this project are small enough to live on GitHub. This project should be ready to use out of the box.

### sdr
This Shiny app is available for archival purposes only and not actively updated. You can view the live version [here]([url](https://bailey-inglish.shinyapps.io/sdr-explorer/)). The only files that are hidden from Git are `.shiny-token.csv` and `cps_00007.dat`. The former is formatted in a csv file as:

```
app, token, secret
1, ABCDEFGHIJKLMNOP, 1234567890/qrstuvwxyz
```

The later is a CPS IPUMS file with the following specifications:

![image](https://github.com/user-attachments/assets/4515665f-4f78-412e-89e5-5cfa6ebd76e0)

Put the .dat and .xml file in `\prep` as follows:

![image](https://github.com/user-attachments/assets/fd428ddf-8c02-4369-9875-77f540086655)


_Sources: IPUMS, UF Election Lab, NCSL_

### turnout
This is the first project I put in this repo and is just kind of a dumpster fire of all my first attempts at wrangling the CPS data. There's not really anything of value to be found here but I'll leave it up for posterity.

### vri_research
Create a folder named `raw_data` in the directory and then add a second folder named `final_data`. IPUMS restricts redistribution, so you will need to register for a free account with them and then submit a request for a CPS extract with the following samples and variables:

![image](https://github.com/user-attachments/assets/e715974e-0949-4324-bc7a-65b21f4f3373)

Download the XML codebook and the .dat file and save both to `raw_data` so that your directory looks like this:

![image](https://github.com/user-attachments/assets/c238d620-300d-485d-abaf-4676d32e469e)

If the numbers at the end of the data files are different, change to match the image above so that the R scripts can read them in.

Additionally, download the datsets from [this Box folder]([url](https://utexas.box.com/s/3anc5qfossbpe4ic5yrh76gui36o1hec)) and put them in their respective directories.

_Sources: IPUMS, UF Election Lab_


## Usage
This repository is primarily intended for ASI researchers and their collaborators, but we make this available to the general public for convenience and scientific research. Please cite/attribute us if you use any of our work. The author of all of the R scripts is Bailey Inglish. The institutional affiliation is The Annette Strauss Institute for Civic Life, The University of Texas at Austin. And if you use any external datasets (i.e. IPUMS, UF Election Lab, etc.), please be especially sure to provide appropriate citations for their work!

## Feedback
This repository is largely intended for working use by researchers at the Annette Strauss Institute for Civic Life and is made publicly available in real time as a courtesy to other researchers who may be interested in our work. If you would like to make a contribution, you are welcome to raise an issue or submit a PR. For all other questions or thoughts, please [email Bailey Inglish](mailto:bei246@my.utexas.edu).

## Copyright
All copyright remains with the original owners as applicable.
