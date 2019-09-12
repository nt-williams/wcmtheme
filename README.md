# wcmtheme

Intended for fellow Biostatisticians in the Division of Biostatistics and Epidemiology at Weill Cornell Medicine, `wcmtheme` contains a custom Rmarkdown theme to be used for statistical reports. The theme maintains the regular workings of an rmarkdown document (i.e., a floating TOC and tabsets) but contains additional parameters often used in statistical reports. 

The theme also takes inspiration from the "better poster" paradigm and encourages the use of *take-home* message written for the investigator in a stylized blockquote. Further theme improvements include: 

- Weill Cornell color palette and branding
- A parameterized footer
- More professional looking tables
- Automatic document dating

`wcmtheme` is heavily inspired by the `hrbrthemes` and `wesanderson` packages, kudos to those creators. 

## Installation

`wcmtheme` can be installed from github with `devtools::install_github("nt-williams/wcmtheme")`. 

Once installed, the theme can be selected from Rstudio using File > New File > R Markdown... > From Template > wcm.

![](https://i.imgur.com/zsnFpHZ.png)
