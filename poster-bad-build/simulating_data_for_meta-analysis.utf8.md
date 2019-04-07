---
# PLEASE SEE THE README for in depth description github.com/brentthorne/posterdown
#
#---POSTER SIZE & DEFAULT FONT---#
poster_height: "38in" # height in inches of poster
poster_width: "45in" # width in inches of poster
font_family: "palatino" # choose from typical latex fonts (example: "palatino")
font_size: "30pt" #please see github.com/brentthorne/posterdown for compatible options.

#---TITLE BOX OPTIONS---#
#ESSENTIALS
title: 'escaping batpigday'
author: "Charles T. Gray^1^"
affiliation: "^1^Department of Mathematics and Statistics, La Trobe University, Victoria, Australia"
#STYLE & FORMATTING
titlebox_bgcol: "008080"  #Colour of the Title Box background
titlebox_bordercol: "0b4545" #Colour of the title Box border.
titlebox_shape: "all"
titlebox_borderwidth: "1cm"
title_textcol: "ffffff" #colour of title text
author_textcol: "0b4545" # Colour of author text
affiliation_textcol: "FFFFFF" # Colour of affiliation text
title_textsize: "Huge"         # Poster title fontsize
author_textsize: "Large"       # Author list font size
affiliation_textsize: "large"  # Affiliation font size
#ADDING LOGOS
logoleft_name: 'Figures/posterdownlogo'
logoleft_width: '3in'
logoleft_xshift: '1in'
logoleft_yshift: '1in'
logoright_name: 'Figures/posterdownlogo'
logoright_width: '3in'
logoright_xshift: '-1in'
logoright_yshift: '1in'

#---POSTER BODY OPTIONS---#
body_bgcol: "ffffff" #colour of the poster main background
body_textsize: "normalsize"    # Size of the main poster body text
body_textcol: "000000" # Colour of main text in the body of poster
column_numbers: 3 # Number of columns that the poster has
column_margins: "0.5in" # Margin spacing for columns
columnline_col: "008080" #colour 
columnline_width: "0pt" #width of line between each column
#SECTION TITLE STYLING
sectitle_textcol: "ffffff" # Colour of the poster section titles
sectitle_bgcol: "0b4545" # Colour of the section title box
sectitle_bordercol: "0b4545" # Colour of the border around the section title box.
sectitle_borderwidth: "2mm" # Thicknes of the section title box border
sectitle_boxshape: "uphill" # Changes the shape of the section title box.

#---BIBLIOGRAPHY OPTIONS---#
bibliography: MyLibrary # name of the .bib file used for referencing
bibliography_spacing: 0.8 # sets the multiplier for line spacing of bibliography spacing (between 0 and 1)
bibliography_textsize: "small"  # size of the bibliography text size (handy for one too many references!)

#---OTHER---#
cite_col: "CC0000" #colour of citation elements
url_col: "008080" #colour of url links
link_col: "008080" #colour of other links within the poster
footnote_textcol: "ffffff" # Colour of footnote text if used
output: posterdown::posterdown_latex
---



# batpigday

\begin{description}
\item[batpigday] \emph{noun} coding equivalent of groundhogday
\end{description}

# the problem

Simulating data is a bitch. 

Why?





\vfill\null
\columnbreak

# the package

I wanted the package to be as modular as possible, so I could swap out components and use them in future simulations.


```r
# load package
library(metasim)

# generate a meta-analysis sample set
sim_n(k = 3) %>% knitr::kable()
```



study     group             n
--------  -------------  ----
study_1   control         123
study_2   control         208
study_3   control         160
study_1   intervention    143
study_2   intervention    202
study_3   intervention     92




\vfill\null
\columnbreak

# the maths


# references



\vfill\null

