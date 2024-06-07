# This file defines the palettes and themes for AIM/PHI analysis 
# In the first feature paper and my thesis

# Shiya ZHAO, 2021/12/21

# palette: line ----
palette_linetype_revenue <- c("Progressive" = 2, "Neutral" = 1)


# palette: shape ----
palette_shape_revenue <- c("Neutral" = 1, "Progressive" = 7)


# palette: color ----
palette_color_revenue <- c("Neutral" = "#376888", "Progressive" = "#de786a")

palette_Sc <- c('No-Miti' ='grey50', # '#8B0000',
                '1.5C' = '#004977', #'#CCCC33', # '#A2CD5A',
                '2C' = '#fca191'#, '#CC99FF' #, '#187CD'
                # 'NDC' = '#C77CFF')#'#009966') #'#9932CC'
)


palette_TH <- c(
                '1.9-threshold' = '#ffb15d', 
                '3.2-threshold' = '#8cc04b',
                '5.5-threshold' ='#007aa9'
) 

palette_COM <- c('Food&Beverages' = '#8cd0e5', #'', 
                 'AlcoholicBeverages&Tobacco' = '#376888', #CCCCFF',
                 'Clothes&Footwear' ="#f9ccab",  # 
                 'Housing&Water' = '#de786a',
                 'Energy' = '#FFCC00',
                 'Furnishing' = '#826b88',
                 'Health' = '#007D62',
                 'Transport' = '#D5DBC5', 
                 'Communication' = "#ffb15d",  
                 'Recreation' = '#EFC4CE', 
                 'Education' = '#4d695d', 
                 'Restaurant&Hotel' = '#30b0e0', 
                 'Miscellaneous' = '#999999')

palette_TH <- c('1.9-threshold' = '#990000',
                '3.2-threshold' = '#CCCC33',
                '5.5-threshold' = '#CC99FF',
                'Relative poverty' ='#009966' )




palette_model <- c('MESSAGEix' = '#4c88b1',# #204056
                'AIM-Hub' = '#f59e7d')


palette_R_CGE <- c("Brazil" = "#228B22", #forest green
                   "Canada" = "#CD5C5C", # crimson
                   "China" = "#DC143C", # gold
                   "Former Soviet Union" = "#000080", # navy
                   "India" = "#FFD700", # indian red
                   "Japan" = "#00BFFF", #deep sky blue
                   "Turkey" = "#9400D3", # dark violet
                   "USA" = "#A0522C", # sienna
                   "Sub-Saharan Africa" = "#BC8F8F", #medium aqua marine
                   "EU" = '#8cd0e5', #teal
                   "Rest of Europe" = "#F08080", # corn flower blue
                   "Rest of South America" = "#C71585", # medium violet red
                   "Middle East" = '#EFC4CE', # rosy brown
                   "North Africa" = "#f9ccab", # orange
                   "Oceania" = "#00FFFF", #aqua
                   "Rest of Asia" = "#ffb15d", # light coral
                   "Southeast Asia" = '#376888') #


# themes ----

MyTheme <- theme(#text = element_text(size = 8), family="serif"
      axis.text.x = element_text(vjust = 0.5,
                                 hjust = 0.5,
                                 angle = 0,
                                 size = 9),
      axis.text.y = element_text(vjust = 0.5,
                                 hjust = 0.5,
                                 angle = 0,
                                 size = 9),
      axis.line = element_line(colour = "grey50"),
      panel.background =element_rect(fill = "white")
      ) + ggthemes::theme_pander() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



