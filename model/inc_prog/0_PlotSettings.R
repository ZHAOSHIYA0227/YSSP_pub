# This file defines the palettes and themes for AIM/PHI analysis 
# In the first feature paper and my thesis

# Shiya ZHAO, 2021/12/21

# palette: line ----
palette_linetype_revenue <- c("EPC" = 2, "Neutral" = 1)


# palette: shape ----
palette_shape_revenue <- c("Neutral" = 1, "EPC" = 7)


palette_shape_model <- c("MESSAGEix" = 4, "AIM-Hub" = 16)


# palette: color ----
palette_color_revenue <- c("Neutral" = "#5da4dc", "EPC" = "#d56e53")

palette_model <- c('MESSAGEix' = '#826b88',# #204056
                   'AIM-Hub' = '#f8b976')

# palette_model <- c('MESSAGEix' = '#3D9F3C',# #204056
                   # 'AIM-Hub' = '#367DB0')

# palette_model <- c('MESSAGEix' = '#9ED17B',# #204056
                   # 'AIM-Hub' = '#9DC1EE')#'AIM-Hub' = '#9DC7DD'
# 
# palette_model <- c('MESSAGEix' = '#367DB0',# #204056
#                    'AIM-Hub' = '#9ED17B')

# palette_model <- c('MESSAGEix' = '#4c88b1',# #204056
#                    'AIM-Hub' = '#fd99a7')

palette_Sc <- c('No-Miti' ='grey50', # '#8B0000',
                '1.5C' = '#004977', #'#CCCC33', # '#A2CD5A',
                '2C' = '#fca191'#, '#CC99FF' #, '#187CD'
                # 'NDC' = '#C77CFF')#'#009966') #'#9932CC'
)

palette_prj <- c("A" = "orchid2",
                 "B" = "olivedrab3",
                 "Phi" = "deepskyblue3")


palette_Prm <- c('Coal (without CCS)' = '#990000', #'#9999CC',
                 'Coal (with CCS)' = '#CC6666', #CCCCFF',
                 'Oil (without CCS)' = '#CC9966',
                 'Oil (with CCS)' = '#CCCC33',
                 'Gas (without CCS)' = '#FF6600',
                 'Gas (with CCS)' = '#FF9966',
                 'Hydropower' = '#66CCFF',
                 'Nuclear' = '#CC99FF',
                 'Solar' = '#FFCC33',
                 'Wind' = '#CCFF99',
                 'Geothermal' = '#CC3399',
                 "Ocean" = '#2e51a2',
                 "Secondary Energy Trade" = '#fdc086',
                 'Bioenergy (without CCS)' = '#009966',
                 'Bioenergy (with CCS)' = '#00CC99',
                 'Others' = '#CCCC99'
)


palette_Fin <- c('Coal' = '#CC6666',
                 'Liquid' = '#CC9966', # liq
                 'Gas' = '#FF6600', # gas
                 'Electricity' = '#99CCFF', 
                 'Heat' = '#FF3366', 
                 'Biomass' = '#009966'
)


palette_Iene <- c('Coal' = '#CC6666',
                 'Liquid' = '#CC9966', # liq
                 'Gas' = '#FF6600', # gas
                 'Electricity' = '#99CCFF', 
                 'Biomass' = '#009966'
)





palette_TH <- c(
                '2.15-threshold' = '#ffb15d', 
                '3.65-threshold' = '#8cc04b',
                '6.85-threshold' ='#007aa9'
) #'#9932CC'



# palette_I_abb <- c('FNB' = '#FF3366', #'', 
#                    'ATN' = '#CC66FF', #CCCCFF',
#                    'CFW' = '#CC6699', # 
#                    'HSG' = '#9999CC',
#                    'WTR' = '#CC6666',
                   # 'SLD' = '#CC6666',
                   # 'GAS' = '#FF6600',
                   # 'LQD' = '#CC9966',
                   # 'ELE' = '#99CCFF',
                   # 'BIO' = '#009966',
                   # 'OTH' = '#CCCC99',
                   # 'FHE' = '#CCFF00', 
                   # 'HLT' = '#3399FF', 
                   # 'TRN' = '#33CC33', 
                   # 'CMN' = '#CC0000', 
                   # 'REC' = '#FFCC00', 
                   # 'EDC' = '#66CCFF', 
                   # 'REH' = '#0099CC', 
                   # 'MGS' = '#999999')

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


palette_effect <- c("Income" = "#ffba49", "Expenditure" = "#30b0e0")


# palette_R_MSG <- c("AFR" = "#B15928", #forest green
#                    "CHN" = "#DC143C", # crimson
#                    "EEU" = "#FFD700", # gold
#                    "FSU" = "#1F78B4", # navy
#                    "LAM" = "#33A02C", # indian red
#                    "MEA" = "#00BFFF", #deep sky blue
#                    "NAM" = "#6A3D9A", # dark violet
#                    "PAO" = "#FB9A99", # sienna
#                    "PAS" = "#CAB2C6", #medium aqua marine
#                    "RCPA" = "#008080", #teal
#                    "SAS" = "#FF7F00", # corn flower blue
#                    "WEU" = "#C71585") #
# 
# palette_R_CGE <- c("BRA" = "#228B22", #forest green
#                    "CAN" = "#FFD700", # crimson
#                    "CHN" = "#DC143C", # gold
#                    "CIS" = "#000080", # navy
#                    "IND" = "#CD5C5C", # indian red
#                    "JPN" = "#00BFFF", #deep sky blue
#                    "TUR" = "#9400D3", # dark violet
#                    "USA" = "#A0522C", # sienna
#                    "XAF" = "#BC8F8F", #medium aqua marine
#                    "XE25" = '#8cd0e5', #teal
#                    "XER" = "#ffb15d", # corn flower blue
#                    "XLM" = "#C71585", # medium violet red
#                    "XME" = '#EFC4CE', # rosy brown
#                    "XNF" = "#f9ccab", # orange
#                    "XOC" = "#00FFFF", #aqua
#                    "XSA" = "#F08080", # light coral
#                    "XSE" = '#376888') #
  
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


# palette: alpha ----

palette_alpha_gini <- c("consistent" = .5, "SSP1" = .2, "SSP3" = .2, "SSP4" = .2, "SSP5" = .2)
palette_alpha_gini_all <- c("SSP2" = .5, "SSP1" = .2, "SSP3" = .2, "SSP4" = .2, "SSP5" = .2)


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



MyThemeLine <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    #    axis.title=element_text(size=5),
    #    axis.text.x = element_text(hjust=1,size = 10, angle = 0),
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    #    panel.grid.major=element_line(linetype="dashed",colour="grey",size=0.5),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size = 10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.1, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.1, b = 0, l = 0), "cm")),
    legend.text = element_text(size = 10),
    axis.ticks.length=unit(0.15,"cm")
  )

MyThemeLine2 <- theme_bw() +
  theme(
    panel.border=element_rect(fill=NA),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    legend.text = element_text(size = 10),
    axis.ticks.length=unit(-0.15,"cm")
  )