#!/usr/bin/Rscript

# Collection of hd models ----
#
# Aitor Vázquez Veloso
# 2024-09-10, adapted on 2026-06-01
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# rodriguez_de_prado_species_2022: 11 additional models ====

#' @article{rodriguez_de_prado_species_2022,
#'   title = {Species {Mixing} {Proportion} and {Aridity} {Influence} in the {Height}–{Diameter} {Relationship} for {Different} {Species} {Mixtures} in {Mediterranean} {Forests}},
#'   volume = {13},
#'   issn = {1999-4907},
#'   url = {https://www.mdpi.com/1999-4907/13/1/119},
#'   doi = {10.3390/f13010119},
#'   abstract = {Estimating tree height is essential for modelling and managing both pure and mixed forest stands. Although height–diameter (H–D) relationships have been traditionally ﬁtted for pure stands, attention must be paid when analyzing this relationship behavior in stands composed of more than one species. The present context of global change makes also necessary to analyze how this relationship is inﬂuenced by climate conditions. This study tends to cope these gaps, by ﬁtting new H–D models for 13 different Mediterranean species in mixed forest stands under different mixing proportions along an aridity gradient in Spain. Using Spanish National Forest Inventory data, a total of 14 height–diameter equations were initially ﬁtted in order to select the best base models for each pair species-mixture. Then, the best models were expanded including species proportion by area (mi) and the De Martonne Aridity Index (M). A general trend was found for coniferous species, with taller trees for the same diameter size in pure than in mixed stands, being this trend inverse for broadleaved species. Regarding aridity inﬂuence on H–D relationships, humid conditions seem to beneﬁciate tree height for almost all the analyzed species and species mixtures. These results may have a relevant importance for Mediterranean coppice stands, suggesting that introducing conifers in broadleaves forests could enhance height for coppice species. However, this practice only should be carried out in places with a low probability of drought. Models presented in our study can be used to predict height both in different pure and mixed forests at different spatio-temporal scales to take better sustainable management decisions under future climate change scenarios.},
#'   language = {en},
#'   number = {1},
#'   urldate = {2024-01-19},
#'   journal = {Forests},
#'   author = {Rodríguez De Prado, Diego and Riofrío, Jose and Aldea, Jorge and McDermott, James and Bravo, Felipe and Herrero De Aza, Celia},
#'   month = jan,
#'   year = {2022},
#'   keywords = {machine learning, adaptive silviculture, climate-smart forestry, height–diameter relationship, mixed forests performance, national forest inventory data, NLMM, programming, species mixing proportions},
#'   pages = {119}
#' }

huang_1992 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 * exp(-beta1 * exp(-beta2 * dbh))
  return(h)
}

meyer_1940 <- function(beta0, beta1, dbh) {
  h <- beta0 * (1 - exp(-beta1 * dbh))
  return(h)
}

pearl_1920 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 / (1 + beta1 * exp(-beta2 * dbh))
  return(h)
}

ratkowsky_1986 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 / (1 + beta1^(-1) * dbh^(-beta2))
  return(h)
}

# richards_1959 <- function(beta0, beta1, beta2, dbh) {
#   h <- beta0 * (1 - exp(beta1 * dbh))^beta2
#   return(h)
# }

schumacher_1939 <- function(beta0, beta1, dbh) {
  h <- 1.3 + beta0 * exp(beta1 / dbh)
  return(h)
}

seber_1989 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 * exp(-exp(-beta1 * (dbh - beta2)))
  return(h)
}

zeide_1992 <- function(beta0, beta1, beta2, beta3, dbh) {
  h <- beta0 * exp(-beta1 * exp(-beta2 * dbh^beta3))
  return(h)
}


# wagle_characterizing_2024: 23 additional models ====

#' @inproceedings{wagle_characterizing_2024,
#'   address = {Athens, GA},
#'   title = {Characterizing height-diameter relationships in the {PMRC} {Culture} x {Density} trials using a mixed-effects modeling approach for loblolly pine},
#'   author = {Wagle, Samjhana and Yang, SI and Bullock, Bronson P.},
#'   year = {2024},
#' }


huang_1992_I <- function(a, b, c, dbh) {
  h <- a * exp(-b / (dbh + c))
  return(h)
}

huang_1992_II <- function(a, b, c, dbh) {
  h <- a * (1 - exp(-b * dbh^c))
  return(h)
}

huang_1992_III <- function(a, b, c, dbh) {
  h <- a * (1 - exp(-b * dbh))^c
  return(h)
}

huang_2000_I <- function(a, b, c, dbh) {
  h <- a / (1 + b * dbh^(-c))
  return(h)
}

peschel_1938 <- function(a, b, c, dbh) {
  h <- a / (1 + 1 / (b * dbh^c))
  return(h)
}

huang_2000_II <- function(a, b, dbh) {
  h <- a * dbh * exp(-b * dbh) 
  return(h)
}

curtis_1967_I <- function(a, b, dbh) {
  h <- a * (1 - exp(-b * dbh)) 
  return(h)
}

huang_1992_IV <- function(a, b, c, dbh) {
  h <- a * dbh^(b * dbh^(-c)) 
  return(h)
}

flewelling_jong_1994 <- function(a, b, c, dbh) {
  h <- a * exp(-b * dbh^(-c)) 
  return(h)
}

huang_1992_V <- function(a, b, dbh) {
  h <- (a * dbh) / (b + dbh) 
  return(h)
}

huang_1992_VI <- function(a, b, c, dbh) {
  h <- a * exp(-b * exp(-c * dbh)) + dbh
  return(h)
}

curtis_1967_II <- function(a, b, dbh) {
  h <- (a * dbh) / (1 + b * dbh) 
  return(h)
}

stoffels_1953 <- function(a, b, dbh) {
  h <- a * dbh^b 
  return(h)
}

peschel_1938_II <- function(a, b, dbh) {
  h <- dbh^2 / ((a * dbh + b)^2)
  return(h)
}

ogana_2018 <- function(a, b, dbh) {
  h <- ((a / dbh)^b)^(-1)
  return(h)
}

wykoff_1982_I <- function(a, b, dbh) {
  h <- exp(a - b * (dbh + 1)^(-1))
  return(h)
}

larsen_hann_1987 <- function(a, b, c, dbh) {
  h <- exp(a + b * dbh^c) 
  return(h)
}

curtis_1967_III <- function(a, b, dbh) {
  h <- a * (1 + 1 / dbh)^b 
  return(h)
}

huang_2000_III <- function(a, b, dbh) {
  h <- a * (dbh / (1 + dbh))^b 
  return(h)
}

staudhammer_lemay_2000 <- function(a, b, dbh) {
  h <- exp(a + b / dbh)
  return(h)
}

burkhart_strub_1974 <- function(a, b, dbh) {
  h <- a * exp(-b * dbh^(-1)) 
  return(h)
}

huang_1992_VII <- function(a, b, c, dbh) {
  h <- a / (1 + b * exp(-c * dbh))
  return(h)
}

strand_1958 <- function(a, b, c, dbh) {
  h <- dbh^2 / (a * dbh^2 + b * dbh + c) 
  return(h)
}



# el_mamoun_modelling_2013: 22 additional models ====

#' @article{el_mamoun_modelling_2013,
#'   title = {Modelling height-diameter relationships of selected economically important natural forests species},
#'   volume = {2},
#'   url = {https://www.researchgate.net/profile/Elmamoun-Osman/publication/284581695_Modelling_height-diameter_relationships_of_selected_economically_important_natural_forests_species/links/62b844c589e4f1160c9dff23/Modelling-height-diameter-relationships-of-selected-economically-important-natural-forests-species.pdf},
#'   journal = {Journal of forest products \& industries},
#'   author = {El Mamoun, H Osman and El Zein, A Idris and El Mugira, M Ibrahim},
#'   year = {2013},
#'   pages = {34--42},
#' }

elmamoun_2013_M1 <- function(a, b, dbh) {
  h <- 1.3 + a * dbh^b
  return(h)
}

elmamoun_2013_M2 <- function(a, b, dbh) {
  h <- 1.3 + a * exp(b / dbh)
  return(h)
}

elmamoun_2013_M3 <- function(a, b, c, dbh) {
  h <- 1.3 + exp(a + (b * (dbh^c)))
  return(h)
}

elmamoun_2013_M4 <- function(a, b, c, dbh) {
  h <- 1.3 + dbh^2 / (a + b * dbh + c * (dbh^2))
  return(h)
}

elmamoun_2013_M5 <- function(a, b, c, dbh) {
  h <- 1.3 + a * dbh^(b + (c * dbh))
  return(h)
}

elmamoun_2013_M6 <- function(a, b, dbh) {
  h <- 1.3 + a * (dbh^2) / (dbh + b)^2
  return(h)
}

elmamoun_2013_M7 <- function(a, b, dbh) {
  h <- 1.3 + dbh^2 / (a + b * dbh)^2
  return(h)
}

elmamoun_2013_M8 <- function(a, b, dbh) {
  h <- 1.3 + (a + b / dbh)^(-5)
  return(h)
}

elmamoun_2013_M9 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * dbh))^c
  return(h)
}

elmamoun_2013_M10 <- function(a, b, dbh) {
  h <- 1.3 + exp(a + (b / (dbh + 1)))
  return(h)
}

elmamoun_2013_M11 <- function(a, b, c, dbh) {
  h <- 1.3 + (exp(a * exp(-b * (dbh^(-c)))))
  return(h)
}

elmamoun_2013_M12 <- function(a, b, dbh) {
  h <- 1.3 + a * dbh / (b + dbh)
  return(h)
}

elmamoun_2013_M13 <- function(a, b, dbh) {
  h <- 1.3 + dbh^2 / (a + b * dbh)^2
  return(h)
}

elmamoun_2013_M14 <- function(a, b, dbh) {
  h <- 1.3 + (a + b / dbh)^(-2.5)
  return(h)
}

elmamoun_2013_M15 <- function(a, b, dbh) {
  h <- 1.3 + (a + b / dbh)^(-8)
  return(h)
}

elmamoun_2013_M16 <- function(a, b, dbh) {
  h <- 1.3 + a * (1 + 1 / dbh)^(-b)
  return(h)
}

elmamoun_2013_M17 <- function(a, b, dbh) {
  h <- 1.3 + exp(a + b / dbh)
  return(h)
}

elmamoun_2013_M18 <- function(a, b, dbh) {
  h <- 1.3 + a * (log(1 + dbh))^b
  return(h)
}

# elmamoun_2013_M18_structure_a <- function(a, b, dbh, dg, N) {
#   h <- 1.3 + a * (log(1 + dbh + dbh/dg))^b
#   return(h)
# }
# 
# elmamoun_2013_M18_structure_b <- function(a, b, dbh, dg, N) {
#   h <- 1.3 + a * (log(1 + dbh + (dbh^2)/(N * (dg^2))))^b
#   return(h)
# }

elmamoun_2013_M19 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * dbh^c))
  return(h)
}

elmamoun_2013_M20 <- function(a, b, c, dbh) {
  h <- 1.3 + a / (1 + b^(-1) * dbh^(-c))
  return(h)
}

elmamoun_2013_M21 <- function(a, b, c, dbh) {
  h <- 1.3 + exp(a + b * (dbh^(-c)))
  return(h)
}

elmamoun_2013_M22 <- function(a, b, c, dbh) {
  h <- 1.3 + dbh^a / (b + (c * (dbh^a)))
  return(h)
}



# moore_height-diameter_1996: 2 additional models ====

#' @article{moore_height-diameter_1996,
#'   title = {Height-diameter equations for ten tree species in the {Inland} {Northwest}},
#'   volume = {11},
#'   url = {https://objects.lib.uidaho.edu/iftnc/iftnc4813.pdf},
#'   number = {4},
#'   journal = {Western Journal of Applied Forestry},
#'   author = {Moore, James A and Zhang, Lianjun and Stuck, Dean},
#'   year = {1996},
#'   note = {Publisher: Oxford University Press},
#'   pages = {132--137},
#' }

wykoff_1982_II <- function(a, b, dbh) {
  # h <- 4.5 + exp(a + (b / (dbh + 1)))  # feets
  h <- 1.3 + exp(a + (b / (dbh + 1)))  # meters
  return(h)
}

lundqvist_1989 <- function(a, b, c, dbh) {
  # h <- 4.5 + a * exp(-b * dbh^(-c))  # feets
  h <- 1.3 + a * exp(-b * dbh^(-c))  # meters
  return(h)
}


# temesgen_regional_2007: 4 additional models ====

#' @article{temesgen_regional_2007,
#'   title = {Regional {Height}–{Diameter} {Equations} for {Major} {Tree} {Species} of {Southwest} {Oregon}},
#'   volume = {22},
#'   issn = {0885-6095},
#'   url = {https://doi.org/10.1093/wjaf/22.3.213},
#'   doi = {10.1093/wjaf/22.3.213},
#'   abstract = {Selected tree height and diameter functions were evaluated for their predictive abilities for major tree species of southwest Oregon. Two sets of equations were evaluated. The first set included four base equations for estimating height as a function of individual tree diameter, and the remaining 16 equations enhanced the four base equations with alternative measures of stand density and relative position. The inclusion of the crown competition factor in larger trees (CCFL) and basal area (BA), which simultaneously indicates the relative position of a tree and stand density, into the base height–diameter equations increased the accuracy of prediction for all species. On the average, root mean square error values were reduced by 45 cm (15\% improvement). On the basis of the residual plots and fit statistics, two equations are recommended for estimating tree heights for major tree species in southwest Oregon. The equation coefficients are documented for future use.},
#'   number = {3},
#'   urldate = {2024-09-09},
#'   journal = {Western Journal of Applied Forestry},
#'   author = {Temesgen, Hailemariam and Hann, David W. and Monleon, Vincente J.},
#'   month = jul,
#'   year = {2007},
#'   pages = {213--219},
#' }

yang_1978 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * (dbh^c)))
  return(h)
}

chapman_richards_1959 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * dbh))^c
  return(h)
}

ratkowsky_1990 <- function(a, b, c, dbh) {
  h <- 1.3 + exp(a + b / (dbh + c))
  return(h)
}

hanus_1999 <- function(a, b, dbh) {
  h <- 1.3 + exp(a + b * (dbh)^c)
  return(h)
}



# temesgen_generalized_2004: 3 additional models ====

#' @article{temesgen_generalized_2004,
#'   title = {Generalized height–diameter models—an application for major tree species in complex stands of interior {British} {Columbia}},
#'   volume = {123},
#'   issn = {1612-4677},
#'   url = {https://doi.org/10.1007/s10342-004-0020-z},
#'   doi = {10.1007/s10342-004-0020-z},
#'   abstract = {Using permanent sample-plot data, selected tree height and diameter functions were evaluated for their predictive abilities for major tree species in complex (multiple age, size and species cohort) stands of interior British Columbia (BC), Canada. Two sets of models were evaluated. The first set included five models for estimating height as a function of individual tree diameter, the second set also included five models for estimating height as a function of individual tree diameter and other stand-level attributes. The inclusion of the BAL index (which simultaneously indicates the relative position of a tree and stand density) into the base height–diameter models increased the accuracy of prediction for all species. On average, by including stand level attributes, root mean square values were reduced by 30.0 cm. Based on the residual plots and fit statistics, these models can be recommended for estimating tree heights for major tree species in complex stands of interior BC. The model coefficients are documented for future use.},
#'   language = {en},
#'   number = {1},
#'   urldate = {2024-09-09},
#'   journal = {European Journal of Forest Research},
#'   author = {Temesgen, H. and v. Gadow, K.},
#'   month = apr,
#'   year = {2004},
#'   keywords = {Canada, BAL index, Multi-age forests},
#'   pages = {45--51},
#' }

wykoff_1982_III <- function(a, b, dbh) {
  h <- 1.3 + exp(a + b / (dbh + 1))
  return(h)
}

hui_gadow_1993_II <- function(a, b, dbh) {
  h <- 1.3 + a * exp(b / (dbh + 1))
  return(h)
}

hui_gadow_1993_III <- function(a, b, dbh) {
  h <- 1.3 + a * dbh^b
  return(h)
}



# scaranello_height-diameter_2012: 11 additional models ====

#' @article{scaranello_height-diameter_2012,
#'   title = {Height-diameter relationships of tropical {Atlantic} moist forest trees in southeastern {Brazil}},
#'   volume = {69},
#'   issn = {1678-992X},
#'   url = {https://www.scielo.br/j/sa/a/nYKKc7HrZ4wX4kFfxDWpZ9q/},
#'   doi = {10.1590/S0103-90162012000100005},
#'   abstract = {Site-specific height-diameter models may be used to improve biomass estimates for forest inventories where only diameter at breast height (DBH) measurements are available. In this study, we fit height-diameter models for vegetation types of a tropical Atlantic forest using field measurements of height across plots along an altitudinal gradient. To fit height-diameter models, we sampled trees by DBH class and measured tree height within 13 one-hectare permanent plots established at four altitude classes. To select the best model we tested the performance of 11 height-diameter models using the Akaike Information Criterion (AIC). The Weibull and Chapman-Richards height-diameter models performed better than other models, and regional site-specific models performed better than the general model. In addition, there is a slight variation of height-diameter relationships across the altitudinal gradient and an extensive difference in the stature between the Atlantic and Amazon forests. The results showed the effect of altitude on tree height estimates and emphasize the need for altitude-specific models that produce more accurate results than a general model that encompasses all altitudes. To improve biomass estimation, the development of regional height-diameter models that estimate tree height using a subset of randomly sampled trees presents an approach to supplement surveys where only diameter has been measured.},
#'   language = {en},
#'   urldate = {2024-09-09},
#'   journal = {Scientia Agricola},
#'   author = {Scaranello, Marcos Augusto da Silva and Alves, Luciana Ferreira and Vieira, Simone Aparecida and Camargo, Plinio Barbosa de and Joly, Carlos Alfredo and Martinelli, Luiz Antônio},
#'   month = feb,
#'   year = {2012},
#'   note = {Publisher: Escola Superior de Agricultura "Luiz de Queiroz"},
#'   keywords = {elevation, tree height},
#'   pages = {26--37},
#' }

linear_model_I <- function(a, b, dbh) {
  h <- a + b * dbh
  return(h)
}

linear_model_II <- function(a, b, dbh) {
  h <- a + b * log(dbh)
  return(h)
}

hyperbolic_model_I <- function(a, b, dbh) {
  h <- a * dbh / (b + dbh)
  return(h)
}

hyperbolic_model_II <- function(a, b, dbh) {
  h <- (dbh^2) / ((a + b * dbh)^2)
  return(h)
}

power_model <- function(a, b, dbh) {
  h <- a * dbh^b
  return(h)
}

exponential_model <- function(a, b, dbh) {
  h <- exp(a + b / (dbh + 1))
  return(h)
}

chapman_richards_model <- function(a, b, c, dbh) {
  h <- a * (1 - exp(b * dbh))^c
  return(h)
}

weibull_model <- function(a, b, c, dbh) {
  h <- a * (1 - exp(-b * dbh^c))
  return(h)
}

monomolecular_model <- function(a, b, c, dbh) {
  h <- a * (1 - b * exp(-c * dbh))
  return(h)
}

gompertz_model <- function(a, b, c, dbh) {
  h <- a * exp(b * exp(-c * dbh))
  return(h)
}

logistic_model <- function(a, b, c, dbh) {
  h <- a / (1 + b * exp(-c * dbh))
  return(h)
}

###

logistic_model_13 <- function(b, c, dbh) {
  h <- 1.3 / (1 + b * exp(-c * dbh))  # logistic model with a = 1.3
  return(h)
}



# lebedev_new_2020: 24 additional models ====

#' @article{lebedev_new_2020,
#'   title = {New generalised height-diameter models for the birch stands in {European} {Russia}},
#'   volume = {26},
#'   url = {https://www.researchgate.net/profile/Aleksandr-Lebedev-3/publication/346470846_New_generalised_height-diameter_models_for_the_birch_stands_in_European_Russia/links/5fc3b7cfa6fdcc6cc67fe2f6/New-generalised-height-diameter-models-for-the-birch-stands-in-European-Russia.pdf},
#'   number = {2},
#'   journal = {Baltic Forestry},
#'   author = {Lebedev, ALEKSANDR V},
#'   year = {2020},
#'   note = {Publisher: Lietuvos Misku Institutas},
#'   pages = {1--7},
#' }

# Note: silenced equations mean that the same model is already implemented in previous sections

# lebedev_M1 <- function(dbh, a, b) {
#   h <- 1.3 + a * dbh^b
#   return(h)
# }

lebedev_M2 <- function(dbh, a, b) {
  h <- 1.3 + (dbh / (a + b * dbh))^2
  return(h)
}

lebedev_M3 <- function(dbh, a, b) {
  h <- 1.3 + (a * dbh) / (b + dbh)
  return(h)
}

lebedev_M4 <- function(dbh, a, b) {
  h <- 1.3 + a * (dbh / (1 + dbh))^b
  return(h)
}

lebedev_M5 <- function(dbh, a, b) {
  h <- 1.3 + (a * dbh) / ((1 + dbh)^b)
  return(h)
}

lebedev_M6 <- function(dbh, a, b) {
  h <- 1.3 + a * (1 - exp(-b * dbh))
  return(h)
}

# lebedev_M7 <- function(dbh, a, b) {
#   h <- 1.3 + exp(a + b / (dbh + 1))
#   return(h)
# }

lebedev_M8 <- function(dbh, a, b) {
  h <- 1.3 + (a * dbh) / ((dbh + 1) + b * dbh)
  return(h)
}

lebedev_M9 <- function(dbh, a, b) {
  h <- 1.3 + a * dbh * exp(-b * dbh)
  return(h)
}

# lebedev_M10 <- function(dbh, a, b) {
#   h <- 1.3 + a * exp(b / dbh)
#   return(h)
# }

# lebedev_M11 <- function(dbh, a, b) {
#   h <- 1.3 + a * (log(1 + dbh))^b
#   return(h)
# }

# lebedev_M12 <- function(dbh, a, b) {
#   h <- 1.3 + (a + b / dbh)^(-5)
#   return(h)
# }

lebedev_M13 <- function(dbh, a, b, c) {
  h <- 1.3 + a / (1 + b * dbh^(-c))
  return(h)
}

lebedev_M14 <- function(dbh, a, b, c) {
  h <- 1.3 + (dbh^2) / (a + b * dbh + c * dbh^2)
  return(h)
}

lebedev_M15 <- function(dbh, a, b, c) {
  h <- 1.3 + a / (1 + b * exp(-c * dbh))
  return(h)
}

lebedev_M16 <- function(dbh, a, b, c) {
  h <- 1.3 + a * (1 - exp(-b * dbh^c))
  return(h)
}

lebedev_M17 <- function(dbh, a, b, c) {
  h <- 1.3 + a * (1 - exp(-b * dbh))^c
  return(h)
}

lebedev_M18 <- function(dbh, a, b, c) {
  h <- 1.3 + a * exp(-b * exp(-c * dbh))
  return(h)
}

lebedev_M19 <- function(dbh, a, b, c) {
  h <- 1.3 + exp(a + b * dbh^c)
  return(h)
}

# lebedev_M20 <- function(dbh, a, b, c) {
#   h <- 1.3 + exp(a + b / (dbh + c))
#   return(h)
# }

# lebedev_M21 <- function(dbh, a, b, c) {
#   h <- 1.3 + a * exp(-b * dbh^(-c))
#   return(h)
# }

lebedev_M22 <- function(dbh, a, b, c) {
  h <- 1.3 + a * sqrt(dbh) + b * dbh + c * dbh^2
  return(h)
}

lebedev_M23 <- function(dbh, a, b, c) {
  h <- 1.3 + a / (1 + (b * dbh^c)^(-1))
  return(h)
}

lebedev_M24 <- function(dbh, a, b, c) {
  h <- 1.3 + a * dbh^(b * dbh^(c * dbh^(-c)))
  return(h)
}



# lebedev_verification_2020: 29 additional models (repeated not coded) ====

#' @article{lebedev_verification_2020,
#'   title = {Verification of two-and three-parameter simple height-diameter models for birch in the {European} part of {Russia}.},
#'   url = {https://jfs.agriculturejournals.cz/pdfs/jfs/2020/09/04.pdf},
#'   author = {Lebedev, Aleksandr and Kuzmichev, Valery},
#'   year = {2020},
#' }

lebedev_M16_b <- function(dbh, a, b) {
  h <- 1.3 + a / (1 + (b * dbh)^(-1))
  return(h)
}

lebedev_M25_b <- function(dbh, a, b, c) {
  h <- ((1.3^a + (b^a - 1.3^a)) * (1 - exp(-c * dbh)) / (1 - exp(-100 * c)))^(1 / a)
  return(h)
}

# lebedev_M26 <- function(dbh, a, b, c) {
#   h <- 1.3 + a * sqrt(dbh) + b * dbh + c * dbh^2
#   return(h)
# }

# lebedev_M27 <- function(dbh, a, b, c) {
#   h <- 1.3 + a / (1 + (b * dbh^c)^(-1))
#   return(h)
# }

lebedev_M28_b <- function(dbh, a, b, c) {
  h <- 1.3 + a * dbh^(b * dbh^(-c))
  return(h)
}

lebedev_M29_b <- function(dbh, a, b, c) {
  h <- 1.3 + dbh^(a / (b + c * dbh^a))
  return(h)
}



# Models list including the functions and starting values for those models that only use dbh as predictor ====

# Function that creates the models list, referencing the already defined functions and providing start values
# list structure: model_name = list(func = function_name, start = list(starting_values))

get_models_list <- function(){
  
  models <- list(
    
    # models from Rodríguez de Prado et al., 2022
    huang_1992 = list(
      func = huang_1992,
      start = list(beta0 = 28.1721, beta1 = 17.524, beta2 = 0.0877228)
    ),
    meyer_1940 = list(
      func = meyer_1940,
      start = list(beta0 = 65.5812, beta1 = 0.0377244)
    ),
    pearl_1920 = list(
      func = pearl_1920,
      start = list(beta0 = 74.2117, beta1 = 50.6808, beta2 = 0.105718)
    ),
    ratkowsky_1986 = list(
      func = ratkowsky_1986,
      start = list(beta0 = 38.4542, beta1 = 0.00912756, beta2 = 1.25178)
    ),
    # richards_1959 = list(
    #   func = richards_1959,
    #   start = list(beta0 = 35.479, beta1 = -0.0403694, beta2 = 0.842554)
    # ),
    schumacher_1939 = list(
      func = schumacher_1939,
      start = list(beta0 = 24.1105, beta1 = -12.8314)
    ),
    seber_1989 = list(
      func = seber_1989,
      start = list(beta0 = 41.3547, beta1 = -2.04973, beta2 = 16.6375)
    ),
    zeide_1992 = list(
      func = zeide_1992,
      start = list(beta0 = 20.7266, beta1 = 1.50327, beta2 = 0.0782159, beta3 = 1.69312)
    ),
    
    # Additional models from Wagle et al. (2024)
    huang_1992_I = list(
      func = huang_1992_I,
      start = list(a = 53.5303, b = 202.698, c = -157.315)
    ),
    huang_1992_II = list(
      func = huang_1992_II,
      start = list(a = 7.70946, b = 0.083061, c = 1.72564)
    ),
    huang_1992_III = list(
      func = huang_1992_III,
      start = list(a = 18.5933, b = 0.0554496, c = 0.848852)
    ),
    huang_2000_I = list(
      func = huang_2000_I,
      start = list(a = 703.791, b = 47101.8, c = 1.56473)
    ),
    peschel_1938 = list(
      func = peschel_1938,
      start = list(a = 38.8038, b = -0.0600187, c = 1.26553)
    ),
    huang_2000_II = list(
      func = huang_2000_II,
      start = list(a = 0.9, b = 0.01)
    ),
    curtis_1967_I = list(
      func = curtis_1967_I,
      start = list(a = 35, b = 0.03)
    ),
    huang_1992_IV = list(
      func = huang_1992_IV,
      start = list(a = 1.35936, b = 2.00535, c = 0.13649)
    ),
    flewelling_jong_1994 = list(
      func = flewelling_jong_1994,
      start = list(a = 81.6411, b = 69.3465, c = 0.608473)
    ),
    huang_1992_V = list(
      func = huang_1992_V,
      start = list(a = 35, b = 25)
    ),
    huang_1992_VI = list(
      func = huang_1992_VI,
      start = list(a = -71.0788, b = 1.97052, c = -0.325017)
    ),
    curtis_1967_II = list(
      func = curtis_1967_II,
      start = list(a = 1.4, b = 0.04)
    ),
    stoffels_1953 = list(
      func = stoffels_1953,
      start = list(a = 2.5, b = 0.6)
    ),
    peschel_1938_II = list(
      func = peschel_1938_II,
      start = list(a = 0.16, b = 2.25)
    ),
    ogana_2018 = list(
      func = ogana_2018,
      start = list(a = 0.2, b = 0.6)
    ),
    wykoff_1982_I = list(
      func = wykoff_1982_I,
      start = list(a = 3.5, b = 18)
    ),
    larsen_hann_1987 = list(
      func = larsen_hann_1987,
      start = list(a = -6.91652, b = 8.15569, c = 0.090637)
    ),
    curtis_1967_III = list(
      func = curtis_1967_III,
      start = list(a = 35, b = -18)
    ),
    huang_2000_III = list(
      func = huang_2000_III,
      start = list(a = 35, b = 17)
    ),
    staudhammer_lemay_2000 = list(
      func = staudhammer_lemay_2000,
      start = list(a = 3.5, b = -17)
    ),
    burkhart_strub_1974 = list(
      func = burkhart_strub_1974,
      start = list(a = 35, b = 17)
    ),
    huang_1992_VII = list(
      func = huang_1992_VII,
      start = list(a = 518.595, b = 74.7981, c = 0.0967295)
    ),
    strand_1958 = list(
      func = strand_1958,
      start = list(a = 0.0592355, b = 0.500404, c = 3.51015)
    ),
    
    # Additional models from El Mamoun et al. (2013)
    elmamoun_2013_M1 = list(
      func = elmamoun_2013_M1,
      start = list(a = 2.50443, b = 0.558941)
    ),
    elmamoun_2013_M2 = list(
      func = elmamoun_2013_M2,
      start = list(a = 24.1105, b = -12.8314)
    ),
    elmamoun_2013_M3 = list(
      func = elmamoun_2013_M3,
      start = list(a = -7.7432, b = 8.72703, c = 0.0885182)
    ),
    elmamoun_2013_M4 = list(
      func = elmamoun_2013_M4,
      start = list(a = 7.96185, b = 0.492054, c = 0.0500846)
    ),
    elmamoun_2013_M5 = list(
      func = elmamoun_2013_M5,
      start = list(a = 0.88906, b = 0.608923, c = 0.0413332)
    ),
    elmamoun_2013_M6 = list(
      func = elmamoun_2013_M6,
      start = list(a = 45010.9, b = -181.272)
    ),
    elmamoun_2013_M7 = list(
      func = elmamoun_2013_M7,
      start = list(a = 1.87138, b = 0.20016)
    ),
    elmamoun_2013_M8 = list(
      func = elmamoun_2013_M8,
      start = list(a = 0.534934, b = 1.57378)
    ),
    elmamoun_2013_M9 = list(
      func = elmamoun_2013_M9,
      start = list(a = 34, b = -0.04, c = 1.5)
    ),
    elmamoun_2013_M10 = list(
      func = elmamoun_2013_M10,
      start = list(a = 3.12932, b = -14.2033)
    ),
    elmamoun_2013_M11 = list(
      func = elmamoun_2013_M11,
      start = list(a = 3.73681, b = 40792.2, c = 0.877071)
    ),
    elmamoun_2013_M12 = list(
      func = elmamoun_2013_M12,
      start = list(a = 70708.3, b = 176694)
    ),
    elmamoun_2013_M13 = list(
      func = elmamoun_2013_M13,
      start = list(a = 1.87138, b = 0.20016)
    ),
    elmamoun_2013_M14 = list(
      func = elmamoun_2013_M14,
      start = list(a = 0.282106, b = 1.92917)
    ),
    elmamoun_2013_M15 = list(
      func = elmamoun_2013_M15,
      start = list(a = 0.669769, b = 1.16989)
    ),
    elmamoun_2013_M16 = list(
      func = elmamoun_2013_M16,
      start = list(a = 24.5061, b = 13.502)
    ),
    elmamoun_2013_M17 = list(
      func = elmamoun_2013_M17,
      start = list(a = 3.09748, b = -12.8314)
    ),
    elmamoun_2013_M18 = list(
      func = elmamoun_2013_M18,
      start = list(a = 1.7199, b = 1.87196)
    ),
    # elmamoun_2013_M18_structure_a = list(
    #   func = elmamoun_2013_M18_structure_a,
    #   start = list(a = 1, b = 0.5)
    # ),
    # elmamoun_2013_M18_structure_b = list(
    #   func = elmamoun_2013_M18_structure_b,
    #   start = list(a = 1, b = 0.5)
    # ),
    elmamoun_2013_M19 = list(
      func = elmamoun_2013_M19,
      start = list(a = -50.0832, b = -0.226828, c = 0.503186)
    ),
    elmamoun_2013_M20 = list(
      func = elmamoun_2013_M20,
      start = list(a = 35.7114, b = -0.0676501, c = 1.43278)
    ),
    elmamoun_2013_M21 = list(
      func = elmamoun_2013_M21,
      start = list(a = -7.47421, b = 8.45514, c = -0.0852862)
    ),
    elmamoun_2013_M22 = list(
      func = elmamoun_2013_M22,
      start = list(a = 1.34139, b = 141407, c = 0.021394)
    ),
    
    # Additional models from Moore et al. (1996)
    wykoff_1982_II = list(
      func = wykoff_1982_II,
      start = list(a = 3.12932, b = -14.2033)
    ),
    lundqvist_1989 = list(
      func = lundqvist_1989,
      start = list(a = 87.7582, b = 7.5747, c = 0.7127)
    ), 
    
    # New models from Temesgen et al. (2007)
    yang_1978 = list(
      func = yang_1978,
      start = list(a = -50.0832, b = -0.226828, c = 0.503186)
    ),
    chapman_richards_1959 = list(
      func = chapman_richards_1959,
      start = list(a = 34, b = -0.04, c = 1.5)
    ),
    ratkowsky_1990 = list(
      func = ratkowsky_1990,
      start = list(a = -0.444361, b = -1609.04, c = -220.235)
    ),
    hanus_1999 = list(
      func = hanus_1999,
      start = list(a = 3.5, b = -18)
    ),
    
    # New models from Temesgen et al. (2004)
    wykoff_1982_III = list(
      func = wykoff_1982_III,
      start = list(a = 3.12932, b = -14.2033)
    ),
    hui_gadow_1993_II = list(
      func = hui_gadow_1993_II,
      start = list(a = 24.9199, b = -14.2034)
    ),
    hui_gadow_1993_III = list(
      func = hui_gadow_1993_III,
      start = list(a = 2.50443, b = 0.558941)
    ),
    
    # New models from Scaranello et al. (20012)
    linear_model_I = list(
      func = linear_model_I,
      start = list(a = 7.16294, b = 0.292507)
    ),
    linear_model_II = list(
      func = linear_model_II,
      start = list(a = -8.35283, b = 7.25604)
    ),
    hyperbolic_model_I = list(
      func = hyperbolic_model_I,
      start = list(a = 34.1539, b = 31.863)
    ),
    hyperbolic_model_II = list(
      func = hyperbolic_model_II,
      start = list(a = 0.16, b = 2.25)
    ),
    power_model = list(
      func = power_model,
      start = list(a = 3.16786, b = 0.503811)
    ),
    exponential_model = list(
      func = exponential_model,
      start = list(a = 3.16782, b = -12.6373)
    ),
    chapman_richards_model = list(
      func = chapman_richards_model,
      start = list(a = 35, b = -0.04, c = 1.5)
    ),
    weibull_model = list(
      func = weibull_model,
      start = list(a = 7.70946, b = 0.083061, c = 1.72564)
    ),
    monomolecular_model = list(
      func = monomolecular_model,
      start = list(a = 19.2847, b = 9.19203, c = 0.0666412)
    ),
    gompertz_model = list(
      func = gompertz_model,
      start = list(a = 13.6979, b = -0.502763, c = 0.0683727)
    ),
    logistic_model = list(
      func = logistic_model,
      start = list(a = 518.595, b = 74.7981, c = 0.0967295)
    ),
    logistic_model_13 = list(
      func = logistic_model_13,
      start = list(b = 1, c = 0.1)
    ),
    lebedev_M2 = list(
      func = lebedev_M2,
      start = list(a = 2.0, b = 0.17)
    ),
    lebedev_M3 = list(
      func = lebedev_M3,
      start = list(a = 5.22039e+08, b = 12.6429)
    ),
    lebedev_M4 = list(
      func = lebedev_M4,
      start = list(a = 2.38726e+07, b = 12.6718)
    ),
    lebedev_M5 = list(
      func = lebedev_M5,
      start = list(a = 0.000478539, b = 35015.7)
    ),
    lebedev_M6 = list(
      func = lebedev_M6,
      start = list(a = 33.7, b = 0.026)
    ),
    lebedev_M8 = list(
      func = lebedev_M8,
      start = list(a = -1.50414e+06, b = 840.699)
    ),
    lebedev_M9 = list(
      func = lebedev_M9,
      start = list(a = 0.000532648, b = 29764.1)
    ),
    lebedev_M13 = list(
      func = lebedev_M13,
      start = list(a = 2.06646, b = 17.8235, c = 7.06001)
    ),
    lebedev_M14 = list(
      func = lebedev_M14,
      start = list(a = 10, b = 1, c = 0.03)
    ),
    lebedev_M15 = list(
      func = lebedev_M15,
      start = list(a = 33.7, b = 10, c = 0.1)
    ),
    lebedev_M16 = list(
      func = lebedev_M16,
      start = list(a = 1.07094, b = 9.91774, c = 0.290893)
    ),
    lebedev_M17 = list(
      func = lebedev_M17,
      start = list(a = 33.7, b = 0.04, c = 1.5)
    ),
    lebedev_M18 = list(
      func = lebedev_M18,
      start = list(a = 33.7, b = 5, c = 0.085)
    ),
    lebedev_M19 = list(
      func = lebedev_M19,
      start = list(a = 1.00079, b = -30.6927, c = 32.7335)
    ),
    lebedev_M22 = list(
      func = lebedev_M22,
      start = list(a = 1.5, b = 0.1, c = -0.001)
    ),
    lebedev_M23 = list(
      func = lebedev_M23,
      start = list(a = 1.19061, b = 19.7942, c = 0.291493)
    ),
    lebedev_M24 = list(
      func = lebedev_M24,
      start = list(a = 1.0, b = 0.5, c = 0.1)
    ),
    lebedev_M16_b = list(
      func = lebedev_M16_b,
      start = list(a = -3.30466e+06, b = 12.6429)
    ),
    lebedev_M25_b = list(
      func = lebedev_M25_b,
      start = list(a = 0.794157, b = 6.45163, c = 20.2164)
    ),
    lebedev_M28_b = list(
      func = lebedev_M28_b,
      start = list(a = 1.10242, b = 17.2037, c = -24.0438)
    ),
    lebedev_M29_b = list(
      func = lebedev_M29_b,
      start = list(a = 151.028, b = 0.199969, c = -4.10917)
    )
  )
  return(models)
}
