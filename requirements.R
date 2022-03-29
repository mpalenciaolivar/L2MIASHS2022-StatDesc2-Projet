pkgLoad <- function( packages = "favourites" ) {
  
  if( length( packages ) == 1L && packages == "favourites" ) {
    # Inscrire les packages que vous souhaitez inclure ci-dessous, en respectant le formalisme ci-dessous.
    # De même, triez la liste des packages par ordre alphabétique.
    # N.B: Les packages ci-dessous sont indiqués à titre d'illustration, ils ne sont pas forcément utiles et ne sont pas
    # obligatoires pour le projet. Si vous avez reconnu les packages du TD 3, vous avez raison.
    packages <- c(
      "Amelia",
      "aod",
      "dplyr",
      "e1071",
      "GGally",
      "ggplot2",
      "MLmetrics",
      "psych",
      "ROSE",
      "rpart",
      "rpart.plot"
    )
  }
  
  packagecheck <- match( packages, utils::installed.packages()[, 1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "https://pbil.univ-lyon1.fr/CRAN/"
    )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
  
}


pkgLoad()
