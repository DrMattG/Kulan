#' Calculate Bayesian estimate and 95% HDI
#' @param p_area proportion of the stratum that has been sampled
#' @param p_min prior minimum number of animals
#' @param p_max prior maximum number of animals
#' @param no_ind Indivduals count column from data
#' @param plot.out Add a plot ("Yes") of the distribution of the estimate (default is "No")
#' @return dataframe with estimate and hdi
#' @export


run_kulan_model=function(p_area=p_area., p_min=p_min., p_max=p_max., no_ind=no_ind., plot.out="No"){

  ### ### Jags ###
  library(MCMCpack)
  library(jagsUI)
  library(wiqid)
#
 write("
model {

	# Prior:
	N ~ dunif(p_min, p_max)	# No. of kulan in whole area
	n <- N * a				# No. of kulan in area a
			# Likelihood:
	C ~ dpois(n)

}
","Kulan.jags") # close quote for modelstring
# writeLines(modelstring, con="model.txt")
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Prepare the data:
# -----------------

jagsData <- list(a = p_area, C = sum(no_ind), p_min=p_min, p_max=p_max)

# Run the model in JAGS
# ------------------------
jagsout <- jags(jagsData, inits=NULL, parameters.to.save=c("N","n"),
                model.file="Kulan.jags",
                n.chains=3, n.iter=20000, n.burnin=0, DIC=FALSE)
#jagsout
#plot(jagsout)
Estimate <- mcmcOutput(jagsout)
out<-data.frame("Estimate"=mean(Estimate$N), "lower_hdi"=hdi(Estimate$N)[1], "upper_hdi"=hdi(Estimate$N)[2], row.names = NULL)
switch(plot.out,
       Yes={p1<-plot(Estimate,"N")
       return(out)
       return(p1)

       },
       No={return(out)
       }
)

}

#run_kulan_model(plot.out = "No")
#run_kulan_model(plot.out = "Yes")
