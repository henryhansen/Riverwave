
# plot prior predictive checks --------------------------------------------

strong.prior.pred <- pp_check(strong.prior.only, ndraws = 50)+
    xlim(0, 2500) +
    ylim(0, 0.01) +
    theme_cowplot() +
    theme(legend.position = "none")+
    theme(axis.text = element_text(size = 20))

strong.prior.pred #show plot

weak.prior.only <- brm(Discharge ~1, data = edbk.f, family = lognormal(),
                       prior = c(weak.mu.prior, weak.sigma.prior),
                       sample_prior = "only",
                       seed = 1234,
)

weak.prior.pred <- pp_check(weak.prior.only, ndraws = 50)+
    xlim(0, 2500) +
    ylim(0, 0.01) +
    theme_cowplot() +
    theme(legend.position = "none") +
    theme(axis.text = element_text(size = 20))

weak.prior.pred #show plot

def.prior.only <- brm(Discharge ~1, data = edbk.f, family = lognormal(),
                      sample_prior = "only",
                      seed = 1234,
)

def.prior.pred <- pp_check(def.prior.only, ndraws = 50) +
    xlim(0, 2500) +
    ylim(0, 0.01) +
    theme_cowplot() +
    theme(legend.position=c(.75,.75),
          legend.text=element_text(size=25)) +
    theme(axis.text = element_text(size = 20))

def.prior.pred #show plot

# Combine plots -----------------------------------------------------------

prior.pred.checks <- plot_grid(strong.prior.pred,
                               weak.prior.pred,
                               def.prior.pred,
                               nrow=1,
                               labels = "AUTO",
                               label_size = 35,
                               hjust = -4.5,
                               vjust = 3,
                               scale = 0.9)

prior.pred.checks

# Open tiff device
png("plots/prior-predictive-checks.png",
    width = 1400,
    height = 500,
    units = "px")

# Make  plot
prior.pred.checks

# Close device
dev.off()

# Here you can check the default prior look -------------------------------
rt_modified <- function(N, nu, mu, standard_dev){
    x1 <- rt(N, nu) # 1
    x2 <- x1/sqrt(nu/(nu-2)) # 2
    x3 <- x2 * standard_dev # 3
    x4 <- x3 + mu # 4
    return(x4)
}



# posterior predictive checks ---------------------------------------------
# # plot posterior predictive checks
post.plots <- lapply(model.outputs, function (j) as_grob(pp_check(j, ndraws = 50) + theme_cowplot()))
post.pred.checks <- plot_grid(plotlist = post.plots,
                              labels = "AUTO",
                              label_size = 35,
                              hjust = 0.0125,
                              scale = 0.9)

post.pred.checks

# Open tiff device
png("plots/post-predictive-checks.png",
    width = 1500,
    height = 1500,
    units = "px")

# Make  plot
post.pred.checks

# Close device
dev.off()

