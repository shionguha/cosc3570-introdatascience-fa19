library(effects)

mod.cowles <- glm(volunteer ~ sex + neuroticism*extraversion,
                  data=Cowles, family=binomial)


summary(mod.cowles)

plot(allEffects(mod.cowles))

e.out <- allEffects(mod.cowles)

e.out$sex$model.matrix

mean(Cowles$neuroticism)
mean(Cowles$extraversion)
mean(Cowles$neuroticism) * mean(Cowles$extraversion) # interaction


plot(allEffects(mod.cowles, 
           xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20),
           given.values=c(sexmale=1)))


plot(Effect(focal.predictors = c("neuroticism","extraversion"), mod = mod.cowles,
            xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20),
            given.values=c(sexmale=1)))


plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"), mod = mod.cowles,
            xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20)))


plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"), mod = mod.cowles,
            xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20)),
     multiline = TRUE)


