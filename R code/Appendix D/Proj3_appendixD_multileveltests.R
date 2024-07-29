#############################################################################################################
# Analyses Appendix D 
#############################################################################################################

#############################################################################################################
# Load packages 
#############################################################################################################
library(lme4)
library(lmerTest)

#############################################################################################################
# Significance test parental worry 
#############################################################################################################
# Extract PPN, day, and worry 
## Control 
worrymothercontr <- proj3_hc_endofday_meanaf2$mother[, c("PPN.y", "Day2", "E_WORRY")]
worryfathercontr <- proj3_hc_endofday_meanaf2$father[, c("PPN.y", "Day2", "E_WORRY")]

worrymotherfathercontr <- rbind(worrymothercontr,
                                worryfathercontr)

worrymotherfathercontr$depr <- rep(0, nrow(worrymotherfathercontr))  # 0 meaning control 

## Depr 
worrymotherdepr <- proj3_depr_endofday_meanaf_am[, c("PPN.y", "Day2", "E_WORRY")]
worryfatherdepr <- proj3_depr_endofday_meanaf_af[, c("PPN.y", "Day2", "E_WORRY")]

worrymotherfatherdepr <- rbind(worrymotherdepr, worryfatherdepr)

worrymotherfatherdepr$depr <- rep(1, nrow(worrymotherfatherdepr))  # 1 meaning depr sample 

# Combine data control and depr 
worrymotherfathercomb <- rbind(worrymotherfathercontr, worrymotherfatherdepr)


modelfit1_worry <- lmer(formula = E_WORRY ~ 1 + (1|PPN.y), 
                        data = worrymotherfathercomb,
                        na.action = na.exclude)

modelfit2_worry <- lmer(formula = E_WORRY ~ 1 + depr + (1|PPN.y), 
                        data = worrymotherfathercomb,
                        na.action = na.exclude)

summary(modelfit2_worry)

# Below failed to converge 
modelfit3_worry <- lmer(formula = E_WORRY ~ 1 + depr + (1 + depr|PPN.y), 
                        data = worrymotherfathercomb,
                        na.action = na.exclude)

summary(modelfit3_worry)
anova(modelfit1_worry, modelfit2_worry, modelfit3_worry, refit = FALSE)

#############################################################################################################
# Significance test parental self-efficacy 
#############################################################################################################
# Extract PPN, day, and worry 
## Control 
selfmothercontr <- proj3_hc_endofday_meanaf2$mother[, c("PPN.y", "Day2", "E_PESTEEM")]
selffathercontr <- proj3_hc_endofday_meanaf2$father[, c("PPN.y", "Day2", "E_PESTEEM")]

selfmotherfathercontr <- rbind(selfmothercontr,
                               selffathercontr)

selfmotherfathercontr$depr <- rep(0, nrow(selfmotherfathercontr))  # 0 meaning control 

## Depr 
selfmotherdepr <- proj3_depr_endofday_meanaf_am[, c("PPN.y", "Day2", "E_PESTEEM")]
selffatherdepr <- proj3_depr_endofday_meanaf_af[, c("PPN.y", "Day2", "E_PESTEEM")]


selfmotherfatherdepr <- rbind(selfmotherdepr, selffatherdepr)

selfmotherfatherdepr$depr <- rep(1, nrow(selfmotherfatherdepr))  # 1 meaning depr sample 

# Combine data control and depr 
selfmotherfathercomb <- rbind(selfmotherfathercontr, selfmotherfatherdepr)

# Multilevel model 
modelfit1_self <- lmer(formula = E_PESTEEM ~ 1 + (1|PPN.y), 
                       data = selfmotherfathercomb,
                       na.action = na.exclude)

modelfit2_self <- lmer(formula = E_PESTEEM ~ 1 + depr + (1|PPN.y), 
                       data = selfmotherfathercomb,
                       na.action = na.exclude)

summary(modelfit2_self)


modelfit3_self <- lmer(formula = E_PESTEEM ~ 1 + depr + (1 + depr|PPN.y), 
                       data = selfmotherfathercomb,
                       na.action = na.exclude)

summary(modelfit3_self)
anova(modelfit1_self, modelfit2_self, modelfit3_self, refit = FALSE)

#####################################################################################################
# Significance test contact evaluation 
#####################################################################################################
# data: proj3_evalcontact_datacomb

modelfit1_contact <- lmer(formula = Evaluation ~ 1 + (1|PPN), 
                          data = proj3_evalcontact_datacomb,
                          na.action = na.exclude)

modelfit2_contact <- lmer(formula = Evaluation ~ 1 + depr + (1|PPN), 
                          data = proj3_evalcontact_datacomb,
                          na.action = na.exclude)

summary(modelfit2_contact)

# Use this one 
modelfit3_contact <- lmer(formula = Evaluation ~ 1 + depr + (1 + depr|PPN), 
                          data = proj3_evalcontact_datacomb,
                          na.action = na.exclude)

summary(modelfit3_contact)
anova(modelfit1_contact, modelfit2_contact, modelfit3_contact, refit = FALSE)
