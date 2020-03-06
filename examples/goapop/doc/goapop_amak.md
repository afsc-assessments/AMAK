---
title: "Trial runs of Gulf of Alaska Pacific ocean perch using AMAK"
author: |
  | James Ianelli$^1$ 
  | $^1$NOAA, jim.ianelli@noaa.gov
date: "March 2020"
output:
  pdf_document:
    includes:
    highlight: zenburn
  html_document:
    theme: flatly
    toc: yes
  word_document: default
---





\pagenumbering{gobble}

# Executive Summary

1. **Stock**: Pacific ocean perch, *Sebastes alutus*, Gulf of Alaska.

2. **Catches**: Peak historical catch was 

3. **Stock biomass**: The NMFS trawl survey mean biomass is 

4. **Recruitment**: Recruitment is based on 

5. **Management performance**: In this assessment estimated total catch 


# A. Summary of Major Changes

## Changes in Management of the Fishery

There are no new changes in management of the fishery.

## Changes to the Input Data

Data used in this assessment have been updated to include the most recently
available fishery and survey numbers. 

## Changes in Assessment Methodology

This assessment uses the AMAK (Assessment model for Alaska)  framework. The
model is configured to track ages over time and uses both age and length composition data.

## Changes in Assessment Results


# B. Responses to SSC and CPT Comments

## GOA Plan Team and SSC Comments on Assessments in General

Comment: *Regarding general code development, the SSC and PT outstanding requests continue to be as follows:*

  1. *add the ability to conduct retrospective analyses*

     Progress was limited in implementing this feature.

  1. *add ability to estimate bycatch fishing mortality rates when observer data are missing but effort data is available*

     This was completed.

  1. *Continued exploration of data weighting (Francis and other approaches) and evaluation of models with and without the 
     1998 natural mortality spike. The authors are encouraged to bring other models forward for CPT and SSC consideration*

      We continued to include an alternative time series estimated from the NMFS trawl survey using the VAST spatiotemporal Delta GLMM model 
      and continued with the iterative re-weighting for composition data.


# C. Introduction

## Scientific Name

The Pacific ocean perch  is a rockfish species, *Sebastes alutus*.

## Distribution

Pacific ocean perch are broadly distributed throughout the North Pacific Ocean from Hokkaido, Japan, to northern California
(Figure \ref{fig:distribution}).  In the Gulf of Alaska...

## Stock Structure

The regional population
differences in POP...

## Life History

Like other rockfish species...

## Management History


The fishing seasons ...

NMFS declared the stock rebuilt on 21 September 2009, and the fishery was reopened after a 10-year closure on 15 October
2009 with a TAC of 529 t (1.167 million pounds), closing again by regulation on 1 February 2010. Seven participating
vessels landed a catch of 209 t (0.461 million pounds) with a reported effort of 10,697 pot lifts and an estimated CPUE of
9.9 retained individual crab per pot lift. The fishery remained open the next three years with modest harvests and
similar CPUE, but large declines in the NMFS trawl-survey estimate of stock abundance raised concerns about the health
of the stock. This prompted ADF&G to close the fishery again for the 2013/14 season. 
The fishery was reopened for the 2014/15 season with a low TAC of 297 t (0.655 million pounds) and in 2015/16 the TAC
was further reduced to 186 t (0.411 million pounds) then completely closed during the 2016/17 season.

Although historical observer data are limited due to low sampling effort, bycatch of female and sublegal male crab from
the directed blue king crab fishery off St. Matthew Island was relatively high historically, with estimated total
bycatch in terms of number of crab captured sometimes more than twice as high as the catch of legal crab (Moore et al.
2000; ADF&G Crab Observer Database). Pot-lift sampling by ADF&G crab observers (Gaeuman 2013; ADF&G Crab Observer
Database) indicates similar bycatch rates of discarded male crab since the reopening of the fishery (Table
\ref{tab:stage_cpue_1}), with total male discard mortality in the 2012/13 directed fishery estimated at about 12% (88
t or 0.193 million pounds) of the reported retained catch weight, assuming 20% handling mortality.

These data suggest a reduction in the bycatch of females, which may be attributable to the later timing of the
contemporary fishery and the more offshore distribution of fishery effort since reopening in 2009/10^[D. Pengilly,
ADF&G, pers. comm.]. Some bycatch of discarded blue king crab has also been observed historically in the eastern Bering
Sea snow crab fishery, but in recent years it has generally been negligible.  The St. Matthew Island golden king crab
fishery, the third commercial crab fishery to have taken place in the area,  typically occurred in areas with depths
exceeding blue king crab distribution. The NMFS observer data suggest that variable, but mostly limited, SMBKC bycatch has
also occurred in the eastern Bering Sea groundfish fisheries (Table \ref{tab:smbkc_groundfish_bycatch}).

# D. Data

## Summary of New Information

Data used in this assessment were updated to include the most recently available fishery and survey numbers. This
assessment makes use of two new survey data points including the 2018 NMFS trawl-survey estimate of abudance, and the
2018 ADF&G pot survey CPUE. Both of these surveys have associated size compositon data. The assessment also uses updated
1993-2016 groundfish and fixed gear bycatch estimates based on AKRO data. The fishery was closed in 2016/17 so no
directed fishery catch data were available. The data used in each of the new models is shown in Figure
\ref{fig:data_extent}.

## Major Data Sources

Major data sources used in this assessment include annual directed-fishery retained-catch statistics from fish tickets
(1978/79-1998/99, 2009/10-2012/13, and 2014/15-2015/16; Table \ref{tab:smbkc_fishery}); results from the annual NMFS
eastern Bering Sea trawl survey (1978-2018; Table \ref{tab:stage_cpue_nmfs}); results from the ADF&G SMBKC pot
survey (every third year during 1995-2013, then 2015-2018; Table \ref{tab:stage_cpue});
mean somatic mass given length category by year (Table \ref{tab:length_weight}); 
size-frequency information from ADF&G crab-observer pot-lift sampling (1990/91-1998/99, 2009/10-2012/13, and
2014/15-2016/17; Table \ref{tab:stage_cpue_1}); and the NMFS groundfish-observer bycatch biomass estimates (1992/93-2016/17;
Table \ref{tab:smbkc_groundfish_bycatch}).

Figure \ref{fig:stations} maps stations from which SMBKC trawl-survey and pot-survey data were obtained. Further
information concerning the NMFS trawl survey as it relates to commercial crab species is available in Daly et al.
(2014); see Gish et al. (2012) for a description of ADF&G SMBKC pot-survey methods. It should be noted that the two
surveys cover different geographic regions and that each has in some years encountered proportionally large numbers of
male blue king crab in areas not covered by the other survey (Figure \ref{fig:catch181}). Crab-observer sampling
protocols are detailed in the crab-observer training manual (ADF&G 2013). Groundfish SMBKC bycatch data come from the NMFS
Regional office and have been compiled to coincide with the 
SMBKC management area.


## Other Data Sources

The growth transition matrix used is based on Otto and Cummiskey (1990), as in the past. Other relevant data sources,
including assumed population and fishery parameters, are presented in Appendix A, which also provides a detailed
description of the model configuration used for this assessment.

# E. Analytic Approach

## History of Modeling Approaches for this Stock

A four-stage catch-survey-analysis (CSA) assessment model was used before 2011 to estimate abundance and biomass and
prescribe fishery quotas for the SMBKC stock. The four-stage CSA is similar to a full
length-based analysis, the major difference being coarser length groups, which are more suited to a small stock with
consistently low survey catches. In this approach, the abundance of male crab with a CL $\ge$ 90 mm is modeled in
terms of four crab stages: stage 1: 90-104 mm CL; stage 2: 105-119 mm CL; stage 3: newshell 120-133 mm CL; and stage 4:
oldshell $\ge$ 120 mm CL and newshell $\ge$ 134 mm CL. Motivation for these stage definitions comes from the fact that
for management of the SMBKC stock, male crab measuring $\ge$ 105 mm CL are considered mature, whereas 120 mm CL is
considered a proxy for the legal size of 5.5 in carapace width, including spines. Additional motivation for these stage
definitions comes from an estimated average growth increment of about 14 mm per molt for SMBKC (Otto and Cummiskey
1990).

Concerns about the pre-2011 assessment model led to the CPT and SSC recommendations that included development of an
alternative model with provisional assessment based on survey biomass or some other index of abundance. An alternative
3-stage model was proposed to the CPT in May 2011, but a survey-based approach was requested for the Fall
2011 assessment. In May 2012 the CPT approved a slightly revised and better documented version of the alternative model
for assessment. Subsequently, the model developed and used since  2012 was a variant of the previous four-stage SMBKC
CSA model and similar in complexity to that described by Collie et al. (2005). Like the earlier model, it considered
only male crab $\ge$ 90 mm in CL, but combined stages 3 and 4 of the earlier model, resulting in three stages
(male size classes) defined by CL measurements of (1) 90-104 mm, (2) 105-119 mm, and (3) 120 mm+ (i.e., 120 mm and
above). This consolidation was driven by concern about the accuracy and consistency of shell-condition information,
which had been used in distinguishing stages 3 and 4 of the earlier model.

In 2016 the accepted SMBKC assessment model made use of the modeling framework Gmacs (Webber et al. 2016). In that
assessment,  an effort was made to match the 2015 SMBKC stock assessment model to bridge a framework which 
provided greater flexibility and opportunity to evaluate model assumptions more fully.

## Assessment Methodology

This assessment model again uses the modeling framework Gmacs and is detailed in Appendix A.

## Model Selection and Evaluation

Five models were presented in the previous assessment. This year, four models are presented with the  reference model
being the same configuration as approved last year (Ianelli et al. 2017), two sensitivities are considered, one with a  different treatment of NMFS
bottom trawl survey (BTS) data using a geo-spatial model (VAST; Thorson and Barnett 2017; Appendix C). A second 
sensitivity was constructed which weights the
survey data more heavily. In addition to these sensitivities, we evaluated the impacts of adding new data 
to the reference model. In summary, the following lists the models presented and the naming convention  used:

1. **2017 Model**: the 2017 recommended model without any new data

2. **BTS**: adds in the 2018 bottom trawl survey (BTS) data

3. **BTS and pot**: as with previous but including the 2018 ADFG pot survey data (Model 16.0 or "reference case")

3. **VAST**: applies a geo-spatial delta-GLMM model (Thorson and Barnett 2017) to the BTS data which provides a different BTS index. See appendix B for 
  details and diagnostics. This is a preliminary examination as more work is needed to ensure options for the BTS CPUE data
   were specified appropriately.

3. **Fit survey**: an exploratory scenario that's the same as the reference model except the NMFS trawl survey is up-weighted 
4. by
   $\lambda^\text{NMFS}=$  and the ADF&G pot survey is 
   up-weighted by $\lambda^\text{ADFG}=$ . 

Note that SSC convention would label these (item 3 above) as model 16.0 (the
model first developed in that year). Since only a few models are presented here, for simplicity
we labeled model 16.0 as "reference" and for the others, we used the  simple naming convention
presented above.

## Results

### a. Sensitivity to new data
Results for scenarios are provided with comparisons to the 2017 model and sensitivity new data are shown in Figures
\ref{fig:surv1} and \ref{fig:surv2} with recruitment and spawning biomass shown in Figures \ref{fig:rec1} and \ref{fig:ssb1}, respectively. 
 The fits to survey CPUEs and spawning biomass show that the addition of new data results in more of a decline than
 in the 2017 assessment, especially with the addition of the pot survey. 

### b. Alternative NMFS bottom-trawl survey index
Results comparing model fits between the VAST model and the reference case show different time-series
of data and a different model fit (Figure \ref{fig:surv3}). The effect on spawning biomass suggests estimates were consistently
higher since 1990 compared to the reference model (Figure  \ref{fig:ssb2}).

### c. Effective sample sizes and weighting factors
Observed and estimated effective sample sizes are compared in Table \ref{tab:effn}. 
Data weighting factors, standard deviation of normalized residuals (SDNRs), and median absolute residual (MAR) are 
presented in Table \ref{tab:data_weighting}. The SDNR for the trawl survey
is acceptable at 1.66 in the reference model. Francis (2011) weighting was applied in 2017
but given the relatively few size
bins in this assessment, this application was suspended this year.  
The SDNRs for the pot surveys show a similar pattern in each of the
scenarios, but are much higher suggesting an inconsistency between 
the pot survey data and the model structure and other data
components. Rather than re-weighting, we chose to retain the values 
as specified, noting that down-weighting these data would effectively 
exclude the signal from this series. 
The MAR values for the trawl and pot surveys shows the same pattern among each of the
scenarios as the SDNR. The SDNR and MAR values for the trawl survey and pot survey size compositions were relatively good,
ranging from 0.54 to 0.73 for the reference case.
The SDNRs for the directed pot fishery and other size compositions were  similar to previous estimates.

### d. Parameter estimates
Model parameter estimates for each of the Gmacs scenarios are summarized in Tables \ref{tab:est_pars_ref},
\ref{tab:est_pars_vast}, and \ref{tab:est_pars_all}. These parameter
estimates are compared in Table \ref{tab:est_pars_all}. Negative log-likelihood values and management measures for each
of the model configurations are compared in Tables \ref{tab:management_quants} through \ref{tab:likelihood_components}.

There are some differences in parameter estimates among models as 
reflected in the log-likelihood components and the management quantities. 
The parameter estimates in the "fit survey" scenario differ the most, as expected, particularly the estimate of the ADF&G pot survey
catchability ($q$) (see Table \ref{tab:est_pars_all}). Also, the residuals for recruitment in the first size group are large for these
model runs, presumably because higher estimates of recruits in some years are required by the model
 to match the observed biomass trends. 

Selectivity estimates show some variability between models (Figure \ref{fig:selectivity}). Estimated recruitment is
variable over time for all models and in recent years is well below average (Figure \ref{fig:recruitment}).  Estimated
mature male biomass on 15 February also fluctuates considerably (Figure \ref{fig:mmb}). Estimated natural mortality each
year ($M_t$) is presented in Figure \ref{fig:M_t}.

### e. Evaluation of the fit to the data.
The model fits to total male ($\ge$ 90 mm CL) trawl survey biomass tend to miss the recent peak around 2010 and is
slightly above the 2017 value for the key sensitivities (Figures \ref{fig:trawl_survey_biomass}).  All of the models fit
the pot survey CPUE poorly (Figure \ref{fig:pot_survey_cpue}. For both surveys the standardized residuals tend to have
similar patterns with some improvement (generally) for the  VAST model (Figures \ref{fig:bts_resid_nmfs} and
\ref{fig:bts_resid_adfg}).

Fits to the size compositions for trawl survey, pot survey, and commercial observer data are reasonable but miss the largest
size category in some years (Figures \ref{fig:sc_pot}, \ref{fig:sc_pot_discarded}, and \ref{fig:sc_trawl_discarded}) 
for all scenarios. Representative
residual  plots of the composition data fits are generally poor (Figures \ref{fig:sc_res_ref} and \ref{fig:sc_res_fit_survey}).
The model fits to different types of retained and discarded catch values performed as expected given the assumed levels
of uncertainty on the input data (Figure  \ref{fig:fit_to_catch} ).

Unsurprisingly, the **Fit surveys** model fits the 
the NMFS survey biomass and ADF&G pot survey CPUE  data better but still has a similar residual pattern
(Figures \ref{fig:trawl_survey_biomass} and \ref{fig:pot_survey_cpue}). 
It is worth noting that that this scenario (included for exploratory purposes) resulted in worse SDNR and MAR values for the two abundance indices.

### f. Retrospective and historical analyses
This is only the second year a formal assessment model developed for this stock. As such, retrospective patterns and 
historical analyses relative to fisheries impacts are limited.

### g. Uncertainty and sensitivity analyses.
Estimated standard deviations of parameters and selected management measures for the models are summarized in
Tables \ref{tab:est_pars_ref}, \ref{tab:est_pars_vast}, and \ref{tab:est_pars_fit_survey} 
(compiled in Table \ref{tab:est_pars_all}). Probabilities for mature male biomass and OFL in 2017 are
presented in Section F.


### h. Comparison of alternative model scenarios.
The estimates of mature male biomass (Figure \ref{fig:mmb}), for the **Fit survey** sensitivity 
differs from the other models due to a low value for pot survey catchability being estimated (which tends to scale the population
estimate). This existng scenario results in a lower MMB from the mid-1980s
through to the late-1990s, and is again lower in the most recent 5 years. This scenario upweights both the trawl 
and pot surveys abundance indices  and represents a model run
that places greater emphasis on the abundance indices.

In summary, the use of the reference model for management purposes is preferred since it provides the best fit to the
data and is consistent with previous model specifications.  Research on alternative model specifications (e.g., natural
mortality variability) was limited this year. The VAST model may take better account of spatial
processes but requires more research to ensure it has been appropriately applied and the assumptions are reasonable.
Consequently, the reference model appears reasonable and appropriate for ACL and OFL  determinations for this stock in
2017. Nonetheless, the **Fit surveys** model, while difficult to statistically justify, portends a more dire stock status
(see below) and should highlight the caution needed in managing this resource.


# F. Calculation of the OFL and ABC
The overfishing level (OFL) is the fishery-related mortality biomass associated with fishing mortality $F_\mathit{OFL}$. The SMBKC stock is currently managed as Tier 4, and only a Tier 4 analysis is presented here. Thus given stock estimates or suitable proxy values of $B_\mathit{MSY}$ and $F_\mathit{MSY}$, along with two additional parameters $\alpha$ and $\beta$, $F_\mathit{OFL}$ is determined by the control rule
\begin{align}
    F_\mathit{OFL} &= 
    \begin{cases}
        F_\mathit{MSY}, &\text{ when } B/B_\mathit{MSY} > 1\\
        F_\mathit{MSY} \frac{\left( B/B_\mathit{MSY} - \alpha \right)}{(1 - \alpha)}, &\text{ when } \beta < B/B_\mathit{MSY} \le 1
    \end{cases}\\
    F_\mathit{OFL} &< F_\mathit{MSY} \text{ with directed fishery } F = 0 \text{ when } B/B_\mathit{MSY} \le \beta \notag
\end{align}
where $B$ is quantified as mature-male biomass (MMB) at mating with time of mating assigned a nominal date of 15
February. Note that as $B$ itself is a function of the fishing mortality $F_\mathit{OFL}$ (therefore numerical
approximation of $F_\mathit{OFL}$ is required). As implemented for this assessment, all calculations proceed according
to the model equations given in Appendix A. $F_\mathit{OFL}$ is taken to be full-selection fishing mortality in the
directed pot fishery and groundfish trawl and fixed-gear fishing mortalities set at their model geometric mean values
over years for which there are data-based estimates of bycatch-mortality biomass.

The currently recommended Tier 4 convention is to use the full assessment period, currently -
, to define a $B_\mathit{MSY}$ proxy in terms of average estimated MMB and to set $\gamma$ = 1.0 with
assumed stock natural mortality $M$ = 0.18 $\text{yr}^{-1}$ in setting the $F_\mathit{MSY}$ proxy value $\gamma M$. The
parameters $\alpha$ and $\beta$ are assigned their default values $\alpha$ = 0.10 and $\beta$ = 0.25. The
$F_\mathit{OFL}$, OFL, ABC, and MMB in 2018 for all scenarios are summarized in Table \ref{tab:management_quants}. The ABC
is 80% of the OFL.



# G. Rebuilding Analysis

This stock is not currently subject to a rebuilding plan. However, interpretation of the point estimate for the reference case
suggests that the mature male biomass is below 50% of $B_\mathit{MSY}$ but slightly above for the "VAST" model configuration (Table \ref{tab:management_quants} ).

# H. Data Gaps and Research Priorities

The following topics have been listed as areas where more research on SMBKC is needed:

  1. Growth increments and molting probabilities as a function of size.

  2. Trawl survey catchability and selectivities.

  3. Temporal changes in spatial distributions near the island.

  4. Natural mortality.

# I. Projections and outlook

The outlook for recruitment is pessimistic and the abundance relative to the proxy $B_\mathit{MSY}$ is low. 
The NMFS survey results in 2018 noted ocean conditions warmer than 
normal with an absence of a "cold pool" in the region. This could have detrimental effects on the SMBKC stocks and
should be carefully monitored. Relative to the impact of historical fishing, we again conducted a
 "dynamic-$B_0$" analysis. This procedure simply projects
the population based on estimated recruitment but removes the effect of fishing. For the reference case,
this suggests that the impact of fishing has reduced to stock to about
  % of what it would have been in the absence of
  fishing (Figure \ref{fig:dynB0}) . The other non-fishing contributors to the observed depleted stock trend (ignoring stock-recruit
  relationship) may reflect variable  survival rates due to environmental conditions and also range shifts. 

# J. Acknowledgements

We thank the Crab Plan Team and AFSC staff for reviewing an earlier draft of this report 
and Andre Punt for his input into refinements to the Gmacs model code.

# K. References

Alaska Department of Fish and Game (ADF&G). 2013. Crab observer training and deployment manual. Alaska Department of Fish and Game Shellfish Observer Program, Dutch Harbor. Unpublished.

Fournier, D.A., H.J. Skaug, J. Ancheta, J. Ianelli, A. Magnusson, M.N. Maunder, A. Nielsen, and J. Sibert. 2012. AD Model Builder: using automatic differentiation for statistical inference of highly parameterized complex nonlinear models. Optim. Methods Softw. 27:233-249.

Francis, R.I.C.C. 2011. Data weighting in statistical fisheries stock assessment models. Can. J. Fish. Aquat. Sci. 68: 1124-1138.


\newpage\clearpage

# Tables

\begin{table}[ht]
\centering
\caption{Observed proportion of crab by size class during the ADF\&G crab observer pot-lift sampling. Source: ADF\&G Crab Observer Database.} 
\label{tab:stage_cpue_1}
\begin{tabular}{lrrrrrr}
  \hline
  Year & Total pot lifts & Pot lifts sampled & Number of crab (90 mm+ CL) & Stage 1 & Stage 2 & Stage 3 \\
  \hline
  1990/91 & 26,264 &  10 &   150 & 0.113 & 0.393 & 0.493 \\
  1991/92 & 37,104 & 125 & 3,393 & 0.133 & 0.177 & 0.690 \\
  1992/93 & 56,630 &  71 & 1,606 & 0.191 & 0.268 & 0.542 \\
  1993/94 & 58,647 &  84 & 2,241 & 0.281 & 0.210 & 0.510 \\
  1994/95 & 60,860 & 203 & 4,735 & 0.294 & 0.271 & 0.434 \\
  1995/96 & 48,560 &  47 &   663 & 0.148 & 0.212 & 0.640 \\
  1996/97 & 91,085 &  96 &   489 & 0.160 & 0.223 & 0.618 \\
  1997/98 & 81,117 & 133 & 3,195 & 0.182 & 0.205 & 0.613 \\
  1998/99 & 91,826 & 135 & 1.322 & 0.193 & 0.216 & 0.591 \\
  \multicolumn{2}{l}{1999/00 - 2008/09} & \multicolumn{5}{c}{FISHERY CLOSED} \\
  2009/10 & 10,484 & 989   & 19,802 & 0.141 & 0.324 & 0.535 \\
  2010/11 & 29,356 & 2,419 & 45,466 & 0.131 & 0.315 & 0.553 \\
  2011/12 & 48,554 & 3,359 & 58,666 & 0.131 & 0.305 & 0.564 \\
  2012/13 & 37,065 & 2,841 & 57,298 & 0.141 & 0.318 & 0.541 \\
  \multicolumn{2}{l}{2013/14} & \multicolumn{5}{c}{FISHERY CLOSED} \\
  2014/15 & 10,133 & 895 & 9,906 & 0.094 & 0.228 & 0.679 \\
  2015/16 &  5,475 & 419 & 3,248 & 0.115 & 0.252 & 0.633 \\
  \multicolumn{2}{l}{2016/17} & \multicolumn{5}{c}{FISHERY CLOSED} \\
  \hline
\end{tabular}
\end{table}



\clearpage

\begin{table}[ht]
\centering
\caption{Fishery characteristics and update. Columns include the 1978/79 to 2015/16 directed St. Matthew Island blue king crab pot fishery. The Guideline Harvest Level (GHL) and Total Allowable Catch (TAC) are in millions of pounds. Harvest includes deadloss. Catch per unit effort (CPUE) in this table is simply the harvest number / pot lifts. The average weight is the harvest weight / harvest number in pounds. The average CL is the average of retained crab in mm from dockside sampling of delivered crab. Source: Fitch et al 2012; ADF\&G Dutch Harbor staff, pers. comm. Note that management (GHL) units are in pounds, for conserving space, conversion to tons is ommitted.} 
\label{tab:smbkc_fishery}
\begin{tabular}{lrrrrrrrr}
  \hline
  & & & \multicolumn{2}{c}{Harvest} & & & & \\
  \cline{4-5}
  Year & Dates & GHL/TAC & Crab & Pounds & Pot lifts & CPUE & avg wt & avg CL \\
  \hline
  1978/79 & 07/15 - 09/03 &  & 436,126 & 1,984,251 & 43,754 & 10 & 4.5 & 132.2 \\
  1979/80 & 07/15 - 08/24 &  &  52,966 &   210,819 &  9,877 &  5 & 4.0 & 128.8 \\
  1980/81 & 07/15 - 09/03 & \multicolumn{7}{c}{CONFIDENTIAL} \\
  1981/82 & 07/15 - 08/21 &  & 1,045,619 & 4,627,761 &  58,550 &  18 & 4.4 & NA \\
  1982/83 & 08/01 - 08/16 &  & 1,935,886 & 8,844,789 & 165,618 &  12 & 4.6 & 135.1 \\
  1983/84 & 08/20 - 09/06 & 8.0     & 1,931,990 & 9,454,323 & 133,944 &  14 & 4.9 & 137.2 \\
  1984/85 & 09/01 - 09/08 & 2.0-4.0 &   841,017 & 3,764,592 &  73,320 &  11 & 4.5 & 135.5 \\
  1985/86 & 09/01 - 09/06 & 0.9-1.9 &   436,021 & 2,175,087 &  46,988 &   9 & 5.0 & 139.0 \\
  1986/87 & 09/01 - 09/06 & 0.2-0.5 &   219,548 & 1,003,162 &  22,073 &  10 & 4.6 & 134.3 \\
  1987/88 & 09/01 - 09/05 & 0.6-1.3 &   227,447 & 1,039,779 &  28,230 &   8 & 4.6 & 134.1 \\
  1988/89 & 09/01 - 09/05 & 0.7-1.5 &   280,401 & 1,236,462 &  21,678 &  13 & 4.4 & 133.3 \\
  1989/90 & 09/01 - 09/04 & 1.7     &   247,641 & 1,166,258 &  30,803 &   8 & 4.7 & 134.6 \\
  1990/91 & 09/01 - 09/07 & 1.9     &   391,405 & 1,725,349 &  26,264 &  15 & 4.4 & 134.3 \\
  1991/92 & 09/16 - 09/20 & 3.2     &   726,519 & 3,372,066 &  37,104 &  20 & 4.6 & 134.1 \\
  1992/93 & 09/04 - 09/07 & 3.1     &   545,222 & 2,475,916 &  56,630 &  10 & 4.5 & 134.1 \\
  1993/94 & 09/15 - 09/21 & 4.4     &   630,353 & 3,003,089 &  58,647 &  11 & 4.8 & 135.4 \\
  1994/95 & 09/15 - 09/22 & 3.0     &   827,015 & 3,764,262 &  60,860 &  14 & 4.9 & 133.3 \\
  1995/96 & 09/15 - 09/20 & 2.4     &   666,905 & 3,166,093 &  48,560 &  14 & 4.7 & 135.0 \\
  1996/97 & 09/15 - 09/23 & 4.3     &   660,665 & 3,078,959 &  91,085 &   7 & 4.7 & 134.6 \\
  1997/98 & 09/15 - 09/22 & 5.0     &   939,822 & 4,649,660 &  81,117 &  12 & 4.9 & 139.5 \\
  1998/99 & 09/15 - 09/26 & 4.0     &   635,370 & 2,968,573 &  91,826 &   7 & 4.7 & 135.8 \\
  \multicolumn{2}{l}{1999/00 - 2008/09} & \multicolumn{7}{c}{FISHERY CLOSED} \\
  2009/10 & 10/15 - 02/01 & 1.17    &   103,376 &   460,859 &  10,697 &  10 & 4.5 & 134.9 \\
  2010/11 & 10/15 - 02/01 & 1.60    &   298,669 & 1,263,982 &  29,344 &  10 & 4.2 & 129.3 \\
  2011/12 & 10/15 - 02/01 & 2.54    &   437,862 & 1,881,322 &  48,554 &   9 & 4.3 & 130.0 \\
  2012/13 & 10/15 - 02/01 & 1.63    &   379,386 & 1,616,054 &  37,065 &  10 & 4.3 & 129.8 \\
  \multicolumn{2}{l}{2013/14} & \multicolumn{7}{c}{FISHERY CLOSED} \\
  2014/15 & 10/15 - 02/05 & 0.66    &    69,109 &   308,582 &  10,133 &   7 & 4.5 & 132.3 \\
  2015/16 & 10/19 - 11/28 & 0.41    &    24,076 &   105,010 &   5,475 &   4 & 4.4 & 132.6 \\
  \multicolumn{2}{l}{2016/17} & \multicolumn{7}{c}{FISHERY CLOSED} \\
  \multicolumn{2}{l}{2017/18} & \multicolumn{7}{c}{FISHERY CLOSED} \\
  \hline
\end{tabular}
\end{table}

\newpage

\begin{table}[ht]
\centering
\caption{NMFS EBS trawl-survey area-swept estimates of male crab abundance ($10^6$ crab) and male ($\ge90$ mm CL) biomass ($10^6$ lbs). Total number of captured male crab $\ge$ 90 mm CL is also given. Source: R. Foy, NMFS. The "+" refer to plus group.} 
\label{tab:stage_cpue_nmfs}
\begin{tabular}{lccccccccc}
  \hline
       & \multicolumn{5}{c}{Abundance}                       & & \multicolumn{2}{c}{Biomass} & \\
  \cline{2-6}\cline{8-9}
       & Stage-1     & Stage-2      & Stage-3   &       &    & & Total       &               & Number \\ 
  Year & (90-104 mm) & (105-119 mm) & (120+ mm) & Total & CV & & (90+ mm CL) & CV            & of crabs \\ 
  \hline
  1978  & 2.213 & 1.991 & 1.521 & 5.726 & 0.411 & & 15.064  & 0.394 & 157 \\
1979  & 3.061 & 2.281 & 1.808 & 7.150 & 0.472 & & 17.615  & 0.463 & 178 \\
1980  & 2.856 & 2.563 & 2.541 & 7.959 & 0.572 & & 22.017  & 0.507 & 185 \\
1981  & 0.483 & 1.213 & 2.263 & 3.960 & 0.368 & & 14.443  & 0.402 & 140 \\
1982  & 1.669 & 2.431 & 5.884 & 9.984 & 0.401 & & 35.763  & 0.344 & 271 \\
1983  & 1.061 & 1.651 & 3.345 & 6.057 & 0.332 & & 21.240  & 0.298 & 231 \\
1984  & 0.435 & 0.497 & 1.452 & 2.383 & 0.175 & & 8.976 & 0.179 & 105 \\
1985  & 0.379 & 0.376 & 1.117 & 1.872 & 0.216 & & 6.858 & 0.210 & 93  \\
1986  & 0.203 & 0.447 & 0.374 & 1.025 & 0.428 & & 3.124 & 0.388 & 46  \\
1987  & 0.325 & 0.631 & 0.715 & 1.671 & 0.302 & & 5.024 & 0.291 & 71  \\
1988  & 0.410 & 0.816 & 0.957 & 2.183 & 0.285 & & 6.963 & 0.252 & 81  \\
1989  & 2.169 & 1.154 & 1.786 & 5.109 & 0.314 & & 13.974  & 0.271 & 208 \\
1990  & 1.053 & 1.031 & 2.338 & 4.422 & 0.302 & & 14.837  & 0.274 & 170 \\
1991  & 1.147 & 1.665 & 2.233 & 5.046 & 0.259 & & 15.318  & 0.248 & 197 \\
1992  & 1.074 & 1.382 & 2.291 & 4.746 & 0.206 & & 15.638  & 0.201 & 220 \\
1993  & 1.521 & 1.828 & 3.276 & 6.626 & 0.185 & & 21.051  & 0.169 & 324 \\
1994  & 0.883 & 1.298 & 2.257 & 4.438 & 0.187 & & 14.416  & 0.176 & 211 \\
1995  & 1.025 & 1.188 & 1.741 & 3.953 & 0.187 & & 12.574  & 0.178 & 178 \\
1996  & 1.238 & 1.891 & 3.064 & 6.193 & 0.263 & & 20.746  & 0.241 & 285 \\
1997  & 1.165 & 2.228 & 3.789 & 7.182 & 0.367 & & 24.084  & 0.337 & 296 \\
1998  & 0.660 & 1.661 & 2.849 & 5.170 & 0.373 & & 17.586  & 0.355 & 243 \\
1998  & 0.223 & 0.222 & 0.558 & 1.003 & 0.192 & & 3.515 & 0.182 & 52  \\
2000  & 0.282 & 0.285 & 0.740 & 1.307 & 0.303 & & 4.623 & 0.310 & 61  \\
2001  & 0.419 & 0.502 & 0.938 & 1.859 & 0.243 & & 6.242 & 0.245 & 91  \\
2002  & 0.111 & 0.230 & 0.640 & 0.981 & 0.311 & & 3.820 & 0.320 & 38  \\
2003  & 0.449 & 0.280 & 0.465 & 1.194 & 0.399 & & 3.454 & 0.336 & 65  \\
2004  & 0.247 & 0.184 & 0.562 & 0.993 & 0.369 & & 3.360 & 0.305 & 48  \\
2005  & 0.319 & 0.310 & 0.501 & 1.130 & 0.403 & & 3.620 & 0.371 & 42  \\
2006  & 0.917 & 0.642 & 1.240 & 2.798 & 0.339 & & 8.585 & 0.334 & 126 \\
2007  & 2.518 & 2.020 & 1.193 & 5.730 & 0.420 & & 14.266  & 0.385 & 250 \\
2008  & 1.352 & 0.801 & 1.457 & 3.609 & 0.289 & & 10.261  & 0.284 & 167 \\
2009  & 1.573 & 2.161 & 1.410 & 5.144 & 0.263 & & 13.892  & 0.256 & 251 \\
2010  & 3.937 & 3.253 & 2.458 & 9.648 & 0.544 & & 24.539  & 0.466 & 388 \\
2011  & 1.800 & 3.255 & 3.207 & 8.263 & 0.587 & & 24.099  & 0.558 & 318 \\
2012  & 0.705 & 1.970 & 1.808 & 4.483 & 0.361 & & 13.669  & 0.339 & 193 \\
2013  & 0.335 & 0.452 & 0.807 & 1.593 & 0.215 & & 5.043 & 0.217 & 74  \\
2014  & 0.723 & 1.627 & 1.809 & 4.160 & 0.503 & & 13.292  & 0.449 & 181 \\
2015  & 0.992 & 1.269 & 1.979 & 4.240 & 0.774 & & 12.958  & 0.770 & 153 \\
2016  & 0.535 & 0.660 & 1.178 & 2.373 & 0.447 & & 7.685 & 0.393 & 108 \\
2017  & 0.091 & 0.323 & 0.663 & 1.077 & 0.657 & & 3.955 & 0.600 & 42  \\
2018  & 0.154 & 0.232 & 0.660 & 1.047 & 0.298 & & 3.816 & 0.281 & 62  \\
  \hline
\end{tabular}
\end{table}


\begin{table}[ht]
\centering
\caption{Size-class and total CPUE (90+ mm CL) with estimated CV and total number of captured crab (90+ mm CL) 
from the 96 common stations surveyed during the ADF\&G SMBKC pot surveys. Source: ADF\&G.} 
\label{tab:stage_cpue}
\begin{tabular}{lcccrrr}
  \hline
       & Stage-1     & Stage-2      & Stage-3   &            &    & \\ 
  Year & (90-104 mm) & (105-119 mm) & (120+ mm) & Total CPUE & CV & Number of crabs \\ 
  \hline
  1995 & 1.919 & 3.198 & 6.922 & 12.042 & 0.13 & 4624 \\ 
  1998 & 0.964 & 2.763 & 8.804 & 12.531 & 0.06 & 4812 \\ 
  2001 & 1.266 & 1.737 & 5.487 & 8.477 & 0.08 & 3255 \\ 
  2004 & 0.112 & 0.414 & 1.141 & 1.667 & 0.15 & 640 \\ 
  2007 & 1.086 & 2.721 & 4.836 & 8.643 & 0.09 & 3319 \\ 
  2010 & 1.326 & 3.276 & 5.607 & 10.209 & 0.13 & 3920 \\ 
  2013 & 0.878 & 1.398 & 3.367 & 5.643 & 0.19 & 2167 \\ 
  2015 & 0.198 & 0.682 & 1.924 & 2.805 & 0.18 & 1077 \\ 
  2016 & 0.198 & 0.456 & 1.724 & 2.378 & 0.19 & 777 \\ 
  2017 & 0.177 & 0.429 & 1.083 & 1.689 & 0.25 & 643 \\ 
  2018 & 0.076 & 0.161 & 0.508 & 0.745 & 0.14 &   286\\
   \hline
\end{tabular}
\end{table}




\begin{table}[ht]
\centering
\caption{Observed and input sample sizes for observer data from the directed pot fishery, the NMFS trawl survey, and the ADF\&G pot survey.} 
\label{tab:effn}
\begin{tabular}{lccccccc}
  \hline
  & \multicolumn{3}{c}{Number measured} & \multicolumn{3}{c}{Input sample sizes} \\
  \cline{2-4}\cline{6-8}
  Year & Observer pot & NMFS trawl & ADF\&G pot & & Observer pot & NMFS trawl & ADF\&G pot \\ 
  \hline
  1978 &              & 157        &          & &              & 50         & \\
  1979 &              & 178        &          & &              & 50         & \\
  1980 &              & 185        &          & &              & 50         & \\
  1981 &              & 140        &          & &              & 50         & \\
  1982 &              & 271        &          & &              & 50         & \\
  1983 &              & 231        &          & &              & 50         & \\
  1984 &              & 105        &          & &              & 50         & \\
  1985 &              &  93        &          & &              & 46.5       & \\
  1986 &              &  46        &          & &              & 23         & \\
  1987 &              &  71        &          & &              & 35.5       & \\
  1988 &              &  81        &          & &              & 40.5       & \\
  1989 &              & 208        &          & &              & 50         & \\
  1990 &  150         & 170        &          & & 15           & 50         & \\
  1991 &  3393        & 197        &          & & 25           & 50         & \\
  1992 &  1606        & 220        &          & & 25           & 50         & \\
  1993 &  2241        & 324        &          & & 25           & 50         & \\
  1994 &  4735        & 211        &          & & 25           & 50         & \\
  1995 &  663         & 178        &  4624    & & 25           & 50         & 100 \\
  1996 &  489         & 285        &          & & 25           & 50         & \\
  1997 &  3195        & 296        &          & & 25           & 50         & \\
  1998 &  1323        & 243        &  4812    & & 25           & 50         & 100 \\
  1999 &              &  52        &          & &              & 26         & \\
  2000 &              &  61        &          & &              & 30.5       & \\
  2001 &              &  91        &  3255    & &              & 45.5       & 100 \\
  2002 &              &  38        &          & &              & 19         & \\
  2003 &              &  65        &          & &              & 32.5       & \\
  2004 &              &  48        &  640     & &              & 24         & 100 \\
  2005 &              &  42        &          & &              & 21         & \\
  2006 &              & 126        &          & &              & 50         & \\
  2007 &              & 250        &  3319    & &              & 50         & 100 \\
  2008 &              & 167        &          & &              & 50         & \\
  2009 &  19802       & 251        &          & & 50           & 50         & \\
  2010 &  45466       & 388        &  3920    & & 50           & 50         & 100 \\
  2011 &  58667       & 318        &          & & 50           & 50         & \\
  2012 &  57282       & 193        &          & & 50           & 50         & \\
  2013 &              &  74        &  2167    & &              & 37         & 100 \\
  2014 &  9906        & 181        &          & & 50           & 50         & \\
  2015 &  3248        & 153        &  1077    & & 50           & 50         & 100 \\
  2016 &              & 108        &   777    & &              & 50         & 100 \\
  2017 &              & 42         &   643    & &              & 21         & 100 \\
  2018 &              & 62         &   286    & &              & 31         & 100 \\
  \hline
\end{tabular}
\end{table}






```
## Error: <text>:6:37: unexpected ','
## 5:        #grep("log_fbar", x$names),
## 6:        grep("log_slx_pars", x$names),
##                                        ^
```















\newpage\clearpage

# Figures
\newpage\clearpage

![Distribution of blue king crab (*Paralithodes platypus*) in the Gulf of Alaska, Bering Sea, and Aleutian Islands
waters (shown in blue).\label{fig:distribution}](figure/Fig1.png)

![King crab Registration Area Q (Bering Sea).\label{fig:registration_area}](figure/Fig2.png)

<!--Data extent  -->



<!--  stations -->

![Trawl and pot-survey stations used in the SMBKC stock assessment.\label{fig:stations}](figure/Fig3.png)

<!--  -->

![Catches (in numbers) of male blue king crab $/ge$ 90 mm CL from the 2012-2017 NMFS trawl-survey at the 56 stations used
to assess the SMBKC stock. Note that the area north of St. Matthew Island, which often shows large catches of crab
at station R-24 is not covered in the ADF&G pot-survey data used in the
assessment.\label{fig:catch181}](../figure/CrabN_Station.png)

<!-- Survey NMFS -->



<!--  Survey pot-->



<!--  -->




<!--  -->



\clearpage
<!--  -->


```
## Error in .get_cpue_df(M[3:4]): could not find function ".get_cpue_df"
```

```
## Error in filter(p.df, fleet == "NMFS Trawl"): object 'p.df' not found
```

<!--  -->



\clearpage
<!--  -->



\clearpage
<!--  -->


```
## Error in A[[i]]: subscript out of bounds
```

<!--  -->



\clearpage
<!--  -->



\clearpage
<!--  -->


```
## Error in .get_cpue_df(M[mod_scen]): could not find function ".get_cpue_df"
```

```
## Error in filter(p.df, fleet == "NMFS Trawl"): object 'p.df' not found
```

<!--  -->



\clearpage
<!--  -->



<!--  -->



\clearpage



<!--  -->



\clearpage



<!--  -->



\clearpage



<!-- \clearpage -->



\clearpage



\clearpage

\newpage\clearpage

# Appendix A: Model Description

## 1. Introduction

The model has been specified 

## 2. Model Population Dynamics

Within the model, the beginning of the 


The natural mortality each season $t$ and year $y$ is
\begin{equation}
    M_{t,y} = \bar{M} \tau_t + \delta_y^M \text{ where } \delta_y^M \sim \mathcal{N} \left( 0, \sigma_M^2 \right)
\end{equation}
Fishing mortality by year $y$ and season $t$ is denoted $F_{t,y}$ and calculated as
\begin{equation}
    F_{t,y} = F_{t,y}^\text{df} + F_{t,y}^\text{tb} + F_{t,y}^\text{fb}
\end{equation}
where $F_{t,y}^\text{df}$ is the fishing mortality associated with the directed fishery, $F_{t,y}^\text{tb}$ is the fishing mortality associated with the trawl bycatch fishery, $F_{t,y}^\text{fb}$ is the fishing mortality associated with the fixed bycatch fishery. Each of these are derived as
\begin{align}
    F_{t,y}^\text{df} &= \bar{F}^\text{df} + \delta^\text{df}_{t,y} \quad \text{where} \quad \delta^\text{df}_{t,y} \sim \mathcal{N} \left( 0, \sigma^2_\text{df} \right), \notag\\
    F_{t,y}^\text{tb} &= \bar{F}^\text{tb} + \delta^\text{tb}_{t,y} \quad \text{where} \quad \delta^\text{df}_{t,y} \sim \mathcal{N} \left( 0, \sigma^2_\text{tb} \right), \notag\\
    F_{t,y}^\text{fb} &= \bar{F}^\text{fb} + \delta^\text{fb}_{t,y} \quad \text{where} \quad \delta^\text{df}_{t,y} \sim \mathcal{N} \left( 0, \sigma^2_\text{fb} \right),
\end{align}
where $\delta^\text{df}_{t,y}$, $\delta^\text{tb}_{t,y}$, and $\delta^\text{fb}_{t,y}$ are the fishing mortality deviations for each of the fisheries, each season $t$ during each year $y$, $\bar{F}^\text{df}$, $\bar{F}^\text{tb}$, and $\bar{F}^\text{fb}$ are the average fishing mortalities for each fishery. The total mortality $Z_{l,t,y}$ represents the combination of natural mortality $M_{t,y}$ and fishing mortality $F_{t,y}$ during season $t$ and year $y$
\begin{equation}
    \boldsymbol{Z}_{t,y} = Z_{l,t,y} = M_{t,y} + F_{t,y}.
\end{equation}
The survival matrix $\boldsymbol{S}_{t,y}$ during season $t$ and year $y$ is
\begin{equation}
  \boldsymbol{S}_{t,y} = \left[ \begin{array}{ccc}
    1-e^{-Z_{1,t,y}} & 0 & 0 \\
    0 & 1-e^{-Z_{2,t,y}} & 0 \\
    0 & 0 & 1-e^{-Z_{3,t,y}} \end{array} \right].
\end{equation}

The basic population dynamics underlying Gmacs can thus be described as
\begin{align}
    \boldsymbol{n}_{t+1,y} &= \boldsymbol{S}_{t,y} \boldsymbol{n}_{t,y}, &\text{ if } t<5 \notag\\
    \boldsymbol{n}_{t,y+1} &= \boldsymbol{G} \boldsymbol{S}_{t,y} \boldsymbol{n}_{t,y} + \boldsymbol{r}_{t,y} &\text{ if } t=5.
\end{align}


## 3. Model Data

Data inputs used in model estimation are listed in Table \ref{tab:model_data}.
\begin{table}[ht]
\centering
\caption{Data inputs used in model estimation.} 
\label{tab:model_data}
\begin{tabular}{lll}
  \hline
  Data & Years & Source \\
  \hline
  Directed pot-fishery retained-catch number & 1978/79 - 1998/99 & Fish tickets \\
  (not biomass) & 2009/10 - 2015/16 & (fishery closed 1999/00 - 2008/09 and 2016/17)\\
  \hline
  Groundfish trawl bycatch biomass & 1992/93 - 2016/17 & NMFS groundfish observer program \\
  \hline
  Groundfish fixed-gear bycatch biomass & 1992/93 - 2016/17 & NMFS groundfish observer program \\
  \hline
  NMFS trawl-survey biomass index & & \\
  (area-swept estimate) and CV & 1978-2018 & NMFS EBS trawl survey \\
  \hline
  ADF\&G pot-survey abundance index & & \\
  (CPUE) and CV & 1995-2017 & ADF\&G SMBKC pot survey \\
  \hline
  NMFS trawl-survey stage proportions & & \\
  and total number of measured crab & 1978-2018 & NMFS EBS trawl survey \\
  \hline
  ADF\&G pot-survey stage proportions & & \\
  and total number of measured crab & 1995-2017 & ADF\&G SMBKC pot survey \\
  \hline
  Directed pot-fishery stage proportions & 1990/91 - 1998/99 & ADF\&G crab observer program \\
  and total number of measured crab & 2009/10 - 2015/16 & (fishery closed 1999/00 - 2008/09 and 2016/17) \\
  \hline
\end{tabular}
\end{table}


## 4. Model Parameters

Table \ref{tab:fixed_pars} lists fixed (externally determined) parameters used in model computations. In all scenarios, the stage-transition matrix is
\begin{equation}
  \label{eq:size_transition}
  \boldsymbol{G} =
  \left[ \begin{array}{ccc}
    0.2 & 0.7 & 0.1 \\
    0 & 0.4 & 0.6 \\
    0 & 0 & 1 \end{array} \right]
\end{equation}
which is the combination of the growth matrix and molting probabilities.
\begin{table}[ht]
\centering
\caption{Fixed model parameters for all scenarios.} 
\label{tab:fixed_pars}
\begin{tabular}{lccl}
  \hline
  Parameter & Symbol & Value & Source/rationale \\
  \hline
  Trawl-survey catchability & $q$ & 1.0 & Default \\
  Natural mortality & $M$ & 0.18 $\text{yr}^{-1}$ & NPFMC (2007) \\
  Size transition matrix & $\boldsymbol{G}$ & Equation \ref{eq:size_transition} & Otto and Cummiskey (1990) \\
  Stage-1 and stage-2 & $w_{1}$, $w_{2}$ & 0.7, 1.2 kg & Length-weight equation (B. Foy, NMFS) \\
  mean weights & & & applied to stage midpoints \\
  Stage-3 mean weight & $w_{3,y}$ & Depends on year & Fishery reported average retained weight \\
  & & Table \ref{tab:length_weight} & from fish tickets, or its average, and \\
  & & & mean weights of legal males \\
  Recruitment SD & $\sigma_R$ & 1.2 & High value \\
  Natural mortality SD & $\sigma_M$ & 10.0 & High value (basically free parameter) \\
  Directed fishery & & 0.2 & 2010 Crab SAFE \\
  handling mortality & & & \\
  Groundfish trawl & & 0.8 & 2010 Crab SAFE \\
  handling mortality & & & \\
  Groundfish fixed-gear & & 0.5 & 2010 Crab SAFE \\
  handling mortality & & & \\
  \hline
\end{tabular}
\end{table}

Estimated parameters are listed in Table \ref{tab:bounds_pars} and include an estimated natural mortality deviation parameter in 1998/99 ($\delta^M_{1998}$) assuming an anomalous mortality event in that year, as hypothesized by Zheng and Kruse (2002), with natural mortality otherwise fixed at 0.18 $\text{yr}^{-1}$.
\begin{table}[ht]
\centering
\caption{The lower bound (LB), upper bound (UB), initial value, prior, and estimation phase for each estimated model parameter.} 
\label{tab:bounds_pars}
\begin{tabular}{lrrrlr}
  \hline
  Parameter & LB & Initial value & UB & Prior & Phase \\ 
  \hline
  Average recruitment $\log (\bar{R})$ & -7 & 10.0 & 20 & Uniform(-7,20) & 1 \\ 
  Stage-1 initial numbers $\log (n^0_1)$ & 5 & 14.5 & 20 & Uniform(5,20) & 1 \\ 
  Stage-2 initial numbers $\log (n^0_2)$ & 5 & 14.0 & 20 & Uniform(5,20) & 1 \\ 
  Stage-3 initial numbers $\log (n^0_3)$ & 5 & 13.5 & 20 & Uniform(5,20) & 1 \\ 
  ADF\&G pot survey catchability $q$ & 0 & 3.0 & 5 & Uniform(0,5) & 1 \\ 
  Stage-1 directed fishery selectivity 1978-2008 & 0 & 0.4 & 1 & Uniform(0,1) & 3 \\ 
  Stage-2 directed fishery selectivity 1978-2008 & 0 & 0.7 & 1 & Uniform(0,1) & 3 \\ 
  Stage-1 directed fishery selectivity 2009-2017 & 0 & 0.4 & 1 & Uniform(0,1) & 3 \\ 
  Stage-2 directed fishery selectivity 2009-2017 & 0 & 0.7 & 1 & Uniform(0,1) & 3 \\ 
  Stage-1 NMFS trawl survey selectivity & 0 & 0.4 & 1 & Uniform(0,1) & 4 \\ 
  Stage-2 NMFS trawl survey selectivity & 0 & 0.7 & 1 & Uniform(0,1) & 4 \\ 
  Stage-1 ADF\&G pot survey selectivity & 0 & 0.4 & 1 & Uniform(0,1) & 4 \\ 
  Stage-2 ADF\&G pot survey selectivity & 0 & 0.7 & 1 & Uniform(0,1) & 4 \\ 
  Natural mortality deviation during 1998 $\delta^M_{1998}$ & -3 & 0.0 & 3 & Normal(0, $\sigma^2_M$) & 4 \\ 
  Recruitment deviations $\delta^R_y$ & -7 & 0.0 & 7 & Normal(0, $\sigma_R^2$) & 3 \\ 
  Average directed fishery fishing mortality $\bar{F}^\text{df}$                 & -  & 0.2 & - & - & 1 \\
  Average trawl bycatch fishing mortality $\bar{F}^\text{tb}$                 & -  & 0.001 & - & - & 1 \\
  Average fixed gear bycatch fishing mortality $\bar{F}^\text{fb}$                 & -  & 0.001 & - & - & 1 \\
  \hline
\end{tabular}
\end{table}


## 5. Model Objective Function and Weighting Scheme

The objective function consists of the sum of several "negative log-likelihood" terms characterizing the hypothesized error structure of the principal data inputs (Table \ref{tab:likelihood_components}). A lognormal distribution is assumed to characterize the catch data and is modelled as
\begin{align}
  \sigma_{t,y}^\text{catch} &= \sqrt{\log \left( 1 + \left( \mathit{CV}_{t,y}^\text{catch} \right)^2 \right)}\\
  \delta_{t,y}^\text{catch} &= \mathcal{N} \left( 0, \left( \sigma_{t,y}^\text{catch} \right)^2 \right)
\end{align}
where $\delta_{t,y}^\text{catch}$ is the residual catch. The relative abudance data is also assumed to be lognormally distributed
\begin{align}
  \sigma_{t,y}^\text{I} &= \frac{1}{\lambda} \sqrt{\log \left( 1 + \left( \mathit{CV}_{t,y}^\text{I} \right)^2 \right)}\\
  \delta_{t,y}^\text{I} &= \log \left( I^\text{obs} / I^\text{pred} \right) / \sigma_{t,y}^\text{I} + 0.5 \sigma_{t,y}^\text{I}
\end{align}
and the likelihood is
\begin{equation}
  \sum \log \left( \delta_{t,y}^\text{I} \right) + \sum 0.5 \left( \sigma_{t,y}^\text{I} \right)^2
\end{equation}

AMAK calculates standard deviation of the normalised residual (SDNR) values
and median of the absolute residual (MAR) values for all abundance indices and
size compositions to help the user come up with resonable likelihood weights.
For an abundance data set to be well fitted, the SDNR should not be much
greater than 1 (a value much less than 1, which means that the data set is
fitted better than was expected, is not a cause for concern). What is meant by
"much greater than 1" depends on $m$ (the number of years in the data set).
Francis (2011) suggests upper limits of 1.54, 1.37, and 1.26 for $m$ = 5, 10,
and 20, respectively. Although an SDNR not much greater than 1 is a necessary
condition for a good fit, it is not sufficient. It is important to plot the
observed and expected abundances to ensure that the fit is good.

AMAK also calculates Francis weights for each of the size composition data
sets supplied (Francis 2011). If the user wishes to use the Francis iterative
re-weighting method, first the weights applied to the abundance indices should
be adjusted by trial and error until the SDNR (and/or MAR) are adequte. Then
the Francis weights supplied by Gmacs should be used as the new likelihood
weights for each of the size composition data sets the next time the model is
run. The user can then iteratively adjust the abudance index and size
composition weights until adequate SDNR (and/or MAR) values are achieved,
given the Francis weights.


## 6. Estimation

The model was implemented using the software AD Model Builder (Fournier et al.
2012), with parameter estimation by minimization of the model objective
function using automatic differentiation. Parameter estimates and standard
deviations provided in this document are AD Model Builder reported values
assuming maximum likelihood theory asymptotics.

