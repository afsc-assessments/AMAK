#!/bin/bash
# set modn=%1
# amak -ind ..\examples\atka\%1.ctl -nox -iprint 100
for i in `seq 15 0 `;
do
  awk -v rrr=$i 'NR==12{print rrr} NR!=12 {print $0}' mod16.0b.ctl >amak.dat
  amak -nox -iprint 200
  cp For_R.rep retro/r_$i.rep
  cp cum_NLL.rep retro/r_NLL_$i.rep
	cp amak.std retro/r_$i.std
	cp amak.bar retro/r_$i.bar
done    
