# Pacific cod run details
On entry into this directory, tmsm (or some other simulation) need to write `pcod_msm.dat` as input
file to 
` amakl_rw.tpl `

The work is handled by ` restart.bat ` which re-initializes the datafile to the hindcast and then by
` pcod_assess.bat ` which runs the program ` amakl_rw.tpl `
and ` amakl.tpl 
However, ` amakl.tpl ` will fit a model to ` pcod.dat ` as generated from ` amakl_rw ` . The sequence is as follows

Finally, ` arcit.bat ` stores the assessment run for that year in subdirector ` arc `

Summary steps are thus:
  1. running the read-write code (using last year's datafile, new data from tmsm)
  2. creates new data file and a new control file
  3. run amakl with these files
  4. edit and rename for the subsequent year


| RW  =>  Assmnt  => 
