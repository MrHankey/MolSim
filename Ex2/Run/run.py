from __future__ import print_function

import os
import subprocess
import shutil

TEMPERATURE = 0.8
RHO = 0.05
STEPSIZE = 0.05
REPETITIONS = 2

while RHO <= 1.0:
    for i in range(1, REPETITIONS):
        f15 = open('fort.15', 'w')

        print('ibeg  nequil  nprod  nsamp', file=f15)
        print('0     1000   1000      1', file=f15)
        print('dr', file=f15)
        print('0.09', file=f15)
        print('ndispl', file=f15)
        print('50', file=f15)
        print('npart    temp    RHO', file=f15)
        print('500     ' + str(TEMPERATURE) + "  " + str(RHO), file=f15)

        f15.close()

        #os.rename("lj.res", "fort.11")
        #os.rename("lj.model", "fort.25")

        shutil.copyfile("lj.res", "fort.11")
        shutil.copyfile("lj.model", "fort.25")
        out_file = open('out', 'w')
        subprocess.call(["time", "../Source/mc_nvt"], stdout=out_file)
        out_file.close()

        os.rename("fort.21", "lj.res")
        os.rename("fort.22", "movie.pdb")
        os.rename("fort.66", "lj.energy")
        os.rename("fort.67", "lj.pressure")
        #os.remove("fort.*")

    RHO = RHO + STEPSIZE

