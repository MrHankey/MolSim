from __future__ import print_function

import glob
import os
import subprocess
import shutil

import numpy as np
import matplotlib.pyplot as plt

TEMPERATURE = 0.728
RHO = 0.8442

#RHOS = [0.05, 0.1, 0.2, 0.3, 0.5, 0.6, 0.7, 1.0]
RHOS = [0.8442]
#TEMPERATURES = [0.4, 0.8, 1.0, 1.5, 2.0]
TEMPERATURES = [0.728]

for RHO in RHOS:
    for TEMPERATURE in TEMPERATURES:
        print ('Rho = ' + str(RHO) + ' Temp = ' + str(TEMPERATURE))
        f15 = open('fort.15', 'w')

        print('ibeg , delt  tmax  tequil nsamp ', file=f15)
        print('0     0.01  10.0   0.00  1', file=f15)
        print('npart temp rho rcp iseed', file=f15)
        print('108 ' + str(TEMPERATURE) + " " + str(RHO) + " 2.5 123456", file=f15)
        print('scale   temp     verlet', file=f15)
        print('.true.  ' + str(TEMPERATURE) + '  .true.', file=f15)
        print('iout gr      iout4', file=f15)
        print('33   500000    36', file=f15)

        f15.close()

        #os.rename("lj.res", "fort.11")
        #os.rename("lj.model", "fort.25")

        shutil.copyfile("lj.res", "fort.11")
        #shutil.copyfile("lj.model", "fort.25")
        out_file = open('out', 'w')
        subprocess.call(["time", "../Source/MD"], stdout=out_file)
        out_file.close()

        os.rename("fort.21", "lj.res")
        #os.rename("fort.22", "movie.pdb")
        os.rename("fort.66", "lj.prt")
        #os.rename("fort.67", "lj.pressure_" + str(TEMPERATURE) + "_" + str(RHO))
        #map(os.remove, glob.glob("fort.*"))

data1 = np.loadtxt('lj.prt')
#data2 = np.loadtxt('lj.res')

plt.figure(1)
plt.plot(data1[:, 0], data1[:, 3], label='en_tot')
plt.xlim([0, len(data1[:,1])])

plt.figure(2)
plt.plot(data1[:, 0], data1[:, 3], label='en_tot')
plt.plot(data1[:, 0], data1[:, 4], label='en_pot')
plt.plot(data1[:, 0], data1[:, 5], label='en_kin')
plt.plot(data1[:, 0], data1[:, 6], label='vir')
plt.legend()
plt.xlim([0, len(data1[:,1])])

plt.show()

