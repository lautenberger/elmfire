#!/usr/bin/env python3

import math
import sys

def Vback_over_Vhead(LoW):
    return (LoW - math.sqrt(LoW**2 - 1) ) / (LoW + math.sqrt(LoW**2 - 1) )

LoW=float(sys.argv[1])
Vhead=float(sys.argv[2])
Dt=int(sys.argv[3])
tstop=int(sys.argv[4])
yign=float(sys.argv[5])
outdir=sys.argv[6]

Ny=1000

Vback = Vhead * Vback_over_Vhead(LoW)
Vflank = 0.5 * (Vhead + Vback) / LoW

t=0
while t < tstop:
    havefirstpoint=False
    t = t + Dt
    a = Vflank * t
    b = a * LoW
    c = Vhead * t - b
    ycen = yign + c
    Dy = b / Ny

    fn = outdir + '/ellipse_' + str(t) + '.csv'
    f = open(fn,'w')
    f.write('id,gm\n')
    linestr='0,"LINESTRING ('

    y = ycen - b
    while y <= ycen + b:
        arg = max(1 - ((y-ycen)/b)**2,0.)
        x = a * math.sqrt(arg)
        if not havefirstpoint:
           firstpoint = str(x) + ' ' + str(y)
           havefirstpoint=True
        linestr = linestr + str(x) + ' ' + str(y) + ', '
        y += Dy

    y = ycen + b
    while y >= ycen - b:
        arg = max(1 - ((y-ycen)/b)**2,0.)
        x = -a * math.sqrt(arg)
        linestr = linestr + str(x) + ' ' + str(y) + ', '
        y -= Dy

    linestr = linestr + firstpoint + ')"\n'
    f.write(linestr)
    f.close

exit()
