#!/usr/bin/env python3

# Sample usage:
# get_polygon.py --firename='ca-glass' --timestamp='20201004_091800' \
#                --transfer_mode='wget' --outdir='./out'

from __future__ import print_function
import logging
import grpc
import get_polygon_pb2
import get_polygon_pb2_grpc
import sys
import argparse
import subprocess
import os

if "CLOUDFIRE_SERVER" in os.environ:
    cloudfire_server= os.environ['CLOUDFIRE_SERVER']
else:
    cloudfire_server='worldgen.cloudfire.io'

parser = argparse.ArgumentParser()
parser.add_argument("--firename", required = True, nargs='?' )
parser.add_argument("--timestamp", required = True, nargs='?' )
parser.add_argument("--transfer_mode", required = False, default='wget', nargs='?' )
parser.add_argument("--outdir", required = False, default='./out', nargs='?' )

args = parser.parse_args()

firename = args.firename
timestamp = args.timestamp
transfer_mode = args.transfer_mode
outdir = args.outdir

suffices = [ 'shp', 'shx', 'dbf', 'prj' ]
def run():
    with grpc.insecure_channel(cloudfire_server + ':50054') as channel:
        stub = get_polygon_pb2_grpc.GetPolygonStub(channel)
        print ( 'firename', firename )
        print ( 'timestamp', timestamp )
        print ( 'transfer_mode', transfer_mode )

        response = stub.GetDomainData(get_polygon_pb2.Request( firename = firename, timestamp = timestamp, transfer_mode = transfer_mode ) )
        print(response.fileloc)

# match is not available prior to python 3.10, so don't use this:
#        match transfer_mode:
#            case 'cp':
#                command='cp -f ' + response.fileloc + '* ' + outdir + '/'
#                print(command)
#                proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
#                proc.wait()
#
#            case 'scp':
#                command='scp -v ' + response.fileloc + '.* ' + outdir + '/'
#                print(command)
#                proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
#                proc.wait()
#
#            case 'wget':
#               for suffix in suffices:
#                   command='wget -q -r -np -nH --cut-dirs=2 -R "index.html*" -P ' + outdir + ' ' + response.fileloc + '.' + suffix
#                   proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
#                   proc.wait()
#
#            case _:
#                print( "Specify --transfer_mode as 'cp', 'scp', or 'wget' ")

        if transfer_mode == 'cp':
            command='cp -f ' + response.fileloc + '* ' + outdir + '/'
            print(command)
            proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
            proc.wait()

        if transfer_mode == 'scp':
            command='scp -v ' + response.fileloc + '.* ' + outdir + '/'
            print(command)
            proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
            proc.wait()

        if transfer_mode == 'wget':
           for suffix in suffices:
               command='wget -q -r -np -nH --cut-dirs=2 -R "index.html*" -P ' + outdir + ' ' + response.fileloc + '.' + suffix
               proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
               proc.wait()

if __name__ == '__main__':
    logging.basicConfig()
    run()
