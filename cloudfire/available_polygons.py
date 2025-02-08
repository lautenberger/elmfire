#!/usr/bin/env python3
#
# Sample usage for listing historical fires (by year):
#./available_polygons.py --list='fires' --active=False \
#                        --year=2020
#
# Sample usage for listing timestamps:
#./available_polygons.py --list='timestamps' --active=False \
#                        --year=2020 --firename='ca-glass'

from __future__ import print_function
import logging
import grpc
import available_polygons_pb2
import available_polygons_pb2_grpc
import sys
import argparse
import os

if "CLOUDFIRE_SERVER" in os.environ:
    cloudfire_server= os.environ['CLOUDFIRE_SERVER']
else:
    cloudfire_server='worldgen.cloudfire.io'

valid_historical_years_years = ["2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"]

def printerr(msg):
  print(msg)
  exit(1)

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

parser = argparse.ArgumentParser()
parser.add_argument("--active", required = False, default=True, nargs='?' )
parser.add_argument("--year", required = False, default='2024', nargs='?' )
parser.add_argument("--list", required = True, default='fires', nargs='?' )
parser.add_argument("--firename", required = False, default='null', nargs='?' )
parser.add_argument("--debug", required = False, default=False, nargs='?' )

args = parser.parse_args()
active = str2bool(args.active)
debug = str2bool(args.debug)
year = args.year
list = args.list
firename = args.firename

if list not in ('fires', 'timestamps'):
    printerr("Error: Specify list as 'fires' or 'timestamps'")

if active:
    type='active'
    if list == 'fires':
        if debug:
           print ('Listing available active fires')

    if list == 'timestamps':
        if firename == 'null':
            printerr('Error: when listing timestamps, specify active fire name')
        if debug:
            print ('Listing timestamps for active fire ' + firename )

else:
    type='historical'
    if any(item == year for item in valid_historical_years_years):
        pass
    else:
        msg='Error: set year to one of '
        for year in valid_historical_years_years:
            msg = msg + year + ' '
        printerr(msg)

    if list == 'fires':
        if debug:
            print ('Listing historical fires for ' + year)

    if list == 'timestamps':
        if firename == 'null':
            printerr('Error: when listing timestamps, specify historical fire name')
        if debug:
            print ('Listing timestamps for historical fire ' + year + ' ' + firename )

def run():
    with grpc.insecure_channel(cloudfire_server + ':50053') as channel:
        stub = available_polygons_pb2_grpc.AvailablePolygonsStub(channel)
        response = stub.GetPolygons(available_polygons_pb2.Request( type = type, year = year, list = list, firename = firename) )
        print (response.available_polygons)

if __name__ == '__main__':
    logging.basicConfig()
    run()
