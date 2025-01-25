#!/usr/bin/env python3
#
# Dependencies are wget, grpcio, and grpcio-tools, e.g.:
# sudo apt-get install wget
# pip3 install grpcio grpcio-tools
#
# Sample usage:
#./elmfire.py \
#    --firename='ca-glass_20200928_213600' --outdir='./ca-glass_20200928_213600' \
#    --point_ignition=False --polygon_ignition=True \
#    --active_fire_timestamp='20200928_213600' --already_burned_timestamp='20200927_220000' \
#    --num_ensemble_members=200 \
#    --wx_type='historical' --ignition_time="2020-09-28 21:36" --run_hours=24 \
#    --fuel_source='landfire' --fuel_version='1.4.0'
#
#./elmfire.py \
#    --firename='ca-test_20181108_140000' --outdir='./ca-test_20181108_140000' \
#    --point_ignition=True --polygon_ignition=False \
#    --ignition_lon=-121.5 --ignition_lat=40.0 \
#    --ignition_radius=300.0 --num_ensemble_members=200 \
#    --wx_type='historical' --ignition_time="2018-11-08 14:00" --run_hours=24 \
#    --fuel_source='landfire' --fuel_version='1.4.0'
#
from __future__ import print_function
import logging
import grpc
import elmfire_pb2
import elmfire_pb2_grpc
import sys
import argparse
import subprocess
import os
import datetime
from datetime import date, timedelta
from dateutil.parser import parse
import pytz

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

def round_time_to_nearest(t,base):
    newmin=0
    newhr=t.hour
    if t.minute > 30:
        newhr = t.hour + 1

    n = newhr + (base//2)
    newhr = n - (n % base)
    if newhr == 24:
       return (t.replace(hour=newhr-6,minute=newmin))
    else:
       return (t.replace(hour=newhr,minute=newmin))

timezone = pytz.timezone("UTC")

valid_fuel_versions = ["1.0.5", "1.3.0", "1.4.0", "2.0.0_2019", "2.0.0_2020", "2.1.0", "2.2.0"]
valid_wx_types = ["forecast", "historical"]

historical_wx_earliest = datetime.datetime(2011, 1, 30,  0, 0, tzinfo=datetime.timezone.utc)
historical_wx_latest =   datetime.datetime(2022, 9, 30, 23, 0, tzinfo=datetime.timezone.utc)

print_inputs=False

if "CLOUDFIRE_SERVER" in os.environ:
    cloudfire_server= os.environ['CLOUDFIRE_SERVER']
else:
    cloudfire_server='worldgen.cloudfire.io'

cloudfire_channel=cloudfire_server + ':60090'

parser = argparse.ArgumentParser()

# Basic inputs - firename, output directory, center lat/lon, and buffer from center:
parser.add_argument("--firename", required = True, nargs='?', help="Fire name")
parser.add_argument("--outdir", required = True, nargs='?' )
parser.add_argument("--center_lat", type=float, default=-9999., required = False, nargs='?', help="Latitude at tile center")
parser.add_argument("--center_lon", type=float, default=-9999., required = False, nargs='?', help="Longitude at tile center")
parser.add_argument("--west_buffer", type=float, default=30.0, required = False, nargs='?')
parser.add_argument("--east_buffer", type=float, default=30.0, required = False, nargs='?')
parser.add_argument("--south_buffer", type=float, default=30.0, required = False, nargs='?')
parser.add_argument("--north_buffer", type=float, default=30.0, required = False, nargs='?')

# Fuel inputs - source (landfire) and version (1.4.0, 2.0.0, etc.)
parser.add_argument("--fuel_source", default='landfire', required = False, nargs='?')
parser.add_argument("--fuel_version", default='2.4.0_2.3.0', required = False, nargs='?')

# Weather inputs
parser.add_argument("--wx_type", default='forecast', required = False, nargs='?')

# Ignition inputs
parser.add_argument("--point_ignition", default=True, required = False, nargs='?')
parser.add_argument("--ignition_lat", type=float, default=-9999., required=False, nargs='?')
parser.add_argument("--ignition_lon", type=float, default=-9999., required=False, nargs='?')
parser.add_argument("--polygon_ignition", default=False, required=False, nargs='?')
parser.add_argument("--active_fire_timestamp", default='null', required=False, nargs='?')
parser.add_argument("--already_burned_timestamp", default='null', required=False, nargs='?')
parser.add_argument("--ignition_radius", type=float, default=300.0, required=False, nargs='?')
parser.add_argument("--ignition_time", default='null', required = True, nargs='?')

# Run parameters
parser.add_argument("--run_hours", type=int, default=24, required = False, nargs='?')
parser.add_argument("--num_ensemble_members", type=int, default=200, required = False, nargs='?')

args = parser.parse_args()

# Basic info
firename = args.firename
outdir = args.outdir
center_lat = args.center_lat
center_lon = args.center_lon
west_buffer= args.west_buffer
east_buffer= args.east_buffer
south_buffer= args.south_buffer
north_buffer= args.north_buffer

# Run parameters
run_hours= args.run_hours
num_ensemble_members= args.num_ensemble_members

# Fuel:
fuel_source = args.fuel_source
if fuel_source != 'landfire':
    printerr("Error: For now, 'landfire' is the only valid --fuel_source")
fuel_version = args.fuel_version
if any(item == fuel_version for item in valid_fuel_versions):
    print('Using LANDFIRE ' + fuel_version)
else:
    msg='Error: set fuel_version to one of '
    for ver in valid_fuel_versions:
        msg = msg + ver + ' '
    printerr(msg)

# Ignition
point_ignition = str2bool(args.point_ignition)
polygon_ignition = str2bool(args.polygon_ignition)
active_fire_timestamp = args.active_fire_timestamp
already_burned_timestamp = args.already_burned_timestamp
ignition_time=args.ignition_time
ignition_lat=-9999.
ignition_lon=-9999.
ignition_radius=-9999.

try:
    t = parse(ignition_time)
except:
    printerr('Error: ignition_time (' + args.ignition_time + ') cannot be parsed')

if polygon_ignition:
    if point_ignition:
        printerr('Error: both point_ignition and polygon_ignition are True')

    active_fire_timestamp = args.active_fire_timestamp
    already_burned_timestamp = args.already_burned_timestamp
    initialization_type = 'active_fire_polygon'

if point_ignition:
    if polygon_ignition:
       printerr('Error: both point_ignition and polygon_ignition are True')

    initialization_type = 'points_within_polygon'

    if args.ignition_lat < -9998 or args.ignition_lon < -9998:
        print('--ignition_lat or --ignition_lon not specified, using --center_lat and --center_lon')
        if args.center_lat < -9998 or args.center_lon < -9998:
            printerr('Error: --center_lat or --center_lon not specified')

    if args.center_lat < -9998 or args.center_lon < -9998:
        print('--center_lat or --center_lon not specified, using --ignition_lat and --ignition_lon')
        if args.ignition_lat < -9998 or args.ignition_lon < -9998:
            printerr('Error: --ignition_lat or --ignition_lon not specified')

    if args.ignition_lat > -9998:
        ignition_lat = args.ignition_lat
    else:
        ignition_lat = center_lat

    if args.ignition_lon:
        ignition_lon = args.ignition_lon
    else:
        ignition_lon = center_lon

    ignition_radius = args.ignition_radius
    print( 'Ignition coordinates: (' + str(ignition_lon) + ', ' + str(ignition_lat) + ')' )
    print( 'Ignition radius: ' + str(ignition_radius) + 'm')

# Weather
wx_type = args.wx_type
if any(item == wx_type for item in valid_wx_types):
    print('Using ' + wx_type + ' weather')
else:
    msg='Error: set wx_type to one of '
    for type in valid_wx_types:
        msg = msg + type + ' '
    printerr(msg)

wx_start_time = datetime.datetime(t.year, t.month, t.day, t.hour, 0, tzinfo=datetime.timezone.utc)

print('Weather start time: ' + wx_start_time.strftime("%Y-%m-%d %H:%M") )

if wx_type == 'historical':
    wx_num_hours = int(args.run_hours) + 1
    wx_stop_time = wx_start_time + timedelta(hours = wx_num_hours)

    print('Weather stop  time: ' + wx_stop_time.strftime ("%Y-%m-%d %H:%M") )

    if wx_start_time < historical_wx_earliest:
        printerr('Error: check --ignition_time. Historical weather begins at ' + historical_wx_earliest.strftime("%Y-%m-%d %H:%M") )
    if wx_stop_time > historical_wx_latest:
        printerr('Error: check --ignition_time and/or --run_hours. Historical weather ends at ' + historical_wx_latest.strftime("%Y-%m-%d %H:%M") )

if wx_type == 'forecast':
    wx_num_hours = 192
    t = datetime.datetime.utcnow() - timedelta(hours = 5)
    five_hours_ago = datetime.datetime(t.year, t.month, t.day, t.hour, t.minute, tzinfo=datetime.timezone.utc)
    forecast_cycle_latest = round_time_to_nearest(five_hours_ago, 6)
    forecast_cycle_earliest = forecast_cycle_latest - timedelta(hours = 66)

    print('Earliest available forecast cycle: ' + forecast_cycle_earliest.strftime("%Y%m%d_%H") )
    print('Latest   available forecast cycle: ' + forecast_cycle_latest.strftime("%Y%m%d_%H") )

    if wx_start_time < forecast_cycle_earliest:
       printerr('Error: --ignition_time is before earliest available forecast cycle')

    if wx_start_time > forecast_cycle_latest:
       print ('Warning: --ignition_time is ahead of latest available forecast cycle')

print( 'Valid inputs received - calling ELMFIRE microservice' )

def run():
    with grpc.insecure_channel(cloudfire_channel) as channel:
        stub = elmfire_pb2_grpc.ELMFIREStub(channel)
        if (print_inputs):
            print ('firename', firename)
            print ('outdir', outdir)
            print ('initialization_type', initialization_type)
            print ('ignition_time', ignition_time)
            print ('active_fire_timestamp', active_fire_timestamp)
            print ('already_burned_timestamp', already_burned_timestamp)
            print ('ignition_lon', ignition_lon )
            print ('ignition_lat', ignition_lat )
            print ('center_lon', center_lon )
            print ('center_lat', center_lat )
            print ('west_buffer', west_buffer )
            print ('south_buffer', south_buffer )
            print ('east_buffer', east_buffer )
            print ('north_buffer', north_buffer )
            print ('num_ensemble_members', num_ensemble_members )
#            print ('add_to_active_fires', add_to_active_fires )
            print ('ignition_radius', ignition_radius )
            print ('run_hours', run_hours )
            print ('fuel_source', fuel_source )
            print ('fuel_version', fuel_version )
#            print ('scp_input_deck', scp_input_deck )

        response = stub.RunELMFIRE(elmfire_pb2.Request( firename = firename,
                                                        initialization_type = initialization_type,
                                                        ignition_time = ignition_time,
                                                        active_fire_timestamp = active_fire_timestamp,
                                                        already_burned_timestamp = already_burned_timestamp,
                                                        ignition_lon = ignition_lon,
                                                        ignition_lat = ignition_lat,
                                                        center_lon = center_lon,
                                                        center_lat = center_lat,
                                                        west_buffer = west_buffer,
                                                        south_buffer = south_buffer,
                                                        east_buffer = east_buffer,
                                                        north_buffer = north_buffer,
                                                        num_ensemble_members = num_ensemble_members,
                                                        ignition_radius = ignition_radius,
                                                        run_hours = run_hours,
                                                        fuel_source = fuel_source,
                                                        fuel_version = fuel_version,
                                                        outdir = outdir ) )

        print( 'Downloading tarball' )

# This will not work prior to python 3.10, so hard code command to use wget:
#    match transfer_mode:
#        case 'cp':
#            command='cp -f ' + response.fileloc + ' ' + outdir + '/'
#
#        case 'scp':
#            command='scp ' + response.fileloc + ' ' + outdir + '/'
#
#        case 'wget':
#            command='wget -q -r -np -nH --cut-dirs=2 -R "index.html*" -P ' + outdir + ' ' + response.fileloc
#
#        case _:
#            print( "Specify --transfer_mode as 'cp', 'scp', or 'wget' ")
#
    command='wget -q -r -np -nH --cut-dirs=2 -R "index.html*" -P ' + outdir + ' ' + response.fileloc

    if command:
        print(command)
        proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
        proc.wait()
        print( 'Complete' )

if __name__ == '__main__':
    logging.basicConfig()
    run()
