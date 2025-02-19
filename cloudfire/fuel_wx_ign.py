#!/usr/bin/env python3
#
# Dependencies are wget, grpcio, and grpcio-tools, e.g.:
# sudo apt-get install wget
# pip3 install grpcio grpcio-tools
#
# Sample usage:
#./fuel_wx_ign.py \
#    --name='sample-historical' --outdir='./sample-historical' \
#    --center_lon=-121.5 --center_lat=40.0 \
#    --do_fuel=True \
#    --fuel_source='landfire' --fuel_version='1.4.0' \
#    --do_wx=True \
#    --wx_type='historical' --wx_start_time="2018-11-08 14:00" --wx_num_hours=72 \
#    --do_ignition=True \
#    --point_ignition=True --ignition_lon=-121.3 --ignition_lat=40.2 --ignition_radius=300.
#
#./fuel_wx_ign.py \
#    --name='sample-forecast' --outdir='./sample-forecast' \
#    --center_lon=-121.5 --center_lat=40.0 \
#    --do_fuel=True \
#    --fuel_source='landfire' --fuel_version='2.2.0' \
#    --do_wx=True \
#    --wx_type='forecast' --wx_start_time="2023-02-04 18:00" \
#    --do_ignition=True \
#    --point_ignition=True --ignition_lon=-121.3 --ignition_lat=40.2 --ignition_radius=300.

from __future__ import print_function
import logging
import grpc
import fuel_wx_ign_pb2
import fuel_wx_ign_pb2_grpc
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

valid_fuel_versions = ["1.0.5", "1.3.0", "1.4.0", "2.0.0_2019", "2.0.0_2020", "2.1.0", "2.2.0", "2.3.0", "2.4.0", "2.4.0_2.1.0_nbflip" ]
valid_wx_types = ["forecast", "historical"]

historical_wx_earliest = datetime.datetime(2011, 1, 30,  0, 0, tzinfo=datetime.timezone.utc)
historical_wx_latest =   datetime.datetime(2025, 1, 19, 23, 0, tzinfo=datetime.timezone.utc)

print_inputs=False

if "CLOUDFIRE_SERVER" in os.environ:
    cloudfire_server= os.environ['CLOUDFIRE_SERVER']
else:
    cloudfire_server='worldgen.cloudfire.io'

cloudfire_channel=cloudfire_server + ':50052'

parser = argparse.ArgumentParser()

# Basic inputs - name, output directory, center lat/lon, and buffer from center:
parser.add_argument("--name", required = False, default='null', nargs='?', help="Name of output .tar file")
parser.add_argument("--outdir", required = False, default='null', nargs='?' )
parser.add_argument("--center_lat", type=float, default=-9999., required = False, nargs='?', help="Latitude at tile center")
parser.add_argument("--center_lon", type=float, default=-9999., required = False, nargs='?', help="Longitude at tile center")
parser.add_argument("--west_buffer", type=float, default=30.0, required = False, nargs='?')
parser.add_argument("--east_buffer", type=float, default=30.0, required = False, nargs='?')
parser.add_argument("--south_buffer", type=float, default=30.0, required = False, nargs='?')
parser.add_argument("--north_buffer", type=float, default=30.0, required = False, nargs='?')

# Fuel inputs - source (landfire) and version (1.4.0, 2.0.0, etc.)
parser.add_argument("--do_fuel", default=True, required = False, nargs='?')
parser.add_argument("--fuel_source", default='landfire', required = False, nargs='?')
parser.add_argument("--fuel_version", default='2.4.0', required = False, nargs='?')

# Weather inputs
parser.add_argument("--get_available_wx_times", default=False, required = False, nargs='?')
parser.add_argument("--do_wx", default=True, required = False, nargs='?')
parser.add_argument("--wx_type", default='forecast', required = False, nargs='?')
parser.add_argument("--wx_start_time", required=False, default='null', nargs='?')
parser.add_argument("--wx_num_hours", type=int, default=24, required = False, nargs='?')

# Ignition inputs
parser.add_argument("--do_ignition", default=True, required = False, nargs='?')
parser.add_argument("--point_ignition", default=True, required = False, nargs='?')
parser.add_argument("--ignition_lat", type=float, default=-9999., required=False, nargs='?')
parser.add_argument("--ignition_lon", type=float, default=-9999., required=False, nargs='?')
parser.add_argument("--polygon_ignition", default=False, required=False, nargs='?')
parser.add_argument("--active_fire_timestamp", default='null', required=False, nargs='?')
parser.add_argument("--already_burned_timestamp", default='null', required=False, nargs='?')
parser.add_argument("--ignition_radius", type=float, default=300.0, required=False, nargs='?')

args = parser.parse_args()

# Print available weather dates and return?
#t = datetime.datetime.utcnow() - timedelta(hours = 5)
t = datetime.datetime.now(datetime.UTC) - timedelta(hours = 5)
five_hours_ago = datetime.datetime(t.year, t.month, t.day, t.hour, t.minute, tzinfo=datetime.timezone.utc)
forecast_cycle_latest = round_time_to_nearest(five_hours_ago, 6)
forecast_cycle_earliest = forecast_cycle_latest - timedelta(hours = 66)
latest_available_forecast_time = forecast_cycle_latest + timedelta(hours = 336)

get_available_wx_times = str2bool(args.get_available_wx_times)
if get_available_wx_times:
   print ('Currently available weather times:')
   print ('historical- ' + historical_wx_earliest.strftime ("%Y-%m-%d %H:%M UTC") + ' to ' + historical_wx_latest.strftime ("%Y-%m-%d %H:%M UTC") )
   print ('forecast-   ' + forecast_cycle_earliest.strftime("%Y-%m-%d %H:%M UTC") + ' to ' + latest_available_forecast_time.strftime("%Y-%m-%d %H:%M UTC") )
   exit(0)

# Check whether --name and --outdir have been specified
name = args.name
if name == 'null':
    printerr('Error:  Specify --name')

outdir = args.outdir
if outdir == 'null':
    printerr('Error:  Specify --outdir')

# Basic info
center_lat = args.center_lat
center_lon = args.center_lon
west_buffer= 1000.0*args.west_buffer
east_buffer= 1000.0*args.east_buffer
south_buffer= 1000.0*args.south_buffer
north_buffer= 1000.0*args.north_buffer

# Fuel:
do_fuel = str2bool(args.do_fuel)
if do_fuel:
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

# Weather
do_wx = str2bool(args.do_wx)
if do_wx:
    wx_start_time = args.wx_start_time
    if wx_start_time == 'null':
        printerr('Error:  Specify --wx_start_time')

    wx_type = args.wx_type
    if any(item == wx_type for item in valid_wx_types):
        print('Using ' + wx_type + ' weather')
    else:
        msg='Error: set wx_type to one of '
        for type in valid_wx_types:
            msg = msg + type + ' '
        printerr(msg)

    try:
        t = parse(args.wx_start_time)
    except:
        printerr('Error: wx_start_time (' + args.wx_start_time + ') cannot be parsed')

    wx_start_time = datetime.datetime(t.year, t.month, t.day, t.hour, 0, tzinfo=datetime.timezone.utc)

    print('Weather start time: ' + wx_start_time.strftime("%Y-%m-%d %H:%M") )

    if wx_type == 'historical':
        wx_num_hours = int(args.wx_num_hours)
        wx_stop_time = wx_start_time + timedelta(hours = wx_num_hours)

        print('Weather stop  time: ' + wx_stop_time.strftime ("%Y-%m-%d %H:%M") )

        if wx_start_time < historical_wx_earliest:
            printerr('Error: check --wx_start_time. Historical weather begins at ' + historical_wx_earliest.strftime("%Y-%m-%d %H:%M") )
        if wx_stop_time > historical_wx_latest:
            printerr('Error: check --wx_start_time and/or --wx_num_hours. Historical weather ends at ' + historical_wx_latest.strftime("%Y-%m-%d %H:%M") )

    if wx_type == 'forecast':
        wx_num_hours = 336
#        t = datetime.datetime.utcnow() - timedelta(hours = 5)
#        five_hours_ago = datetime.datetime(t.year, t.month, t.day, t.hour, t.minute, tzinfo=datetime.timezone.utc)
#        forecast_cycle_latest = round_time_to_nearest(five_hours_ago, 6)
#        forecast_cycle_earliest = forecast_cycle_latest - timedelta(hours = 66)

        print('Earliest available forecast cycle: ' + forecast_cycle_earliest.strftime("%Y%m%d_%H") )
        print('Latest   available forecast cycle: ' + forecast_cycle_latest.strftime("%Y%m%d_%H") )

        if wx_start_time < forecast_cycle_earliest:
           printerr('Error: --wx_start_time is before earliest available forecast cycle')

        if wx_start_time > forecast_cycle_latest:
           print ('Warning: --wx_start_time is ahead of latest available forecast cycle')
else:
    wx_type='historical'
    wx_start_time=historical_wx_earliest
    wx_num_hours=0

# Ignition
do_ignition = str2bool(args.do_ignition)
point_ignition = str2bool(args.point_ignition)
polygon_ignition = str2bool(args.polygon_ignition)
active_fire_timestamp = args.active_fire_timestamp
already_burned_timestamp = args.already_burned_timestamp

if do_ignition:

    if polygon_ignition:
        if point_ignition:
           printerr('Error: both point_ignition and polygon_ignition are True')

        active_fire_timestamp = args.active_fire_timestamp
        already_burned_timestamp = args.already_burned_timestamp
        ignition_lat=-9999.
        ignition_lon=-9999.
        ignition_radius=-9999.

    if point_ignition:
        if polygon_ignition:
           printerr('Error: both point_ignition and polygon_ignition are True')

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

else:
    ignition_lat = -9999.
    ignition_lon = -9999.
    ignition_radius = -9999.

print( 'Valid inputs received - calling CloudFire fuel weather ignition microservice' )

def run():
    with grpc.insecure_channel(cloudfire_channel) as channel:
        stub = fuel_wx_ign_pb2_grpc.FuelWxIgnStub(channel)
        if (print_inputs):
            print ('name', name)
            print ('center_lat', center_lat )
            print ('center_lon', center_lon )
            print ('west_buffer', west_buffer )
            print ('east_buffer', east_buffer )
            print ('south_buffer', south_buffer )
            print ('north_buffer', north_buffer )
            print ('do_fuel', do_fuel )
            print ('fuel_source', fuel_source )
            print ('fuel_version', fuel_version )
            print ('do_wx', do_wx )
            print ('wx_type', wx_type )
            print ('wx_start_time', wx_start_time.strftime ("%Y-%m-%d %H:%M") )
            print ('wx_num_hours', wx_num_hours )
            print ('do_ignition', do_ignition )
            print ('point_ignition', point_ignition )
            print ('ignition_lat', ignition_lat )
            print ('ignition_lon', ignition_lon )
            print ('polygon_ignition', polygon_ignition )
            print ('active_fire_timestamp', active_fire_timestamp.strftime ("%Y-%m-%d %H:%M") )
            print ('already_burned_timestamp', already_burned_timestamp.strftime ("%Y-%m-%d %H:%M") )
            print ('ignition_radius', ignition_radius )

        response = stub.GetDomainData(fuel_wx_ign_pb2.Request( name = name ,
                                                               center_lat = center_lat,
                                                               center_lon = center_lon,
                                                               west_buffer = west_buffer,
                                                               east_buffer = east_buffer,
                                                               south_buffer = south_buffer,
                                                               north_buffer = north_buffer,
                                                               do_fuel = do_fuel,
                                                               fuel_source = fuel_source,
                                                               fuel_version = fuel_version,
                                                               do_wx = do_wx,
                                                               wx_type = wx_type,
                                                               wx_start_time = wx_start_time.strftime ("%Y-%m-%d %H:%M"),
                                                               wx_num_hours = wx_num_hours,
                                                               do_ignition = do_ignition,
                                                               point_ignition = point_ignition,
                                                               ignition_lat = ignition_lat,
                                                               ignition_lon = ignition_lon,
                                                               polygon_ignition = polygon_ignition,
                                                               active_fire_timestamp = active_fire_timestamp,
                                                               already_burned_timestamp = already_burned_timestamp,
                                                               ignition_radius = ignition_radius,
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
#    command='scp ' + response.fileloc + ' ' + outdir + '/'

    if command:
        proc = subprocess.Popen([command], stdout=subprocess.PIPE, shell = True)
        proc.wait()
        print( 'Complete' )

if __name__ == '__main__':
    logging.basicConfig()
    run()
