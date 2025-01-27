#!/usr/bin/env python3

from __future__ import print_function
import logging
import grpc
import tiles_fcst_pb2
import tiles_fcst_pb2_grpc
import sys
import os

pattern=sys.argv[1]
forecast_cycle=sys.argv[2]
lfmdate=sys.argv[3]
tile=sys.argv[4]
transfer_mode=sys.argv[5]

if "CLOUDFIRE_SERVER" in os.environ:
    cloudfire_server= os.environ['CLOUDFIRE_SERVER']
else:
    cloudfire_server='worldgen.cloudfire.io'

cloudfire_channel=cloudfire_server + ':50055'

def run():
    with grpc.insecure_channel(cloudfire_channel) as channel:
        stub = tiles_fcst_pb2_grpc.RiskTilesStub(channel)
        response = stub.GetTileData(tiles_fcst_pb2.Request( pattern=pattern,
                                                            forecast_cycle=forecast_cycle,
                                                            lfmdate=lfmdate,
                                                            tile=tile,
                                                            transfer_mode=transfer_mode ) )
    print(response.wxloc)
    print(response.fuelloc)
    print(response.topoloc)
    print(response.structdensloc)
    print(response.ignloc)
    print(response.lfmloc)
    print(response.snodasloc)

if __name__ == '__main__':
    logging.basicConfig()
    run()
