# Generated by the gRPC Python protocol compiler plugin. DO NOT EDIT!
"""Client and server classes corresponding to protobuf-defined services."""
import grpc

import tiles_fcst_pb2 as tiles__fcst__pb2


class RiskTilesStub(object):
    """Missing associated documentation comment in .proto file."""

    def __init__(self, channel):
        """Constructor.

        Args:
            channel: A grpc.Channel.
        """
        self.GetTileData = channel.unary_unary(
                '/RiskTiles/GetTileData',
                request_serializer=tiles__fcst__pb2.Request.SerializeToString,
                response_deserializer=tiles__fcst__pb2.Reply.FromString,
                )


class RiskTilesServicer(object):
    """Missing associated documentation comment in .proto file."""

    def GetTileData(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')


def add_RiskTilesServicer_to_server(servicer, server):
    rpc_method_handlers = {
            'GetTileData': grpc.unary_unary_rpc_method_handler(
                    servicer.GetTileData,
                    request_deserializer=tiles__fcst__pb2.Request.FromString,
                    response_serializer=tiles__fcst__pb2.Reply.SerializeToString,
            ),
    }
    generic_handler = grpc.method_handlers_generic_handler(
            'RiskTiles', rpc_method_handlers)
    server.add_generic_rpc_handlers((generic_handler,))


 # This class is part of an EXPERIMENTAL API.
class RiskTiles(object):
    """Missing associated documentation comment in .proto file."""

    @staticmethod
    def GetTileData(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/RiskTiles/GetTileData',
            tiles__fcst__pb2.Request.SerializeToString,
            tiles__fcst__pb2.Reply.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)
