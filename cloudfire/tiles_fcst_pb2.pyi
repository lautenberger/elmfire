from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from typing import ClassVar as _ClassVar, Optional as _Optional

DESCRIPTOR: _descriptor.FileDescriptor

class Request(_message.Message):
    __slots__ = ("pattern", "forecast_cycle", "lfmdate", "tile", "transfer_mode")
    PATTERN_FIELD_NUMBER: _ClassVar[int]
    FORECAST_CYCLE_FIELD_NUMBER: _ClassVar[int]
    LFMDATE_FIELD_NUMBER: _ClassVar[int]
    TILE_FIELD_NUMBER: _ClassVar[int]
    TRANSFER_MODE_FIELD_NUMBER: _ClassVar[int]
    pattern: str
    forecast_cycle: str
    lfmdate: str
    tile: str
    transfer_mode: str
    def __init__(self, pattern: _Optional[str] = ..., forecast_cycle: _Optional[str] = ..., lfmdate: _Optional[str] = ..., tile: _Optional[str] = ..., transfer_mode: _Optional[str] = ...) -> None: ...

class Reply(_message.Message):
    __slots__ = ("wxloc", "fuelloc", "topoloc", "structdensloc", "ignloc", "lfmloc", "snodasloc")
    WXLOC_FIELD_NUMBER: _ClassVar[int]
    FUELLOC_FIELD_NUMBER: _ClassVar[int]
    TOPOLOC_FIELD_NUMBER: _ClassVar[int]
    STRUCTDENSLOC_FIELD_NUMBER: _ClassVar[int]
    IGNLOC_FIELD_NUMBER: _ClassVar[int]
    LFMLOC_FIELD_NUMBER: _ClassVar[int]
    SNODASLOC_FIELD_NUMBER: _ClassVar[int]
    wxloc: str
    fuelloc: str
    topoloc: str
    structdensloc: str
    ignloc: str
    lfmloc: str
    snodasloc: str
    def __init__(self, wxloc: _Optional[str] = ..., fuelloc: _Optional[str] = ..., topoloc: _Optional[str] = ..., structdensloc: _Optional[str] = ..., ignloc: _Optional[str] = ..., lfmloc: _Optional[str] = ..., snodasloc: _Optional[str] = ...) -> None: ...
