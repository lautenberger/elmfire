from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from typing import ClassVar as _ClassVar, Optional as _Optional

DESCRIPTOR: _descriptor.FileDescriptor

class Reply(_message.Message):
    __slots__ = ["fileloc", "status_code", "status_message"]
    FILELOC_FIELD_NUMBER: _ClassVar[int]
    STATUS_CODE_FIELD_NUMBER: _ClassVar[int]
    STATUS_MESSAGE_FIELD_NUMBER: _ClassVar[int]
    fileloc: str
    status_code: int
    status_message: str
    def __init__(self, status_message: _Optional[str] = ..., status_code: _Optional[int] = ..., fileloc: _Optional[str] = ...) -> None: ...

class Request(_message.Message):
    __slots__ = ["active_fire_timestamp", "already_burned_timestamp", "center_lat", "center_lon", "do_fuel", "do_ignition", "do_wx", "east_buffer", "fuel_source", "fuel_version", "ignition_lat", "ignition_lon", "ignition_radius", "name", "north_buffer", "outdir", "point_ignition", "polygon_ignition", "south_buffer", "west_buffer", "wx_num_hours", "wx_start_time", "wx_type"]
    ACTIVE_FIRE_TIMESTAMP_FIELD_NUMBER: _ClassVar[int]
    ALREADY_BURNED_TIMESTAMP_FIELD_NUMBER: _ClassVar[int]
    CENTER_LAT_FIELD_NUMBER: _ClassVar[int]
    CENTER_LON_FIELD_NUMBER: _ClassVar[int]
    DO_FUEL_FIELD_NUMBER: _ClassVar[int]
    DO_IGNITION_FIELD_NUMBER: _ClassVar[int]
    DO_WX_FIELD_NUMBER: _ClassVar[int]
    EAST_BUFFER_FIELD_NUMBER: _ClassVar[int]
    FUEL_SOURCE_FIELD_NUMBER: _ClassVar[int]
    FUEL_VERSION_FIELD_NUMBER: _ClassVar[int]
    IGNITION_LAT_FIELD_NUMBER: _ClassVar[int]
    IGNITION_LON_FIELD_NUMBER: _ClassVar[int]
    IGNITION_RADIUS_FIELD_NUMBER: _ClassVar[int]
    NAME_FIELD_NUMBER: _ClassVar[int]
    NORTH_BUFFER_FIELD_NUMBER: _ClassVar[int]
    OUTDIR_FIELD_NUMBER: _ClassVar[int]
    POINT_IGNITION_FIELD_NUMBER: _ClassVar[int]
    POLYGON_IGNITION_FIELD_NUMBER: _ClassVar[int]
    SOUTH_BUFFER_FIELD_NUMBER: _ClassVar[int]
    WEST_BUFFER_FIELD_NUMBER: _ClassVar[int]
    WX_NUM_HOURS_FIELD_NUMBER: _ClassVar[int]
    WX_START_TIME_FIELD_NUMBER: _ClassVar[int]
    WX_TYPE_FIELD_NUMBER: _ClassVar[int]
    active_fire_timestamp: str
    already_burned_timestamp: str
    center_lat: float
    center_lon: float
    do_fuel: bool
    do_ignition: bool
    do_wx: bool
    east_buffer: float
    fuel_source: str
    fuel_version: str
    ignition_lat: float
    ignition_lon: float
    ignition_radius: float
    name: str
    north_buffer: float
    outdir: str
    point_ignition: bool
    polygon_ignition: bool
    south_buffer: float
    west_buffer: float
    wx_num_hours: int
    wx_start_time: str
    wx_type: str
    def __init__(self, name: _Optional[str] = ..., center_lat: _Optional[float] = ..., center_lon: _Optional[float] = ..., west_buffer: _Optional[float] = ..., east_buffer: _Optional[float] = ..., south_buffer: _Optional[float] = ..., north_buffer: _Optional[float] = ..., do_fuel: bool = ..., fuel_source: _Optional[str] = ..., fuel_version: _Optional[str] = ..., do_wx: bool = ..., wx_type: _Optional[str] = ..., wx_start_time: _Optional[str] = ..., wx_num_hours: _Optional[int] = ..., do_ignition: bool = ..., point_ignition: bool = ..., polygon_ignition: bool = ..., ignition_lat: _Optional[float] = ..., ignition_lon: _Optional[float] = ..., active_fire_timestamp: _Optional[str] = ..., already_burned_timestamp: _Optional[str] = ..., ignition_radius: _Optional[float] = ..., outdir: _Optional[str] = ...) -> None: ...
