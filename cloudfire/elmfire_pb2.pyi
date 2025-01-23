from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from typing import ClassVar as _ClassVar, Optional as _Optional

DESCRIPTOR: _descriptor.FileDescriptor

class Request(_message.Message):
    __slots__ = ("firename", "initialization_type", "ignition_time", "active_fire_timestamp", "already_burned_timestamp", "ignition_lon", "ignition_lat", "center_lon", "center_lat", "west_buffer", "south_buffer", "east_buffer", "north_buffer", "num_ensemble_members", "ignition_radius", "run_hours", "fuel_source", "fuel_version", "outdir")
    FIRENAME_FIELD_NUMBER: _ClassVar[int]
    INITIALIZATION_TYPE_FIELD_NUMBER: _ClassVar[int]
    IGNITION_TIME_FIELD_NUMBER: _ClassVar[int]
    ACTIVE_FIRE_TIMESTAMP_FIELD_NUMBER: _ClassVar[int]
    ALREADY_BURNED_TIMESTAMP_FIELD_NUMBER: _ClassVar[int]
    IGNITION_LON_FIELD_NUMBER: _ClassVar[int]
    IGNITION_LAT_FIELD_NUMBER: _ClassVar[int]
    CENTER_LON_FIELD_NUMBER: _ClassVar[int]
    CENTER_LAT_FIELD_NUMBER: _ClassVar[int]
    WEST_BUFFER_FIELD_NUMBER: _ClassVar[int]
    SOUTH_BUFFER_FIELD_NUMBER: _ClassVar[int]
    EAST_BUFFER_FIELD_NUMBER: _ClassVar[int]
    NORTH_BUFFER_FIELD_NUMBER: _ClassVar[int]
    NUM_ENSEMBLE_MEMBERS_FIELD_NUMBER: _ClassVar[int]
    IGNITION_RADIUS_FIELD_NUMBER: _ClassVar[int]
    RUN_HOURS_FIELD_NUMBER: _ClassVar[int]
    FUEL_SOURCE_FIELD_NUMBER: _ClassVar[int]
    FUEL_VERSION_FIELD_NUMBER: _ClassVar[int]
    OUTDIR_FIELD_NUMBER: _ClassVar[int]
    firename: str
    initialization_type: str
    ignition_time: str
    active_fire_timestamp: str
    already_burned_timestamp: str
    ignition_lon: float
    ignition_lat: float
    center_lon: float
    center_lat: float
    west_buffer: float
    south_buffer: float
    east_buffer: float
    north_buffer: float
    num_ensemble_members: int
    ignition_radius: float
    run_hours: int
    fuel_source: str
    fuel_version: str
    outdir: str
    def __init__(self, firename: _Optional[str] = ..., initialization_type: _Optional[str] = ..., ignition_time: _Optional[str] = ..., active_fire_timestamp: _Optional[str] = ..., already_burned_timestamp: _Optional[str] = ..., ignition_lon: _Optional[float] = ..., ignition_lat: _Optional[float] = ..., center_lon: _Optional[float] = ..., center_lat: _Optional[float] = ..., west_buffer: _Optional[float] = ..., south_buffer: _Optional[float] = ..., east_buffer: _Optional[float] = ..., north_buffer: _Optional[float] = ..., num_ensemble_members: _Optional[int] = ..., ignition_radius: _Optional[float] = ..., run_hours: _Optional[int] = ..., fuel_source: _Optional[str] = ..., fuel_version: _Optional[str] = ..., outdir: _Optional[str] = ...) -> None: ...

class Reply(_message.Message):
    __slots__ = ("status_message", "status_code", "fileloc")
    STATUS_MESSAGE_FIELD_NUMBER: _ClassVar[int]
    STATUS_CODE_FIELD_NUMBER: _ClassVar[int]
    FILELOC_FIELD_NUMBER: _ClassVar[int]
    status_message: str
    status_code: int
    fileloc: str
    def __init__(self, status_message: _Optional[str] = ..., status_code: _Optional[int] = ..., fileloc: _Optional[str] = ...) -> None: ...
