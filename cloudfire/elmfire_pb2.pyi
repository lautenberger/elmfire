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
    __slots__ = ["active_fire_timestamp", "already_burned_timestamp", "center_lat", "center_lon", "east_buffer", "firename", "fuel_source", "fuel_version", "ignition_lat", "ignition_lon", "ignition_radius", "ignition_time", "initialization_type", "north_buffer", "num_ensemble_members", "outdir", "run_hours", "south_buffer", "west_buffer"]
    ACTIVE_FIRE_TIMESTAMP_FIELD_NUMBER: _ClassVar[int]
    ALREADY_BURNED_TIMESTAMP_FIELD_NUMBER: _ClassVar[int]
    CENTER_LAT_FIELD_NUMBER: _ClassVar[int]
    CENTER_LON_FIELD_NUMBER: _ClassVar[int]
    EAST_BUFFER_FIELD_NUMBER: _ClassVar[int]
    FIRENAME_FIELD_NUMBER: _ClassVar[int]
    FUEL_SOURCE_FIELD_NUMBER: _ClassVar[int]
    FUEL_VERSION_FIELD_NUMBER: _ClassVar[int]
    IGNITION_LAT_FIELD_NUMBER: _ClassVar[int]
    IGNITION_LON_FIELD_NUMBER: _ClassVar[int]
    IGNITION_RADIUS_FIELD_NUMBER: _ClassVar[int]
    IGNITION_TIME_FIELD_NUMBER: _ClassVar[int]
    INITIALIZATION_TYPE_FIELD_NUMBER: _ClassVar[int]
    NORTH_BUFFER_FIELD_NUMBER: _ClassVar[int]
    NUM_ENSEMBLE_MEMBERS_FIELD_NUMBER: _ClassVar[int]
    OUTDIR_FIELD_NUMBER: _ClassVar[int]
    RUN_HOURS_FIELD_NUMBER: _ClassVar[int]
    SOUTH_BUFFER_FIELD_NUMBER: _ClassVar[int]
    WEST_BUFFER_FIELD_NUMBER: _ClassVar[int]
    active_fire_timestamp: str
    already_burned_timestamp: str
    center_lat: float
    center_lon: float
    east_buffer: float
    firename: str
    fuel_source: str
    fuel_version: str
    ignition_lat: float
    ignition_lon: float
    ignition_radius: float
    ignition_time: str
    initialization_type: str
    north_buffer: float
    num_ensemble_members: int
    outdir: str
    run_hours: int
    south_buffer: float
    west_buffer: float
    def __init__(self, firename: _Optional[str] = ..., initialization_type: _Optional[str] = ..., ignition_time: _Optional[str] = ..., active_fire_timestamp: _Optional[str] = ..., already_burned_timestamp: _Optional[str] = ..., ignition_lon: _Optional[float] = ..., ignition_lat: _Optional[float] = ..., center_lon: _Optional[float] = ..., center_lat: _Optional[float] = ..., west_buffer: _Optional[float] = ..., south_buffer: _Optional[float] = ..., east_buffer: _Optional[float] = ..., north_buffer: _Optional[float] = ..., num_ensemble_members: _Optional[int] = ..., ignition_radius: _Optional[float] = ..., run_hours: _Optional[int] = ..., fuel_source: _Optional[str] = ..., fuel_version: _Optional[str] = ..., outdir: _Optional[str] = ...) -> None: ...
