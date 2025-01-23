from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from typing import ClassVar as _ClassVar, Optional as _Optional

DESCRIPTOR: _descriptor.FileDescriptor

class Request(_message.Message):
    __slots__ = ("type", "year", "list", "firename")
    TYPE_FIELD_NUMBER: _ClassVar[int]
    YEAR_FIELD_NUMBER: _ClassVar[int]
    LIST_FIELD_NUMBER: _ClassVar[int]
    FIRENAME_FIELD_NUMBER: _ClassVar[int]
    type: str
    year: str
    list: str
    firename: str
    def __init__(self, type: _Optional[str] = ..., year: _Optional[str] = ..., list: _Optional[str] = ..., firename: _Optional[str] = ...) -> None: ...

class Reply(_message.Message):
    __slots__ = ("available_polygons",)
    AVAILABLE_POLYGONS_FIELD_NUMBER: _ClassVar[int]
    available_polygons: str
    def __init__(self, available_polygons: _Optional[str] = ...) -> None: ...
