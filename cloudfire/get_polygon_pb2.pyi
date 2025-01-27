from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from typing import ClassVar as _ClassVar, Optional as _Optional

DESCRIPTOR: _descriptor.FileDescriptor

class Request(_message.Message):
    __slots__ = ("firename", "timestamp", "transfer_mode")
    FIRENAME_FIELD_NUMBER: _ClassVar[int]
    TIMESTAMP_FIELD_NUMBER: _ClassVar[int]
    TRANSFER_MODE_FIELD_NUMBER: _ClassVar[int]
    firename: str
    timestamp: str
    transfer_mode: str
    def __init__(self, firename: _Optional[str] = ..., timestamp: _Optional[str] = ..., transfer_mode: _Optional[str] = ...) -> None: ...

class Reply(_message.Message):
    __slots__ = ("fileloc",)
    FILELOC_FIELD_NUMBER: _ClassVar[int]
    fileloc: str
    def __init__(self, fileloc: _Optional[str] = ...) -> None: ...
