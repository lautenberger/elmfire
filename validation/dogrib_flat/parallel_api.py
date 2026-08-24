# parallel_api.py
# -*- coding: utf-8 -*-

from __future__ import annotations

import time
import threading
from dataclasses import dataclass
from typing import Callable, Iterable, List, Optional, TypeVar, Generic, Any

import requests
from concurrent.futures import ThreadPoolExecutor, as_completed

T = TypeVar("T")
R = TypeVar("R")

_PRINT_LOCK = threading.Lock()
_thread_local = threading.local()


def make_logger(prefix: str = "") -> Callable[[str], None]:
    pfx = f"[{prefix}] " if prefix else ""
    def log(msg: str = "") -> None:
        ts = time.strftime("%H:%M:%S")
        lines = str(msg).splitlines() or [""]
        with _PRINT_LOCK:
            for line in lines:
                print(f"{ts} {pfx}{line}", flush=True)
    return log


def get_thread_session() -> requests.Session:
    """
    One requests.Session per worker thread (connection reuse, faster).
    """
    if not hasattr(_thread_local, "session"):
        _thread_local.session = requests.Session()
    return _thread_local.session


def retry_call(
    fn: Callable[[], R],
    *,
    tries: int = 4,
    base_sleep_s: float = 1.0,
    max_sleep_s: float = 20.0,
    retry_on: tuple = (requests.RequestException, TimeoutError),
    log: Optional[Callable[[str], None]] = None,
) -> R:
    """
    Exponential backoff retry wrapper.
    """
    sleep_s = base_sleep_s
    last_exc: Optional[BaseException] = None

    for attempt in range(1, tries + 1):
        try:
            return fn()
        except retry_on as e:
            last_exc = e
            if attempt == tries:
                break
            if log:
                log(f"Retry {attempt}/{tries} after {type(e).__name__}: {e}")
            time.sleep(min(max_sleep_s, sleep_s))
            sleep_s *= 2

    assert last_exc is not None
    raise last_exc


@dataclass
class TaskOutcome(Generic[T, R]):
    item: T
    ok: bool
    result: Optional[R] = None
    error: Optional[str] = None


def run_parallel(
    items: Iterable[T],
    worker_fn: Callable[[T], R],
    *,
    max_workers: int = 8,
    log: Optional[Callable[[str], None]] = None,
) -> List[TaskOutcome[T, R]]:
    items = list(items)
    if not items:
        return []

    if log:
        log(f"Submitting {len(items)} tasks with max_workers={max_workers}")

    out: List[TaskOutcome[T, R]] = []
    with ThreadPoolExecutor(max_workers=max_workers) as ex:
        fut_to_item = {ex.submit(worker_fn, it): it for it in items}
        for fut in as_completed(fut_to_item):
            it = fut_to_item[fut]
            try:
                res = fut.result()
                out.append(TaskOutcome(item=it, ok=True, result=res))
            except Exception as e:
                out.append(TaskOutcome(item=it, ok=False, error=f"{type(e).__name__}: {e}"))

    if log:
        ok = sum(o.ok for o in out)
        log(f"Finished: {ok} ok, {len(out) - ok} failed")

    return out