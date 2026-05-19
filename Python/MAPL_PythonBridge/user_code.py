import abc


class UserCode(abc.ABC):
    """Abstract class for user code to derive from"""

    def __init__(self, name: str) -> None:
        self.name = name

    @abc.abstractmethod
    def init(self, mapl_state, import_state, export_state) -> None: ...

    @abc.abstractmethod
    def run(self, mapl_state, import_state, export_state) -> None: ...

    @abc.abstractmethod
    def run_with_internal(self, mapl_state, import_state, export_state, internal_state) -> None: ...

    @abc.abstractmethod
    def finalize(self, mapl_state, import_state, export_state) -> None: ...
