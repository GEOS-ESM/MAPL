import abc


class GEOSInterfaceCode(abc.ABC):
    """Abstract class for user code to derive from"""

    def __init__(self, name: str) -> None:
        self.name = name

    @abc.abstractclassmethod
    def init(grid_comp, import_state, export_state) -> None: ...

    @abc.abstractclassmethod
    def run(grid_comp, import_state, export_state) -> None: ...

    @abc.abstractclassmethod
    def finalize(grid_comp, import_state, export_state) -> None: ...
