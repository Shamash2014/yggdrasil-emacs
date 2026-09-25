import importlib.util
from pathlib import Path

from rassumfrassum.presets.tslint import TypeScriptLogic

_spec = importlib.util.spec_from_file_location('rass_harper', Path(__file__).with_name('rass-harper.py'))
_harper = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_harper)


class TypeScriptCommandsLogic(_harper.HarperBesideLogic, TypeScriptLogic):
    pass


def logic_class():
    return TypeScriptCommandsLogic
