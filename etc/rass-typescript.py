from rassumfrassum.presets.tslint import TypeScriptLogic


class TypeScriptCommandsLogic(TypeScriptLogic):
    # rass 0.3.4 looks for executeCommandProvider beside capabilities, not in them
    async def on_server_response(self, method, request_params, payload, is_error, server):
        await super().on_server_response(method, request_params, payload, is_error, server)
        if method == 'initialize' and isinstance(payload, dict) and not is_error:
            provider = (payload.get('capabilities') or {}).get('executeCommandProvider') or {}
            for command in provider.get('commands') or []:
                self.commands_map.setdefault(command, server)


def logic_class():
    return TypeScriptCommandsLogic
