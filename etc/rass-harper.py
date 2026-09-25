from rassumfrassum.frassum import DirectResponse, LspLogic


def _is_harper(server):
    return 'harper' in (server.name or '').lower()


class HarperBesideLogic:
    # rass 0.3.4 routes only commands it saw in a code action; the rest go to the language server
    async def on_client_request(self, method, params, servers):
        targets = await super().on_client_request(method, params, servers)
        if method == 'workspace/executeCommand' and targets == [] and servers:
            return [servers[0]]
        return targets

    async def on_server_request(self, method, params, source):
        if _is_harper(source):
            if method == 'client/registerCapability':
                return DirectResponse(payload=None)
            if method == 'workspace/configuration':
                return DirectResponse(payload=[{} for _ in params.get('items') or []])
        return await super().on_server_request(method, params, source)

    async def on_server_response(self, method, request_params, payload, is_error, server):
        await super().on_server_response(method, request_params, payload, is_error, server)
        if method == 'initialize' and isinstance(payload, dict) and not is_error:
            provider = (payload.get('capabilities') or {}).get('executeCommandProvider') or {}
            for command in provider.get('commands') or []:
                self.commands_map.setdefault(command, server)


class PrimaryCommandsLogic(HarperBesideLogic, LspLogic):
    pass


def logic_class():
    return PrimaryCommandsLogic
