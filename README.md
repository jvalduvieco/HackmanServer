HackmanServer
---

A server for HackmanJS

Usage
---
    {ok, Match} = hs_match_manager_service:get_match_handle(<<"0">>).
    hs_match_referee:start_match(Match).