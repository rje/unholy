#include <lua.h>
#include <lauxlib.h>
// luaL_error must unwind inside Lua's protected call, never a Haskell callback.
static void budget(lua_State *L,lua_Debug *ar) {
    (void)ar;luaL_error(L,"Script exceeded its per-call instruction budget (200000)");
}
void hj_lua_budget(lua_State *L,int enabled) {lua_sethook(L,enabled?budget:NULL,enabled?LUA_MASKCOUNT:0,200000);}
