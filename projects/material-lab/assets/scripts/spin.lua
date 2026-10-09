-- Modules are instantiated per scripted entity, so closure state is private.
local time = 0
return {
    update = function(self, dt, input, scene)
        time = time + dt
        self.rotation.y = time * 28
        self.position.y = 1.55 + math.sin(time * 1.4) * 0.18
        scene.hud = "Material lab\nGGX metal / roughness, point lights, ACES tone mapping\nStop play to edit materials in the inspector."
    end
}
