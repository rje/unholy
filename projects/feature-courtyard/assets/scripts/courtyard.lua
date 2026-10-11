local time = 0
return {
    update = function(self, dt, input, scene)
        time = time + dt
        self.rotation.y = time * 24
        scene.hud = "FEATURE COURTYARD\nIdle / two independent walks / paused jump\nHDR roughness row / 128 shared columns / editable terrain\nStop to inspect; RMB orbit, MMB pan, wheel zoom."
    end
}
