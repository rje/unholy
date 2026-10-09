local time, deaths, checkpoint, won = 0, 0, false, false
local jumpWasDown = false
local function respawn(self)
    self.position = checkpoint and {x=0,y=1.0,z=-5} or {x=0,y=1.0,z=8}
    self.body.velocity = {x=0,y=0,z=0}
    self.rotation = {x=0,y=0,z=0}
end
return {
    start = function(self, dt, input, scene)
        print("Marble Run: reach the gold platform. Space jumps the gap.")
    end,
    update = function(self, dt, input, scene)
        if input.restart then
            time, deaths, checkpoint, won = 0, 0, false, false
            respawn(self)
        end
        if not won then
            time = time + dt
            local v = self.body.velocity
            local x,z = input.x,input.z
            local len = math.sqrt(x*x+z*z)
            if len>1 then x,z=x/len,z/len end
            v.x = v.x + x*19*dt
            v.z = v.z + z*19*dt
            if x==0 then v.x=v.x*math.exp(-3*dt) end
            if z==0 then v.z=v.z*math.exp(-3*dt) end
            local speed=math.sqrt(v.x*v.x+v.z*v.z)
            if speed>5 then v.x,v.z=v.x*5/speed,v.z*5/speed end
            if input.jump and not jumpWasDown and self.body.grounded then v.y=7.5 end
            if self.position.y < -7 then deaths=deaths+1;respawn(self) end
            if self.position.z < -3.5 and self.position.z > -7 and math.abs(self.position.x)<2.8 and self.position.y>0 then
                checkpoint=true
                local flag=scene:get("Checkpoint beacon")
                if flag then flag.material.color={x=0.2,y=1,z=0.55};flag.material.emission=2 end
            end
            if checkpoint and self.position.x>7.8 and math.abs(self.position.z+5)<2.8 and self.position.y>0 then
                won=true
                self.body.velocity={x=0,y=0,z=0}
                print(string.format("Course complete in %.1f seconds!",time))
            end
        else
            self.body.velocity.x,self.body.velocity.z=0,0
        end
        jumpWasDown=input.jump
        local p=self.position
        scene.camera.eye={x=p.x+10,y=16,z=p.z+14}
        scene.camera.target={x=p.x,y=0,z=p.z-2}
        scene.hud = won and string.format("COURSE COMPLETE  /  %.1fs  /  %d falls\nR to race again",time,deaths)
            or string.format("MARBLE RUN  /  %.1fs  /  %d falls\nWASD roll  /  Space jump  /  R restart\n%s",time,deaths,checkpoint and "Checkpoint set. Turn right to the gold island." or "Blue path -> jump the gap -> green checkpoint.")
    end
}
