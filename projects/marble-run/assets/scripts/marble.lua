local time, deaths, checkpoint, won = 0, 0, false, false
local jumpWasDown = false
local clock, wasGrounded, previousYVelocity = 0, false, 0
local assemblies = {}
local function rememberAssembly(scene, prefix, anchor)
    local parts = {}
    for name,part in pairs(scene.by_name) do
        if name:sub(1,#prefix)==prefix then
            parts[#parts+1]={name=name,x=part.position.x-anchor.x,y=part.position.y-anchor.y,z=part.position.z-anchor.z}
        end
    end
    assemblies[prefix]=parts
end
local function moveAssembly(scene, prefix, anchor)
    for _,offset in ipairs(assemblies[prefix]) do
        local part=scene:get(offset.name)
        part.position={x=anchor.x+offset.x,y=anchor.y+offset.y,z=anchor.z+offset.z}
    end
end
local function animate(scene, dt)
    clock=clock+dt
    local shuttle=scene:get("Turn right")
    shuttle.position.z=-5+math.sin(clock*.85)*1.0
    moveAssembly(scene,"Shuttle / ",shuttle.position)
    local lift=scene:get("Sky lift")
    lift.position.y=-.5+1.6*(1-math.cos(clock*.7))
    moveAssembly(scene,"Lift / ",lift.position)
    scene:get("Balcony sweeper").rotation.y=clock*50
    local ring=scene:get("Finish ring")
    ring.rotation.y=math.sin(clock*.6)*14
    ring.position.y=1.4+math.sin(clock*1.8)*.12
end
local function respawn(self)
    self.position = checkpoint and {x=0,y=1.0,z=-5} or {x=0,y=1.0,z=8}
    self.body.velocity = {x=0,y=0,z=0}
    self.rotation = {x=0,y=0,z=0}
end
return {
    start = function(self, dt, input, scene)
        rememberAssembly(scene,"Shuttle / ",{x=5,y=-.5,z=-5})
        rememberAssembly(scene,"Lift / ",{x=-6,y=-.5,z=8})
        scene:sound("assets/audio/start.wav",.4)
        print("Aether Gardens: jump the gap, cross the sliding bridge, reach the gold arch. The left lift leads to a bonus balcony. M mutes sound.")
    end,
    update = function(self, dt, input, scene)
        animate(scene,dt)
        scene:loop("assets/audio/wind.wav",.10)
        scene:loop("assets/audio/ambience.wav",.25)
        if input.restart then
            time, deaths, checkpoint, won = 0, 0, false, false
            local flag=scene:get("Checkpoint beacon")
            flag.material.color={x=.25,y=.71,z=.68};flag.material.emission=.5
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
            if input.jump and not jumpWasDown and self.body.grounded then
                v.y=7.5;scene:sound("assets/audio/jump.wav",.35)
            end
            if self.body.grounded and not wasGrounded and previousYVelocity<-1.5 then
                scene:sound("assets/audio/land.wav",math.min(.6,.12-previousYVelocity*.035))
            end
            scene:loop("assets/audio/roll.wav",self.body.grounded and math.min(.24,speed*.045) or 0,.7+speed*.14)
            if self.position.y < -7 then deaths=deaths+1;scene:sound("assets/audio/fall.wav",.4);respawn(self) end
            if self.position.z < -3.5 and self.position.z > -7 and math.abs(self.position.x)<2.8 and self.position.y>0 then
                if not checkpoint then scene:sound("assets/audio/checkpoint.wav",.5) end
                checkpoint=true
                local flag=scene:get("Checkpoint beacon")
                if flag then flag.material.color={x=0.2,y=1,z=0.55};flag.material.emission=2 end
            end
            if checkpoint and self.position.x>7.8 and math.abs(self.position.z+5)<2.8 and self.position.y>0 then
                won=true
                self.body.velocity={x=0,y=0,z=0}
                scene:sound("assets/audio/finish.wav",.6)
                scene:loop("assets/audio/roll.wav",0)
                print(string.format("Course complete in %.1f seconds!",time))
            end
        else
            self.body.velocity.x,self.body.velocity.z=0,0
        end
        jumpWasDown=input.jump
        wasGrounded=self.body.grounded;previousYVelocity=self.body.velocity.y
        local p=self.position
        scene.camera.eye={x=p.x+10,y=12.5,z=p.z+14}
        scene.camera.target={x=p.x,y=0,z=p.z-2}
        scene.hud = won and string.format("COURSE COMPLETE  /  %.1fs  /  %d falls\nR to race again",time,deaths)
            or string.format("AETHER GARDENS  /  %.1fs  /  %d falls\nWASD roll  /  Space jump  /  R restart  /  M mute\n%s",time,deaths,checkpoint and "Checkpoint set. Cross the sliding bridge to the gold arch." or "Follow the blue lights. Jump the gap. Left lift: bonus balcony.")
    end
}
