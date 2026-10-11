local time, deaths, checkpoint, won = 0, 0, false, false
local jumpWasDown = false
local clock, wasGrounded, previousYVelocity = 0, false, 0
local props, gems, crateCooldown = {}, 0, 0
local restartWasDown, spawnWasDown = false, false
local function resetProps(scene)
    for _,id in ipairs(props) do scene:destroy(id) end
    props, gems = {}, 0
    for _,p in ipairs({{x=0,y=.8,z=6},{x=0,y=.8,z=-1.7},{x=0,y=.8,z=-5},{x=9,y=.8,z=-5},{x=-9.1,y=4,z=8}}) do
        local gem=scene:spawn('assets/prefabs/aether-gem.prefab.json',p)
        props[#props+1]=gem.id
    end
end
local function animate(scene, dt)
    clock=clock+dt
    local shuttle=scene:get("Turn right")
    shuttle.position.z=-5+math.sin(clock*.85)*1.0
    local lift=scene:get("Sky lift")
    lift.position.y=-.5+1.6*(1-math.cos(clock*.7))
    scene:get("Balcony sweeper").rotation.y=clock*50
    local ring=scene:get("Finish ring")
    ring.rotation.y=math.sin(clock*.6)*14
    ring.position.y=1.4+math.sin(clock*1.8)*.12
end
local function respawn(self,scene)
    scene:teleport(self, checkpoint and {x=0,y=1.0,z=-5} or {x=0,y=1.0,z=8})
    self.rotation = {x=0,y=0,z=0}
end
return {
    start = function(self, dt, input, scene)
        resetProps(scene)
        scene:sound("assets/audio/start.wav",.4)
        print("Aether Gardens: jump the gap, cross the sliding bridge, reach the gold arch. The left lift leads to a bonus balcony. M mutes sound.")
    end,
    update = function(self, dt, input, scene)
        animate(scene,dt)
        scene:loop("assets/audio/wind.wav",.10)
        scene:loop("assets/audio/ambience.wav",.25)
        crateCooldown=math.max(0,crateCooldown-dt)
        if input.spawn and not spawnWasDown and crateCooldown==0 then
            local p=self.position
            local hit=scene:raycast({x=p.x,y=p.y+1,z=p.z},{x=p.x,y=p.y+1,z=p.z-3},self)
            local z=hit and hit.point.z+1 or p.z-2
            local crate=scene:spawn("assets/prefabs/physics-crate.prefab.json",{x=p.x,y=p.y+2,z=z})
            props[#props+1]=crate.id;crateCooldown=.35
        end
        spawnWasDown=input.spawn
        if input.restart and not restartWasDown then
            resetProps(scene)
            time, deaths, checkpoint, won = 0, 0, false, false
            local flag=scene:get("Checkpoint beacon")
            flag.material.color={x=.25,y=.71,z=.68};flag.material.emission=.5
            respawn(self,scene)
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
            if self.position.y < -7 then deaths=deaths+1;scene:sound("assets/audio/fall.wav",.4);respawn(self,scene) end

        else
            self.body.velocity.x,self.body.velocity.z=0,0
        end
        restartWasDown=input.restart
        jumpWasDown=input.jump
        wasGrounded=self.body.grounded;previousYVelocity=self.body.velocity.y
        local p=self.position
        scene.camera.eye={x=p.x+10,y=12.5,z=p.z+14}
        scene.camera.target={x=p.x,y=0,z=p.z-2}
        scene.hud = won and string.format("COURSE COMPLETE  /  %.1fs  /  %d falls\n%d / 5 gems  /  R to race again",time,deaths,gems)
            or string.format("AETHER GARDENS  /  %.1fs  /  %d falls\nWASD roll / Space jump / E crate / R restart / M mute\n%d / 5 gems  /  %s",time,deaths,gems,checkpoint and "Checkpoint set. Cross the sliding bridge to the gold arch." or "Follow the blue lights. Jump the gap. Left lift: bonus balcony.")
    end,
    on_trigger = function(self,event,scene)
        if event.kind~='enter' or not event.other then return end
        local name=event.other.name
        if name=='Aether gem' then gems=gems+1 end
        if name=='Checkpoint trigger' and not checkpoint then
            checkpoint=true
            scene:sound('assets/audio/checkpoint.wav',.5)
            local flag=scene:get('Checkpoint beacon')
            flag.material.color={x=.2,y=1,z=.55};flag.material.emission=2
        end
        if name=='Finish trigger' and checkpoint and not won then
            won=true;self.body.velocity={x=0,y=0,z=0}
            scene:sound('assets/audio/finish.wav',.6);scene:loop('assets/audio/roll.wav',0)
            print(string.format('Course complete in %.1f seconds!',time))
        end
    end
}
