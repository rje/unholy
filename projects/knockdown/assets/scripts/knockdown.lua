local time, won, jumpWasDown = 0, false, false
local initial, toppled = {}, {}
local function reset(self,scene)
    time,won,toppled=0,false,{}
    self.position={x=0,y=0.7,z=7}
    self.body.velocity={x=0,y=0,z=0}
    for i=1,8 do
        local e=scene:get("Target "..i)
        e.position={x=initial[i].x,y=initial[i].y,z=initial[i].z}
        e.rotation={x=0,y=0,z=0}
        e.body.velocity={x=0,y=0,z=0}
        e.material.color={x=0.75,y=0.34,z=0.2}
    end
end
return {
    start=function(self,dt,input,scene)
        for i=1,8 do
            local p=scene:get("Target "..i).position
            initial[i]={x=p.x,y=p.y,z=p.z}
        end
        print("Knockdown: roll the steel ball into all eight orange targets.")
    end,
    update=function(self,dt,input,scene)
        if input.restart then reset(self,scene) end
        if not won then time=time+dt end
        local v=self.body.velocity
        v.x=v.x+input.x*26*dt
        v.z=v.z+input.z*26*dt
        if input.x==0 then v.x=v.x*math.exp(-2*dt) end
        if input.z==0 then v.z=v.z*math.exp(-2*dt) end
        local speed=math.sqrt(v.x*v.x+v.z*v.z)
        if speed>8 then v.x,v.z=v.x*8/speed,v.z*8/speed end
        if input.jump and not jumpWasDown and self.body.grounded then v.y=7 end
        jumpWasDown=input.jump
        local count=0
        for i=1,8 do
            local e=scene:get("Target "..i)
            local p,r=e.position,e.rotation
            if math.abs(r.x)>35 or math.abs(r.z)>35 or p.y<0.55 or math.abs(p.x-initial[i].x)>1.3 or math.abs(p.z-initial[i].z)>1.3 then
                toppled[i]=true
                e.material.color={x=0.2,y=0.75,z=0.52}
            end
            if toppled[i] then count=count+1 end
        end
        if count==8 and not won then won=true;print(string.format("All targets down in %.1fs",time)) end
        if self.position.y < -7 then self.position={x=0,y=0.7,z=7};self.body.velocity={x=0,y=0,z=0} end
        scene.hud=won and string.format("ALL TARGETS DOWN  /  %.1fs\nR to play again",time)
            or string.format("KNOCKDOWN  /  %d of 8 targets  /  %.1fs\nWASD roll  /  Space hop  /  R restart\nRam the orange towers. Green means scored.",count,time)
    end
}
