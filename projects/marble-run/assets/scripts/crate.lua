local age, cooldown=0,0
return {
    update=function(self,dt,_,scene)
        age=age+dt;cooldown=math.max(0,cooldown-dt)
        if self.position.y < -12 or age>20 then scene:destroy(self) end
    end,
    on_collision=function(self,event,scene)
        if event.kind=='enter' and event.impulse>1 and cooldown==0 then
            scene:sound('assets/audio/land.wav',math.min(.5,event.impulse*.035),.7)
            cooldown=.3
        end
    end
}
