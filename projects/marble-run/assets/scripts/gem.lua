local time, baseY=0,0
return {
    start=function(self) baseY=self.position.y end,
    update=function(self,dt)
        time=time+dt
        self.rotation.y=time*65
        self.position.y=baseY+math.sin(time*2.6)*.12
    end,
    on_trigger=function(self,event,scene)
        if event.kind=='enter' and event.other and event.other.name=='Marble' then
            scene:sound('assets/audio/checkpoint.wav',.22,1.5)
            scene:destroy(self)
        end
    end
}
