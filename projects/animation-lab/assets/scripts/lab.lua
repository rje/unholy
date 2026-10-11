return {update=function(self,dt,input,scene)
  self.rotation.y=self.rotation.y+dt*22
  scene.hud='ANIMATION LAB  /  Survey - Walk - Run\n22 foxes / independent poses and GPU shadows\nStop, select a fox and scroll to Animator.\nGold tag follows the paused fox head joint.'
end}
