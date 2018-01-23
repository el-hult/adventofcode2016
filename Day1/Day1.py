import time # for nice presentation

class Walker:
    def __init__(self):
        self.dir = 0
        self.long = 0
        self.lat = 0
        self.track = [(0,0)]
        self.bunny_hq = None
        
    def turn(self,char):
        if char == "R":
            change = 1
        else:
            change = -1
        self.dir = (self.dir + change) % 4
        return

    def getDir(self):
        if self.dir == 0:
            return "North"
        elif self.dir == 1:
            return "East"
        elif self.dir == 2:
            return "South"
        elif self.dir == 3:
            return "West"

    def move(self,instruction):
        self.turn(instruction[0])

        dist = int(float(instruction[1:]))

        for _ in range(dist):

            if self.dir == 0:
                self.long += 1
            elif self.dir == 1:
                self.lat += 1
            elif self.dir == 2:
                self.long -= 1
            elif self.dir == 3:
                self.lat -= 1

            newpos = (self.long,self.lat)

            if self.bunny_hq is None:
                if newpos in self.track:
                    self.bunny_hq = newpos
                    print("found HQ!!!!")

            self.track.append(newpos)

    def getPos(self):
        return (self.long,self.lat)
    
    def sayDistanceTo(self,pos):
        print("The distance to %s from the origin is %i"  % (pos, abs(pos[0])+abs(pos[1])))

with open('data.txt', 'r') as myfile:
    data=myfile.read().split(", ")

me = Walker()
for inst in data:
    me.move(inst)

me.sayDistanceTo((me.long,me.lat))
me.sayDistanceTo(me.bunny_hq)