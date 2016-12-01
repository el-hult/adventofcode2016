input = "L4, L1, R4, R1, R1, L3, R5, L5, L2, L3, R2, R1, L4, R5, R4, L2, R1, R3, L5, R1, L3, L2, R5, L4, L5, R1, R2, L1, R5, L3, R2, R2, L1, R5, R2, L1, L1, R2, L1, R1, L2, L2, R4, R3, R2, L3, L188, L3, R2, R54, R1, R1, L2, L4, L3, L2, R3, L1, L1, R3, R5, L1, R5, L1, L1, R2, R4, R4, L5, L4, L1, R2, R4, R5, L2, L3, R5, L5, R1, R5, L2, R4, L2, L1, R4, R3, R4, L4, R3, L4, R78, R2, L3, R188, R2, R3, L2, R2, R3, R1, R5, R1, L1, L1, R4, R2, R1, R5, L1, R4, L4, R2, R5, L2, L5, R4, L3, L2, R1, R1, L5, L4, R1, L5, L1, L5, L1, L4, L3, L5, R4, R5, R2, L5, R5, R5, R4, R2, L1, L2, R3, R5, R5, R5, L2, L1, R4, R3, R1, L4, L2, L3, R2, L3, L5, L2, L2, L1, L2, R5, L2, L2, L3, L1, R1, L4, R2, L4, R3, R5, R3, R4, R1, R5, L3, L5, L5, L3, L2, L1, R3, L4, R3, R2, L1, R3, R1, L2, R4, L3, L3, L3, L1, L2"

class Walker:
    def __init__(self):
        self.dir = 0
        self.long = 0
        self.lat = 0
        
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

        dist = int(float(instruction[1]))

        if self.dir == 0:
            self.long += dist
        elif self.dir == 1:
            self.lat += dist
        elif self.dir == 2:
            self.long -= dist
        elif self.dir == 3:
            self.lat -= dist

    def getPos(self):
        return (self.long,self.lat)
    
    def getDistFromOrigin(self):
        return self.long+self.lat

me = Walker()
for inst in input.split(", "):
    me.move(inst)
    print("Lets now move %s!" % inst)
    print("That makes me face %s and i stand in %s" % (me.getDir(), me.getPos() ))
    input("Press Enter to continue...")
    
print("I am now %i units away from where i started" % me.getDistFromOrigin())