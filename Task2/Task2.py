
class State:
    def __str__(self):
        return self.__class__.__name__

    def run(self):
        print(self.__str__())

    def next(self, input):
        assert 0, "next not implemented"

class StateMachine:
    def __init__(self, initialState):
        self.currentState = initialState
        #self.currentState.run()
    
    def runAll(self, inputs):
        for i in inputs:
            self.currentState = self.currentState.next(i)
            #self.currentState.run()

class One(State):
    def next(self,input): 
        if input == "R":
            return Two()
        elif input == "D":
            return Four()
        else:
            return self

class Two(State):
    def next(self,input): 
        if input == "R":
            return Three()
        elif input == "D":
            return Five()
        elif input =="L":
            return One()
        else:
            return self

class Three(State):
    def next(self,input): 
        if input == "L":
            return Two()
        elif input == "D":
            return Six()
        else:
            return self

class Four(State):
    def next(self,input): 
        if input == "R":
            return Five()
        elif input == "U":
            return One()
        elif input =="D":
            return Seven()
        else:
            return self

class Five(State):
    def next(self,input): 
        if input == "R":
            return Six()
        elif input == "L":
            return Four()
        elif input == "D":
            return Eight()
        elif input == "U":
            return Two()
        else:
            return self

class Six(State):
    def next(self,input): 
        if input == "L":
            return Five()
        elif input == "U":
            return Three()
        elif input == "D":
            return Nine()
        else:
            return self

class Seven(State):
    def next(self,input): 
        if input == "R":
            return Eight()
        elif input == "U":
            return Four()
        else:
            return self

class Eight(State):
    def next(self,input): 
        if input == "R":
            return Nine()
        elif input == "L":
            return Seven()
        elif input == "U":
            return Five()
        else:
            return self

class Nine(State):
    def next(self,input): 
        if input == "U":
            return Six()
        elif input == "L":
            return Eight()
        else:
            return self

with open('data.txt', 'r') as myfile:
    data=myfile.readlines()

lock = StateMachine(Five())        
print("Task 1")
for l in data:
    lock.runAll(l)
    print(lock.currentState)


print("Task 2")


class Keypad2:
    def __init__(self):
        self.long = 0
        self.lat = 2
        self.map = [[".",".","1",".","."],
                    [".","2","3","4","."],
                    ["5","6","7","8","9"],
                    [".","A","B","C","."],
                    [".",".","D",".","."]]

    def get_state(self):
        return self.map[self.lat][self.long]

    def move(self,dir):
        if dir=="U":
            newlong = self.long 
            newlat = self.lat -1
        elif dir =="D":
            newlong = self.long 
            newlat = self.lat+1
        elif dir =="R":
            newlong = self.long+1
            newlat = self.lat
        elif dir =="L":
            newlong = self.long -1
            newlat = self.lat
        else:
            print("Strange input found:%s" % dir)
            return

        if newlong == -1 or newlat == -1 or newlong > 4 or newlat > 4:
            return

        if self.map[newlat][newlong] == ".":
            return

        self.long = newlong
        self.lat = newlat

    def moveAll(self,dirs):
        for dir in dirs:
            self.move(dir)

k2 = Keypad2()  
for l in data:
    k2.moveAll(l.strip())
    print(k2.get_state())

