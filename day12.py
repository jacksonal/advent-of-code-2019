from functools import reduce
from itertools import combinations


class Body(object):
    def __init__(self, pos):
        self.position = pos
        self.velocity = [0, 0, 0]
    def UpdateVelocity(self, inc, index):
        self.velocity[index] += inc
    def UpdatePosition(self):
        self.position[0] += self.velocity[0]
        self.position[1] += self.velocity[1]
        self.position[2] += self.velocity[2]
    def GetEnergy(self):
        return reduce(lambda x,y: abs(x)+abs(y),self.position) * reduce(lambda x,y: abs(x)+abs(y),self.velocity)

def updateVelocity(pair):
    bodyA, bodyB = pair
    for dim in range(3):
        if bodyA.position[dim] < bodyB.position[dim]:
            bodyA.UpdateVelocity(1, dim)
            bodyB.UpdateVelocity(-1, dim)
        elif bodyA.position[dim] > bodyB.position[dim]:
            bodyA.UpdateVelocity(-1, dim)
            bodyB.UpdateVelocity(1, dim)

sampleBodies = (
    Body([-1, 0, 2]),
    Body([2, -10, -7]),
    Body([4, -8, 8]),
    Body([3, 5, -1])
)

def step(bodies):
    for pair in combinations(bodies, 2):
        updateVelocity(pair)
    for body in bodies:
        body.UpdatePosition()

def simulate(numSteps, bodies):
    [step(bodies) for s in range(numSteps)]