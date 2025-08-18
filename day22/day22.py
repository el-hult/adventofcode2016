import dataclasses
from collections import defaultdict


def parse_data(path="inputs/day22.txt"):
    with open(path, "r") as f:
        data = f.read()

    nodes = []
    for line in data.splitlines()[2:]:
        addr = line.split(" ")[0]
        xpart = int(addr.split("-")[1][1:])
        ypart = int(addr.split("-")[2][1:])
        uses = line[30:33]
        size = line[24:27]
        nodes.append((xpart, ypart, int(uses), int(size)))

    # x is left-to-right, y is top-to-bottom
    # so (0,0) is the top-left corner
    # and (xmax, 0) is the target coord, i.e. top right corner
    xmax = max(x for x, _, _, _ in nodes)
    ymax = max(y for _, y, _, _ in nodes)
    target_coord = (xmax, 0)

    nrows = ymax + 1
    ncols = xmax + 1


    # Now we have the nodes in a list, and we can create a grid of sizes and uses
    sizes = [[None] * nrows for _ in range(ncols)]
    uses = [[None] * nrows for _ in range(ncols)]
    for x, y, used, size in nodes:
        sizes[x][y] = size
        uses[x][y] = used

    return sizes, uses, target_coord, nrows, ncols


def part_one():
  sizes, uses, target_coord, nrows, ncols = parse_data()

  def pair_is_viable(xyA,xyB):
      if uses[xyA[0]][xyA[1]] == 0:
          return False # A is empty
      if xyA == xyB:
          return False # not the same node
      if uses[xyA[0]][xyA[1]] + uses[xyB[0]][xyB[1]] > sizes[xyB[0]][xyB[1]]:
          return False # data wont fit in B
      return True


  print("Part one: ")
  print(sum(1 for i in range(ncols) for j in range(nrows) for k in range(ncols) for m in range(nrows) if pair_is_viable((i, j), (k,m))))



@dataclasses.dataclass(frozen=True)
class State:
  hpos: tuple[int, int]  # (x, y) position of the hole
  gpos: tuple[int, int]  # (x, y) position of the goal data

def heuristic(state: State) -> int:
  """one must go taxicab distance with the hole to the goal, and then 5 times the taxicab distance from goal to the (0,0) point"""
  d1 = abs(state.hpos[0] - state.gpos[0]) + abs(state.hpos[1] - state.gpos[1])
  d2 = abs(state.gpos[0] - 0) + abs(state.gpos[1] - 0)
  return d1 + 5 * d2

def reconstruct_path(came_from: dict[State, State], current: State):
  path = []
  while current in came_from:
    path.append(current)
    current = came_from[current]
  path.reverse()
  return path

def astar(start,h, ncols,nrows, walls):
  open_set = {start}
  came_from = dict()
  gScore = defaultdict(lambda: float('inf'))
  gScore[start] = 0
  fScore = defaultdict(lambda: float('inf'))
  fScore[start] = h(start)
  return astar1(h, ncols, nrows, walls, open_set, came_from, gScore, fScore)


def getNeighborStates(current: State, ncols: int, nrows: int, walls: set[tuple[int, int]]) -> list[State]:
  """Generate all possible states by moving the hole to an adjacent position.
  Can be written as a comprehension, which is more haskelly"""
  neighbour_states = []
  for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]: 
    new_hpos = (current.hpos[0] + dx, current.hpos[1] + dy)
    if new_hpos[0] < 0 or new_hpos[0] >= ncols or new_hpos[1] < 0 or new_hpos[1] >= nrows:
      continue # hole would go out of bounds
    if new_hpos in walls:
      continue # the hole would move into a wall (or rather, this means that the data that would be moved into the hole is too large)
    new_gpos = current.gpos
    if new_hpos == current.gpos:  # if the hole moves to the goal
      new_gpos = current.hpos  # the goal data moves to the hole's old position
    neighbour_states.append(State(new_hpos, new_gpos))
  return neighbour_states

def astar1(h: callable, ncols: int, nrows: int, walls: set[tuple[int, int]], open_set: set[State], came_from: dict[State, State], gScore: dict[State, int], fScore: dict[State, int]):
  if not open_set:
    return "No path found"
  
  current = min(open_set, key=lambda x: fScore[x])
  if current.gpos == (0, 0): # reached the goal
    return reconstruct_path(came_from, current)

  open_set.remove(current)

  # Generate neighbors by considering where to move the hole
  neighbour_states = getNeighborStates(current, ncols, nrows, walls)

  # TODO decide on how to rewrite this in a more haskelly way
  # the issue is that we need to update state in a loop. :(
  # I feel this might be an appropriate situation for a state monad, since explicit state passing may be too annoying
  # explicit state passing would in this case mean passing open_set, came_from, gScore, fScore around as input/output in a function
  # and then fold neighbours with  this function.
  for neighbour in neighbour_states:
    tentative_gScore = gScore[current] + 1
    if tentative_gScore < gScore[neighbour]:
      came_from[neighbour] = current
      gScore[neighbour] = tentative_gScore
      fScore[neighbour] = tentative_gScore + h(neighbour)
      if neighbour not in open_set:
        open_set.add(neighbour)


  return astar1(h, ncols, nrows, walls, open_set, came_from, gScore, fScore)


part_one()
def part_two():
  sizes, uses, target_coord, nrows, ncols = parse_data()
  
  hole_poss = [(x, y) for x in range(ncols) for y in range(nrows) if uses[x][y] == 0]
  assert len(hole_poss) == 1, "There should be exactly one hole"
  hole_pos = hole_poss[0]
  # some nodes are 'walls'. 
  walls = set( (x, y) for x in range(ncols) for y in range(nrows) if uses[x][y] >= 100 )
  # all other nodes are 'normal' nodes, i.e. they have a utilization that can go anywhere, but no two nodes can be combined.
  min_size_except_walls = min(sizes[x][y] for x in range(ncols) for y in range(nrows) if (x, y) not in walls)
  max_util_except_walls = max(uses[x][y] for x in range(ncols) for y in range(nrows) if (x, y) not in walls)
  min_util_except_hole = min(uses[x][y] for x in range(ncols) for y in range(nrows) if (x, y) != hole_pos)
  max_size_except_walls = max(sizes[x][y] for x in range(ncols) for y in range(nrows) if (x, y) not in walls)
  assert min_size_except_walls > max_util_except_walls, "There should be at least one node that can hold all the data of the hole"
  assert 2*min_util_except_hole > max_size_except_walls, "There should be at least one node that can hold all the data of the hole"
  for x, y in walls:
    # this data don't fit into any non-wall-node
    assert uses[x][y] > max_util_except_walls, "Wall nodes have more data than normal nodes can hold"
    # and the space they have free is smaller than any other nodes utilization
    assert sizes[x][y] - uses[x][y] < min_util_except_hole, "Wall nodes have less free space than the minimum utilization of non-wall nodes"

  # We have proven that:
  # 1) one node is a hole
  # 2) no two nodes can be combined, i.e. data can only be moved into the hole
  # 3) the wall nodes can never be moved

  state0 = State(hpos=hole_pos, gpos=target_coord)
  print("Part two: ")
  print(len(astar(state0, heuristic, ncols, nrows, walls)))

part_two()