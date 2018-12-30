const cave = [];

const depth = 11541;
const target = {x: 14, y: 778};
const bounds = {x: target.x + 13, y: target.y + 0}
const types = ["rocky", "wet", "narrow"];

const iRange = (to) => {
  return [...Array(to + 1).keys()];
};

const getCoordinate = (x, y) => {
  if (y < 0 || x < 0) {
    return null;
  }

  if (y > bounds.y || x > bounds.x) {
    return null;
  }

  return cave[y][x]
};

const geoLogicalIndex = (x, y) => {
  if (x == 0 && y == 0) {
    return 0;
  }

  if (target.x == x && target.y == y) {
    return 0;
  }

  if (y == 0) {
    return x * 16807;
  }

  if (x == 0) {
    return y * 48271
  }

  const left = getCoordinate(x - 1, y);
  const up = getCoordinate(x, y - 1);

  return left.erosion * up.erosion;
}

const erosionLevel = (geoIndex) => {
  return (geoIndex + depth) % 20183;
}

const setupCave = (tx, ty) => {
  iRange(ty).forEach(y => {
    cave[y] = [];
    iRange(tx).forEach(x => {
      const geoIndex = geoLogicalIndex(x, y);
      const erosion = erosionLevel(geoIndex);
      const type = types[erosion % 3];

      cave[y][x] = {x, y,  geoIndex, erosion, type};
    })
  })
};

const riskLevel = () => {
  let riskLevel = 0;
  cave.forEach(row => {
    row.forEach(cell => {
      riskLevel += types.indexOf(cell.type);
    });
  });

  return riskLevel;
}

setupCave(target.x, target.y);
console.log("Part 1", riskLevel());

setupCave(bounds.x, bounds.y);

// PART 2

const typeEquipment = {
  "rocky": ["climbing", "torch"],
  "wet": ["climbing", "neither"],
  "narrow": ["torch", "neither"]
}

const createNodes = () => {
  const nodes = [];
  cave.forEach(row => {
    row.forEach(cell => {
      const allowedEq = typeEquipment[cell.type];

      allowedEq.forEach(equipment => {
        nodes.push({x: cell.x, y: cell.y, type: cell.type, equipment, id: `${cell.x},${cell.y}-${equipment}`,
      dist: Infinity, prev: null, // For Dijkstra's algorithm
      fScore: Infinity, gScore: Infinity, closed: false // For A* algorithm
    })
      });
    });
  });

  return nodes;
};

const nodes = createNodes();
console.log("Nodes", nodes.length);

const addMovesTo = node => {
  const {x, y, type} = node;
  const canChangeTo = typeEquipment[type];

  const up = getCoordinate(x, y - 1),
        down = getCoordinate(x, y + 1),
        left = getCoordinate(x - 1, y),
        right = getCoordinate(x + 1, y);

  const moveCells = [up, down, left, right].filter(c => c !== null && c !== undefined);

  const moves = [];

  moveCells.forEach(cell => {
    //console.log(cell);
    const allowedEq = typeEquipment[cell.type].filter(eq => canChangeTo.includes(eq));
    //console.log("a", allowedEq);

    allowedEq.forEach(equipment => {
      const moveNode = nodes.find(n => n.x == cell.x && n.y == cell.y && equipment === n.equipment);
      if (moveNode !== undefined) {
        moves.push([moveNode, equipment === node.equipment ? 1 : 8])
      }
    })
  });

  node.moves = moves;

  return node;
}

function dijkstra(nodes, source) {
  let queue = [...nodes];
  
  console.log(source.id);

  source.dist = 0;

  while (queue.length !== 0) {
    const current = queue.reduce((a, b) => a.dist < b.dist ? a : b);
    const idx = queue.indexOf(current);
    queue.splice(idx, 1);

    const currentCost = current.dist;
    current.moves.forEach(([node, cost]) => {
      const alt = currentCost + cost;

      if (alt < node.dist) {
        node.dist = alt;
        node.prev = current.id;
      }
    });
  }

  return nodes;
}

function manhattanDistance (a, b) {
  return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
}

function aStar(start, goal) {
  start.gScore = 0;
  start.fScore = manhattanDistance(start, goal);

  const open = [start];

  // const cameFrom = {}; <- This would be needed for the full path.
  // In this case only the cost is of interest.

  while (open.length !== 0) {
    const current = open.reduce((a, b) => a.fScore < b.fScore ? a : b);

    if (current === goal) {
      return current.fScore;
    }

    const idx = open.indexOf(current);
    open.splice(idx, 1);
    current.closed = true;

    current.moves.forEach(([neighbor, cost]) => {
      if(neighbor.closed) {
        return;
      }

      tentativeGScore = current.gScore + cost;

      if (open.indexOf(neighbor) == -1) {
        //Discovered a new node
        open.push(neighbor);
      } else if (tentativeGScore >= neighbor.gScore) {
        return;
      }

      // cameFrom[neighbor.id] = current;
      neighbor.gScore = tentativeGScore;
      neighbor.fScore = neighbor.gScore + manhattanDistance(neighbor, goal);
    }); 
  }
}

const nodesWithMoves = []

const nodeCnt = nodes.length;

for(i = 0; i < nodeCnt; i++) {
  nodesWithMoves.push(addMovesTo(nodes[i]));
}

console.time("Dijkstra");
let alteredNodes = dijkstra(nodesWithMoves, nodesWithMoves[1]);
console.timeEnd("Dijkstra");

console.log("Part 2, Dijstra", alteredNodes.find(n => n.id === `${target.x},${target.y}-torch`).dist,
alteredNodes.find(n => n.id === `${target.x},${target.y}-climbing`).dist + 7);

console.time("A*");
const dist1 = aStar(nodesWithMoves[1], nodesWithMoves.find(n => n.id === `${target.x},${target.y}-torch`));

nodesWithMoves.forEach(n => {
  n.closed = false;
  n.gScore = Infinity;
  n.fScore = Infinity;
});

const dist2 = aStar(nodesWithMoves[1], nodesWithMoves.find(n => n.id === `${target.x},${target.y}-climbing`));
console.timeEnd("A*");

console.log("Part 2, A*", dist1, dist2 + 7);