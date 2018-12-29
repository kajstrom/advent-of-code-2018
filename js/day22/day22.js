const cave = [];

const depth = 510;
const target = {x: 10, y: 10};
const bounds = {x: target.x + 10, y: target.y + 10}
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

setupCave(bounds.x, bounds.y);
console.log(riskLevel());

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
        nodes.push({x: cell.x, y: cell.y, type: cell.type, equipment, id: `${cell.x},${cell.y}-${equipment}`})
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

function smallestKey(obj, allowed) {
  return Object.keys(obj)
    .filter(a => allowed.includes(a))
    .reduce((a, b) => obj[a] < obj[b] ? a : b);
}

function dijk(nodes, source) {
  let queue = [...nodes];
  
  console.log(source.id);

  const dist = {};
  const prev = {};

  nodes.forEach(node => {
    dist[node.id] = Infinity;
    prev[node.id] = null;
  });

  dist[source.id] = 0;

  while (queue.length !== 0) {
    const queueKeys = queue.map(q => q.id);
    console.time("smallestKey");
    const nextId = smallestKey(dist, queueKeys);
    console.timeEnd("smallestKey");

    console.time("splice");
    const idx = queue.findIndex(q => q.id === nextId); // Find next item
    const current = queue[idx];
    //queue = queue.filter(q => q.id !== nextId); //Remove from q
    queue.splice(idx, 1);
    //console.log(queue.length);
    console.timeEnd("splice");

    if (queue.length % 100 === 0) {
      console.log("Remaining", queue.length);
    }

    const currentCost = dist[current.id];
    current.moves.forEach(([node, cost]) => {
      const alt = currentCost + cost;

      if (alt < dist[node.id]) {
        dist[node.id] = alt;
        prev[node.id] = current.id;
      }
    });
  }

  return [dist, prev];
}

const nodesWithMoves = nodes.map(addMovesTo);

//console.log(nodesWithMoves.find(n => n.id === "9,10-neither").moves)

const [dists, prevs] = dijk(nodesWithMoves, nodesWithMoves[1])

console.log(dists[`${target.x},${target.y}-torch`], dists[`${target.x},${target.y}-climbing`] + 7);