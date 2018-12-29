const cave = [];

const depth = 11541;
const target = {x: 14, y: 778};
const types = ["rocky", "wet", "narrow"];

const iRange = (to) => {
  return [...Array(to + 1).keys()];
};

const getCoordinate = (x, y) => cave[y][x];

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

      cave[y][x] = {geoIndex, erosion, type};
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
console.log(riskLevel());