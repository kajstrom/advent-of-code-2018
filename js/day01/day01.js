const fs = require('fs');

const splitLinesFromFile = (file) => {
  return fs.readFileSync(file).toString().split("\r\n");
}

const parseInt = (n) => {
  return Number.parseInt(n, 10)
}

const part1 = () => {
  const numbers = splitLinesFromFile("input.txt").map(parseInt);

  return numbers.reduce((sum, number) => sum += number);
}

console.time("part1");
console.log(part1());
console.timeEnd("part1");

const part2 = () => {
  const numbers = splitLinesFromFile("input.txt").map(parseInt);
  const frequencies = [0];
  let currentFrequency = 0;
  let idx = 0;
  while (true) {
    idx = idx % numbers.length;
    const number = numbers[idx];
    currentFrequency += number;

    if (frequencies.indexOf(currentFrequency) >= 0) {
      return currentFrequency;
    }

    frequencies.push(currentFrequency);
    idx++;
  }
}

console.time("part2");
console.log(part2());
console.timeEnd("part2");