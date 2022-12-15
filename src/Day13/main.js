
const fs = require('fs');

const zip = (a, b) => Array.from(Array(Math.max(b.length, a.length)), (_, i) => [a[i], b[i]]);

const part1 = 
    fs
        .readFileSync("./src/Day13/input.txt", 'utf8')
        .split(/\n/)
        .reduce((acc,l) => {
            if (l == '') {
                acc.push([])
            } else {
                acc[acc.length - 1].push(l)
            }

            return acc
        }, [[]])
    .map(as => as.map(JSON.parse))
    .filter(p => p.length > 0)
    .map(isOrderCorrect)
    .map((b, i) => b == true ? i + 1 : 0)
    .reduce((acc, i) => acc + i, 0)

console.log('part1', part1)

const part2 = 
    fs
        .readFileSync("./src/Day13/test-input.txt", 'utf8')
        .split(/\n/)
        .filter(l => l != '')
        .map(JSON.parse)
        .concat([[[2]]])
        .concat([[[6]]])
        .sort()

console.log(part2)
const d1 = part2.findIndex(m => m.length == 1 && m[0].length == 1 && m[0][0] == 2) + 1
const d2 = part2.findIndex(m => m.length == 1 && m[0].length == 1 && m[0][0] == 6) + 1
console.log('part2', d1, d2, d1*d2);

function isOrderCorrect(pair) {
    console.log('isOrderCorrect', pair)
    let a = pair[0]
    let b = pair[1]

    const isCorrect = compare(a, b);

    console.log('isCorrect', isCorrect);
    return isCorrect;

    //if (a.length == 0 && b.length > 0) { return true }
    //if (a.length > 0 && b.length == 0) { return false }

    //if (a.length > 0 && a[0] 
}

function compare(l, r) {
    console.log('compare', 'l', l, 'r', r);

    if (l == undefined && r !== undefined) {
        return true;
    }

    if (l !== undefined && r == undefined) {
        return false;
    }

    if (Number.isInteger(l) && Number.isInteger(r)) {
        if (l !== r) {
            return l < r
        }
    }

    if (Array.isArray(l) && Array.isArray(r)) {
        console.log('arrays', l, r);
        if (l.length == 0 && r.length > 0) { return true }
        if (l.length > 0 && r.length == 0) { return false }
        //if (l.length == 0 && r.length == 0) { return false }

        var answer = undefined;
        while (answer == undefined && (l.length > 0 || r.length > 0)) {
            answer = compare(l.shift(), r.shift());
        }

        return answer;
    }

    if (Number.isInteger(l) && Array.isArray(r)) {
        return compare([l], r);
    }

    if (Array.isArray(l) && Number.isInteger(r)) {
        return compare(l, [r]);
    }

    return undefined
}


//
// 32625
