const fs = require('fs')
const input = fs.readFileSync('./src/Day5/test-input.txt', { encoding: 'utf8' });
const lines = input.split("\n");

const INDEX_ROW = 3

const stackDefinitions = lines.slice(0,INDEX_ROW)

console.log('stackDefinitions', stackDefinitions)

const totalStacks = (lines[INDEX_ROW].split(' ').map(Number).filter(n => n > 0).sort().reverse())[0]
console.log('totalStacks', totalStacks)
const stacks = 
    Array(totalStacks)
        .fill([])
        .map((_, i) => {
            return stackDefinitions
                .map(sd => sd.charAt((i + 1) * 4 - 3))
                .filter(c => c != ' ')
                .reverse()
        })

console.log('stacks', stacks)

const instructions = 
    lines
        .slice(INDEX_ROW+2, -1)
        //.flatMap(l => {
        //    const parts = l.match(/move (\d+) from (\d+) to (\d+)/)
        //    return Array(Number(parts[1])).fill({}).map(_ => ({
        //        src: parts[2] - 1,
        //        dst: parts[3] - 1
        //    }))
        //})
        .map(l => {
            const parts = l.match(/move (\d+) from (\d+) to (\d+)/)
            return {
                count: parts[1],
                src: parts[2] - 1,
                dst: parts[3] - 1
            }
        })

console.log('total instructions', instructions.length)

function applyInstructions(instructions, stacks) {
    if (instructions.length == 0) {
        console.log('No more instructions', stacks)
        return stacks
    }

    const updatedStacks = applyInstruction(stacks)(instructions.shift())
    return applyInstructions(instructions, updatedStacks)
}

function applyInstruction(stacks) {
    return function(instruction) {
        //console.log('Applying', instruction)
        const crate = stacks[instruction.src].splice(0 - instruction.count);
        console.log('Moving', crate, 'from', instruction.src, 'to', instruction.dst)
        stacks[instruction.dst].push(crate)
        return stacks
    }
}

const reordered = applyInstructions(instructions, stacks)
console.log('reordered', reordered)
const top = reordered.map(s => s[s.length - 1])

console.log('top', top.join(''))
